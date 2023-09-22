{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Local representation for display purpose of cardano-ledger events.
--
-- Shamelessly stolen from db-sync.
module Cardano.Node.LedgerEvent (
    ConvertLedgerEvent (..)
  , EventsConstraints
  , LedgerEvent (..)
  , AnchoredEvent (..)
  , fromAuxLedgerEvent
  , convertPoolRewards
  , ledgerEventName
  , eventCodecVersion
  , deserializeEvent
  , serializeEvent
  , tailEvent
  ) where

import           Cardano.Prelude hiding (All, Sum)

import           Cardano.Ledger.Binary (DecCBOR(..), EncCBOR(..), Version, fromCBOR,
                   serialize', toCBOR, toPlainDecoder)
import           Cardano.Ledger.Binary.Coders (Decode(..), Encode (..), encode, (!>),
                   (<!), decode)
import           Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import qualified Cardano.Ledger.Core as Ledger
import           Cardano.Ledger.Core (eraProtVerLow)
import qualified Cardano.Ledger.Credential as Ledger
import           Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import           Cardano.Ledger.Keys (KeyRole (StakePool))
import           Cardano.Ledger.Shelley.API (InstantaneousRewards (..), KeyHash)
import           Cardano.Ledger.Shelley.Core (EraCrypto)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import           Cardano.Ledger.Shelley.Rules (RupdEvent (..),
                     ShelleyEpochEvent (..), ShelleyMirEvent (..),
                     ShelleyNewEpochEvent, ShelleyPoolreapEvent (..),
                     ShelleyTickEvent (..))
import           Cardano.Slotting.Slot (SlotNo, EpochNo (..))
import qualified Codec.CBOR.Write as CBOR
import           Codec.CBOR.Read(deserialiseFromBytes)
import           Control.State.Transition (Event)
import           Data.ByteString.Short(ShortByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import           Data.SOP (All, K (..))
import           Data.SOP.Strict (NS(..), hcmap, hcollapse)
import qualified Data.Set as Set
import           Data.Maybe.Strict(StrictMaybe(..))
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.Cardano.Block (AllegraEra, AlonzoEra,
                     BabbageEra, CardanoEras, ConwayEra, HardForkBlock,
                     MaryEra, ShelleyEra)
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
                     (OneEraLedgerEvent(..))
import           Ouroboros.Consensus.Ledger.Abstract (AuxLedgerEvent,
                     LedgerState)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock,
                     ShelleyLedgerEvent (..))
import           Ouroboros.Consensus.TypeFamilyWrappers

data LedgerEvent
  = LedgerMirDist !(Map StakeCred (Set Reward))
  | LedgerPoolReap !EpochNo !Rewards
  | LedgerIncrementalRewards !EpochNo !Rewards
  | LedgerDeltaRewards !EpochNo !Rewards
  | LedgerRestrainedRewards !EpochNo !Rewards !(Set StakeCred)
  | LedgerTotalRewards !EpochNo !(Map StakeCred (Set (Ledger.Reward StandardCrypto)))
  | LedgerStartAtEpoch !EpochNo
  -- TODO complete those vvv
  | LedgerBody
  -- | LedgerUtxoTotalDeposits
  -- | LedgerNewEpoch
  -- | LedgerRegisterPool
  -- | LedgerReRegisterPool
  | LedgerTick
  deriving (Eq, Show)

-- TODO: Review encoding & make future-proof (i.e. favor records over lists/tuples)
instance EncCBOR LedgerEvent where
  encCBOR = encode . \case
    LedgerMirDist rewards ->
      Sum LedgerMirDist 0 !> To rewards
    LedgerPoolReap epoch rewards ->
      Sum LedgerPoolReap 1 !> To epoch !> To rewards
    LedgerIncrementalRewards epoch rewards ->
      Sum LedgerIncrementalRewards 2 !> To epoch !> To rewards
    LedgerDeltaRewards epoch rewards ->
      Sum LedgerDeltaRewards 3 !> To epoch !> To rewards
    LedgerRestrainedRewards epoch rewards credentials ->
      Sum LedgerRestrainedRewards 4 !> To epoch !> To rewards !> To credentials
    LedgerTotalRewards epoch rewards ->
      Sum LedgerTotalRewards 5 !> To epoch !> To rewards
    LedgerStartAtEpoch epoch ->
      Sum LedgerStartAtEpoch 6 !> To epoch
    LedgerBody ->
      Sum LedgerBody 7
    LedgerTick ->
      Sum LedgerBody 8

instance DecCBOR LedgerEvent where
  decCBOR = decode (Summands "LedgerEvent" decRaw)
    where
      decRaw 0 = SumD LedgerMirDist <! From
      decRaw 1 = SumD LedgerPoolReap <! From <! From
      decRaw 2 = SumD LedgerIncrementalRewards <! From <! From
      decRaw 3 = SumD LedgerDeltaRewards <! From <! From
      decRaw 4 = SumD LedgerRestrainedRewards <! From <! From <! From
      decRaw 5 = SumD LedgerTotalRewards <! From <! From
      decRaw 6 = SumD LedgerStartAtEpoch <! From
      decRaw 7 = SumD LedgerBody
      decRaw 8 = SumD LedgerTick
      decRaw n = Invalid n

data Reward = Reward
  { rewardSource :: !RewardSource
  , rewardPool   :: !(StrictMaybe (PoolKeyHash))
  , rewardAmount :: !Coin
  }
  deriving (Eq, Ord, Show)

instance EncCBOR Reward where
  encCBOR Reward{rewardSource, rewardPool, rewardAmount} =
    encode $ Rec Reward !> To rewardSource !> To rewardPool !> To rewardAmount

instance DecCBOR Reward where
  decCBOR =
    decode $ RecD Reward
      <! From
      <! From
      <! From

-- The following must be in alphabetic order.
data RewardSource
  = RwdLeader
  | RwdMember
  | RwdReserves
  | RwdTreasury
  | RwdDepositRefund
  deriving (Bounded, Enum, Eq, Ord, Show)

instance EncCBOR RewardSource where
  encCBOR = encode . \case
    RwdLeader -> Sum RwdLeader 0
    RwdMember -> Sum RwdMember 1
    RwdReserves -> Sum RwdReserves 2
    RwdTreasury -> Sum RwdTreasury 3
    RwdDepositRefund -> Sum RwdDepositRefund 4

instance DecCBOR RewardSource where
  decCBOR = decode (Summands "RewardSource" decRaw)
    where
      decRaw 0 = SumD RwdLeader
      decRaw 1 = SumD RwdMember
      decRaw 2 = SumD RwdReserves
      decRaw 3 = SumD RwdTreasury
      decRaw 4 = SumD RwdDepositRefund
      decRaw n = Invalid n

type PoolKeyHash = KeyHash 'StakePool StandardCrypto

type StakeCred = Ledger.StakeCredential StandardCrypto

-- The `ledger-specs` code defines a `RewardUpdate` type that is parameterised over
-- Shelley/Allegra/Mary. This is a huge pain in the neck for `db-sync` so we define a
-- generic one instead.
newtype Rewards = Rewards
  { unRewards :: Map StakeCred (Set Reward)
  }
  deriving stock (Eq, Show)
  deriving newtype (EncCBOR, DecCBOR)

instance Ord LedgerEvent where
  a <= b = toOrdering a <= toOrdering b

toOrdering :: LedgerEvent -> Int
toOrdering ev = case ev of
  LedgerMirDist {}            -> 0
  LedgerPoolReap {}           -> 1
  LedgerIncrementalRewards {} -> 2
  LedgerDeltaRewards {}       -> 3
  LedgerRestrainedRewards {}  -> 4
  LedgerTotalRewards {}       -> 5
  LedgerStartAtEpoch {}       -> 6
  LedgerBody{}                -> 7
  LedgerTick {}               -> 8

ledgerEventName :: LedgerEvent -> Text
ledgerEventName le =
  case le of
    LedgerMirDist {}            -> "LedgerMirDist"
    LedgerPoolReap {}           -> "LedgerPoolReap"
    LedgerIncrementalRewards {} -> "LedgerIncrementalRewards"
    LedgerDeltaRewards {}       -> "LedgerDeltaRewards"
    LedgerRestrainedRewards {}  -> "LedgerRestrainedRewards"
    LedgerTotalRewards {}       -> "LedgerTotalRewards"
    LedgerStartAtEpoch {}       -> "LedgerStartAtEpoch"
    LedgerBody {}               -> "LedgerBody"
    LedgerTick {}               -> "LedgerTick"

fromAuxLedgerEvent :: forall xs . (All ConvertLedgerEvent xs) => AuxLedgerEvent (LedgerState (HardForkBlock xs)) -> Maybe LedgerEvent
fromAuxLedgerEvent = toLedgerEvent . WrapLedgerEvent @(HardForkBlock xs)

class ConvertLedgerEvent blk where
  toLedgerEvent :: WrapLedgerEvent blk -> Maybe LedgerEvent

instance All ConvertLedgerEvent xs => ConvertLedgerEvent (HardForkBlock xs) where
  toLedgerEvent =
    hcollapse
      . hcmap (Proxy @ ConvertLedgerEvent) (K . toLedgerEvent)
      . getOneEraLedgerEvent
      . unwrapLedgerEvent

instance ConvertLedgerEvent ByronBlock where
  toLedgerEvent _ = Nothing

type EventsConstraints era =
  ( Event (Ledger.EraRule "TICK" era) ~ ShelleyTickEvent era
  , Event (Ledger.EraRule "NEWEPOCH" era) ~ ShelleyNewEpochEvent era
  , Event (Ledger.EraRule "MIR" era) ~ ShelleyMirEvent era
  , Event (Ledger.EraRule "EPOCH" era) ~ ShelleyEpochEvent era
  , Event (Ledger.EraRule "POOLREAP" era) ~ ShelleyPoolreapEvent era
  , Event (Ledger.EraRule "RUPD" era) ~ RupdEvent (EraCrypto era)
  )

toLedgerEventShelley
  :: ( EraCrypto ledgerEra ~ StandardCrypto
     , EventsConstraints ledgerEra
     )
  => WrapLedgerEvent (ShelleyBlock proto ledgerEra)
  -> Maybe LedgerEvent
toLedgerEventShelley evt =
  case unwrapLedgerEvent evt of
    LETotalRewards e m ->
      Just $ LedgerTotalRewards e m
    LERestraintRewards e m creds ->
      Just $ LedgerRestrainedRewards e (convertPoolRewards m) creds
    LEDeltaReward e m ->
      Just $ LedgerDeltaRewards e (convertPoolRewards m)
    LEIncrementalReward e m ->
      Just $ LedgerIncrementalRewards e (convertPoolRewards m)
    LEMirTransfer rp tp _rtt _ttr ->
      Just $ LedgerMirDist (convertMirRewards rp tp)
    LERetiredPools r _u en ->
      Just $ LedgerPoolReap en (convertPoolDepositRefunds r)
    ShelleyLedgerEventBBODY {} ->
      Just LedgerBody
    ShelleyLedgerEventTICK {} ->
      Just LedgerTick

instance ConvertLedgerEvent (ShelleyBlock p (ShelleyEra StandardCrypto)) where
  toLedgerEvent = toLedgerEventShelley

instance ConvertLedgerEvent (ShelleyBlock p (MaryEra StandardCrypto)) where
  toLedgerEvent = toLedgerEventShelley

instance ConvertLedgerEvent (ShelleyBlock p (AllegraEra StandardCrypto)) where
  toLedgerEvent = toLedgerEventShelley

instance ConvertLedgerEvent (ShelleyBlock p (AlonzoEra StandardCrypto)) where
  toLedgerEvent = toLedgerEventShelley

instance ConvertLedgerEvent (ShelleyBlock p (BabbageEra StandardCrypto)) where
  toLedgerEvent = toLedgerEventShelley

instance ConvertLedgerEvent (ShelleyBlock p (ConwayEra StandardCrypto)) where
  -- TODO: do something with conway epoch events
  toLedgerEvent = const Nothing

eventCodecVersion :: forall crypto. Crypto crypto => OneEraLedgerEvent (CardanoEras crypto) -> Version
eventCodecVersion = \case
  OneEraLedgerEvent (          S(Z{})     ) -> eraProtVerLow @(ShelleyEra crypto)
  OneEraLedgerEvent (        S(S(Z{}))    ) -> eraProtVerLow @(AllegraEra crypto)
  OneEraLedgerEvent (      S(S(S(Z{})))   ) -> eraProtVerLow @(MaryEra crypto)
  OneEraLedgerEvent (    S(S(S(S(Z{}))))  ) -> eraProtVerLow @(AlonzoEra crypto)
  OneEraLedgerEvent (  S(S(S(S(S(Z{}))))) ) -> eraProtVerLow @(BabbageEra crypto)
  OneEraLedgerEvent (S(S(S(S(S(S(Z{}))))))) -> eraProtVerLow @(ConwayEra crypto)

--------------------------------------------------------------------------------

convertPoolDepositRefunds ::
  Map StakeCred (Map PoolKeyHash Coin) ->
  Rewards
convertPoolDepositRefunds rwds =
  Rewards $
    Map.map (Set.fromList . map convert . Map.toList) rwds
  where
    convert :: (PoolKeyHash, Coin) -> Reward
    convert (kh, coin) =
      Reward
        { rewardSource = RwdDepositRefund
        , rewardPool = SJust kh
        , rewardAmount = coin
        }

convertMirRewards ::
  Map StakeCred Coin ->
  Map StakeCred Coin ->
  Map StakeCred (Set Reward)
convertMirRewards resPay trePay =
  Map.unionWith Set.union (convertResPay resPay) (convertTrePay trePay)
  where
    convertResPay :: Map StakeCred Coin -> Map StakeCred (Set Reward)
    convertResPay = Map.map (mkPayment RwdReserves)

    convertTrePay :: Map StakeCred Coin -> Map StakeCred (Set Reward)
    convertTrePay = Map.map (mkPayment RwdTreasury)

    mkPayment :: RewardSource -> Coin -> Set Reward
    mkPayment src coin =
      Set.singleton $
        Reward
          { rewardSource = src
          , rewardPool = SNothing
          , rewardAmount = coin
          }

convertPoolRewards ::
  Map StakeCred (Set (Ledger.Reward StandardCrypto)) ->
  Rewards
convertPoolRewards rmap =
  Rewards $
    map (Set.map convertReward) rmap
  where
    convertReward :: Ledger.Reward StandardCrypto -> Reward
    convertReward sr =
      Reward
        { rewardSource = rewardTypeToSource $ Ledger.rewardType sr
        , rewardAmount = Ledger.rewardAmount sr
        , rewardPool = SJust $ Ledger.rewardPool sr
        }

rewardTypeToSource :: Ledger.RewardType -> RewardSource
rewardTypeToSource rt =
  case rt of
    Ledger.LeaderReward -> RwdLeader
    Ledger.MemberReward -> RwdMember

--------------------------------------------------------------------------------
-- Patterns for event access. Why aren't these in ledger-specs?

pattern LERestraintRewards ::
  ( EraCrypto ledgerera ~ StandardCrypto
  , Event (Ledger.EraRule "TICK" ledgerera) ~ ShelleyTickEvent ledgerera
  , Event (Ledger.EraRule "NEWEPOCH" ledgerera) ~ ShelleyNewEpochEvent ledgerera
  ) =>
  EpochNo ->
  Map StakeCred (Set (Ledger.Reward StandardCrypto)) ->
  Set StakeCred ->
  AuxLedgerEvent (LedgerState (ShelleyBlock p ledgerera))
pattern LERestraintRewards e m creds <-
  ShelleyLedgerEventTICK
    (TickNewEpochEvent (Shelley.RestrainedRewards e m creds))

pattern LETotalRewards ::
  ( EraCrypto ledgerera ~ StandardCrypto
  , Event (Ledger.EraRule "TICK" ledgerera) ~ ShelleyTickEvent ledgerera
  , Event (Ledger.EraRule "NEWEPOCH" ledgerera) ~ ShelleyNewEpochEvent ledgerera
  ) =>
  EpochNo ->
  Map StakeCred (Set (Ledger.Reward StandardCrypto)) ->
  AuxLedgerEvent (LedgerState (ShelleyBlock p ledgerera))
pattern LETotalRewards e m <-
  ShelleyLedgerEventTICK
    (TickNewEpochEvent (Shelley.TotalRewardEvent e m))

pattern LEDeltaReward ::
  ( EraCrypto ledgerera ~ StandardCrypto
  , Event (Ledger.EraRule "TICK" ledgerera) ~ ShelleyTickEvent ledgerera
  , Event (Ledger.EraRule "NEWEPOCH" ledgerera) ~ ShelleyNewEpochEvent ledgerera
  , Event (Ledger.EraRule "RUPD" ledgerera) ~ RupdEvent (EraCrypto ledgerera)
  ) =>
  EpochNo ->
  Map StakeCred (Set (Ledger.Reward StandardCrypto)) ->
  AuxLedgerEvent (LedgerState (ShelleyBlock p ledgerera))
pattern LEDeltaReward e m <-
  ShelleyLedgerEventTICK
    (TickNewEpochEvent (Shelley.DeltaRewardEvent (RupdEvent e m)))

pattern LEIncrementalReward ::
  ( EraCrypto ledgerera ~ StandardCrypto
  , Event (Ledger.EraRule "TICK" ledgerera) ~ ShelleyTickEvent ledgerera
  , Event (Ledger.EraRule "RUPD" ledgerera) ~ RupdEvent (EraCrypto ledgerera)
  ) =>
  EpochNo ->
  Map StakeCred (Set (Ledger.Reward StandardCrypto)) ->
  AuxLedgerEvent (LedgerState (ShelleyBlock p ledgerera))
pattern LEIncrementalReward e m <-
  ShelleyLedgerEventTICK
    (TickRupdEvent (RupdEvent e m))

pattern LEMirTransfer ::
  ( EraCrypto ledgerera ~ StandardCrypto
  , Event (Ledger.EraRule "TICK" ledgerera) ~ ShelleyTickEvent ledgerera
  , Event (Ledger.EraRule "NEWEPOCH" ledgerera) ~ ShelleyNewEpochEvent ledgerera
  , Event (Ledger.EraRule "MIR" ledgerera) ~ ShelleyMirEvent ledgerera
  ) =>
  Map StakeCred Coin ->
  Map StakeCred Coin ->
  DeltaCoin ->
  DeltaCoin ->
  AuxLedgerEvent (LedgerState (ShelleyBlock p ledgerera))
pattern LEMirTransfer rp tp rtt ttr <-
  ShelleyLedgerEventTICK
    ( TickNewEpochEvent
        ( Shelley.MirEvent
            ( MirTransfer
                (InstantaneousRewards rp tp rtt ttr)
              )
          )
      )

pattern LERetiredPools ::
  ( EraCrypto ledgerera ~ StandardCrypto
  , Event (Ledger.EraRule "TICK" ledgerera) ~ ShelleyTickEvent ledgerera
  , Event (Ledger.EraRule "NEWEPOCH" ledgerera) ~ ShelleyNewEpochEvent ledgerera
  , Event (Ledger.EraRule "EPOCH" ledgerera) ~ ShelleyEpochEvent ledgerera
  , Event (Ledger.EraRule "POOLREAP" ledgerera) ~ ShelleyPoolreapEvent ledgerera
  ) =>
  Map StakeCred (Map PoolKeyHash Coin) ->
  Map StakeCred (Map PoolKeyHash Coin) ->
  EpochNo ->
  AuxLedgerEvent (LedgerState (ShelleyBlock p ledgerera))
pattern LERetiredPools r u e <-
  ShelleyLedgerEventTICK
    ( TickNewEpochEvent
        ( Shelley.EpochEvent
            ( PoolReapEvent
                (RetiredPools r u e)
              )
          )
      )

data AnchoredEvent =
  AnchoredEvent { headerHash :: ShortByteString
                , slotNo :: SlotNo
                , ledgerEvent :: LedgerEvent
                }
  deriving (Eq, Show)


instance EncCBOR AnchoredEvent where
  encCBOR AnchoredEvent{headerHash, slotNo, ledgerEvent} =
    encode $  Rec AnchoredEvent !> To headerHash !> To slotNo !> To ledgerEvent

instance DecCBOR AnchoredEvent where
  decCBOR =
    decode $ RecD AnchoredEvent
      <! From
      <! From
      <! From


serializeEvent :: Version -> AnchoredEvent -> ByteString
serializeEvent codecVersion event =
  CBOR.toStrictByteString (toCBOR codecVersion) <> serialize' codecVersion event

deserializeEvent :: LBS.ByteString -> Maybe AnchoredEvent
deserializeEvent bytes = do
  case deserialiseFromBytes @Version fromCBOR bytes of
    Right (rest, version) ->
      case deserialiseFromBytes (toPlainDecoder version decCBOR) rest of
        Right (_, event) -> Just event
        Left{} -> Nothing
    Left{} -> Nothing

-- IO action to read ledger events in binary form
tailEvent :: FilePath -> IO ()
tailEvent eventsDb =
  withFile eventsDb ReadMode $ \ h -> LBS.hGetContents h >>= go
  where
    go bytes = do
     let version = maxBound
     case deserialiseFromBytes (toPlainDecoder version decCBOR) bytes of
       Right (rest, event :: AnchoredEvent) -> do
         putStrLn $ "Anchored Event: " <> show @_ @Text event
         go rest
       Left err -> putStrLn $ "FIXME: Error: " <> show @_ @Text err
