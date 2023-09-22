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
  , ledgerEventName
  , eventCodecVersion
  , deserializeEvent
  , serializeEvent

  , foldEvent
  , filterRewards
  , parseStakeCredential
  ) where

import           Cardano.Prelude hiding (All, Sum)

import           Cardano.Ledger.Binary (DecCBOR(..), EncCBOR(..), Version, fromCBOR,
                   serialize', toCBOR, toPlainDecoder)
import           Cardano.Ledger.Binary.Coders (Decode(..), Encode (..), encode, (!>),
                   (<!), decode)
import           Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import           Cardano.Ledger.Credential(Credential, StakeCredential)
import           Cardano.Ledger.Rewards (Reward(..))
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Address as Ledger
import           Cardano.Ledger.Core (eraProtVerLow)
import           Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import           Cardano.Ledger.Keys (KeyRole (..))
import           Cardano.Ledger.Shelley.API (InstantaneousRewards (..), KeyHash)
import           Cardano.Ledger.Shelley.Core (EraCrypto)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import           Cardano.Ledger.Shelley.Rules (RupdEvent (..),
                     ShelleyEpochEvent (..), ShelleyMirEvent (..),
                     ShelleyNewEpochEvent, ShelleyPoolreapEvent (..),
                     ShelleyTickEvent (..))
import           Cardano.Slotting.Slot (SlotNo, EpochNo (..))
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Codec.CBOR.Read(deserialiseFromBytes)
import           Control.State.Transition (Event)
import           Data.ByteString.Short(ShortByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base16 as Hex
import qualified Data.Map.Strict as Map
import           Data.SOP (All, K (..))
import           Data.SOP.Strict (NS(..), hcmap, hcollapse)
import qualified Data.Set as Set
import           Data.String (String)
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
import           System.IO(hIsEOF)

data LedgerEvent crypto
  = LedgerMirDist
      !(Map (StakeCredential crypto) Coin)
        -- ^ Rewards paid from the __Reserve__ into stake credentials
      !(Map (StakeCredential crypto) Coin)
        -- ^ Rewards paid from the __Treasury__ to stake credentials
      !DeltaCoin
        -- ^ Transfer from the __Reserve__ into the __Treasury__
      !DeltaCoin
        -- ^ Transfer from the __Treasury__ into the __Reserve__
  | LedgerPoolReaping
      !EpochNo
        -- ^ Epoch from which the event is emitted
      !(Map (Credential 'Staking crypto) (Map (KeyHash 'StakePool crypto) Coin))
        -- ^ Stake pools refunds after retirement
      !(Map (Credential 'Staking crypto) (Map (KeyHash 'StakePool crypto) Coin))
        -- ^ Unclaimed deposit after retirement, for stake credentials that no longer exist.
  | LedgerIncrementalRewards
      !EpochNo
      !(Map (StakeCredential crypto) (Set (Reward crypto)))
  | LedgerDeltaRewards
      !EpochNo
      !(Map (StakeCredential crypto) (Set (Reward crypto)))
  | LedgerRestrainedRewards
      !EpochNo
      !(Map (StakeCredential crypto) (Set (Reward crypto)))
      !(Set (StakeCredential crypto))
  | LedgerTotalRewards
      !EpochNo
      !(Map (StakeCredential crypto) (Set (Reward crypto)))
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
instance Crypto crypto => EncCBOR (LedgerEvent crypto) where
  encCBOR = encode . \case
    LedgerMirDist fromReserve fromTreasury deltaReserve deltaTreasury ->
      Sum LedgerMirDist 0
        !> To fromReserve
        !> To fromTreasury
        !> To deltaReserve
        !> To deltaTreasury
    LedgerPoolReaping epoch refunded unclaimed ->
      Sum LedgerPoolReaping 1
        !> To epoch
        !> To refunded
        !> To unclaimed
    LedgerIncrementalRewards epoch rewards ->
      Sum LedgerIncrementalRewards 2
        !> To epoch
        !> To rewards
    LedgerDeltaRewards epoch rewards ->
      Sum LedgerDeltaRewards 3
        !> To epoch
        !> To rewards
    LedgerRestrainedRewards epoch rewards credentials ->
      Sum LedgerRestrainedRewards 4
        !> To epoch
        !> To rewards
        !> To credentials
    LedgerTotalRewards epoch rewards ->
      Sum LedgerTotalRewards 5
        !> To epoch
        !> To rewards
    LedgerStartAtEpoch epoch ->
      Sum LedgerStartAtEpoch 6
        !> To epoch
    LedgerBody ->
      Sum LedgerBody 7
    LedgerTick ->
      Sum LedgerBody 8

instance Crypto crypto => DecCBOR (LedgerEvent crypto) where
  decCBOR = decode (Summands "LedgerEvent" decRaw)
    where
      decRaw 0 = SumD LedgerMirDist
        <! From
        <! From
        <! From
        <! From
      decRaw 1 = SumD LedgerPoolReaping
        <! From
        <! From
        <! From
      decRaw 2 = SumD LedgerIncrementalRewards
        <! From
        <! From
      decRaw 3 = SumD LedgerDeltaRewards
        <! From
        <! From
      decRaw 4 = SumD LedgerRestrainedRewards
        <! From
        <! From
        <! From
      decRaw 5 = SumD LedgerTotalRewards
        <! From
        <! From
      decRaw 6 = SumD LedgerStartAtEpoch
        <! From
      decRaw 7 = SumD LedgerBody
      decRaw 8 = SumD LedgerTick
      decRaw n = Invalid n

-- | Parse a 'StakeCredential' from a stake address in base16.
parseStakeCredential :: String -> Maybe (StakeCredential StandardCrypto)
parseStakeCredential str =
   case Hex.decode (B8.pack str) of
     Right bytes -> Ledger.getRwdCred <$> Ledger.decodeRewardAcnt bytes
     Left{} -> Nothing

instance Ord (LedgerEvent crypto) where
  a <= b = toOrdering a <= toOrdering b

toOrdering :: LedgerEvent crypto -> Int
toOrdering = \case
  LedgerMirDist {}            -> 0
  LedgerPoolReaping {}        -> 1
  LedgerIncrementalRewards {} -> 2
  LedgerDeltaRewards {}       -> 3
  LedgerRestrainedRewards {}  -> 4
  LedgerTotalRewards {}       -> 5
  LedgerStartAtEpoch {}       -> 6
  LedgerBody{}                -> 7
  LedgerTick {}               -> 8

ledgerEventName :: LedgerEvent crypto -> Text
ledgerEventName = \case
  LedgerMirDist {}            -> "LedgerMirDist"
  LedgerPoolReaping {}        -> "LedgerPoolReaping"
  LedgerIncrementalRewards {} -> "LedgerIncrementalRewards"
  LedgerDeltaRewards {}       -> "LedgerDeltaRewards"
  LedgerRestrainedRewards {}  -> "LedgerRestrainedRewards"
  LedgerTotalRewards {}       -> "LedgerTotalRewards"
  LedgerStartAtEpoch {}       -> "LedgerStartAtEpoch"
  LedgerBody {}               -> "LedgerBody"
  LedgerTick {}               -> "LedgerTick"

fromAuxLedgerEvent
  :: forall xs crypto. (All ConvertLedgerEvent xs, crypto ~ StandardCrypto)
  => AuxLedgerEvent (LedgerState (HardForkBlock xs))
  -> Maybe (LedgerEvent crypto)
fromAuxLedgerEvent =
  toLedgerEvent . WrapLedgerEvent @(HardForkBlock xs)

class ConvertLedgerEvent blk where
  toLedgerEvent :: WrapLedgerEvent blk -> Maybe (LedgerEvent StandardCrypto)

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
  :: ( EraCrypto era ~ StandardCrypto
     , EventsConstraints era
     )
  => WrapLedgerEvent (ShelleyBlock proto era)
  -> Maybe (LedgerEvent (EraCrypto era))
toLedgerEventShelley evt =
  case unwrapLedgerEvent evt of
    ShelleyLedgerEventTICK (TickNewEpochEvent (Shelley.TotalRewardEvent epoch rewards)) ->
      Just $ LedgerTotalRewards epoch rewards
    ShelleyLedgerEventTICK (TickNewEpochEvent (Shelley.RestrainedRewards epoch rewards credentials)) ->
      Just $ LedgerRestrainedRewards epoch rewards credentials
    ShelleyLedgerEventTICK (TickNewEpochEvent (Shelley.DeltaRewardEvent (RupdEvent epoch rewards))) ->
      Just $ LedgerDeltaRewards epoch rewards
    ShelleyLedgerEventTICK (TickRupdEvent (RupdEvent epoch rewards)) ->
      Just $ LedgerIncrementalRewards epoch rewards
    ShelleyLedgerEventTICK (TickNewEpochEvent (Shelley.MirEvent transfer)) ->
      case transfer of
        MirTransfer (InstantaneousRewards fromReserve fromTreasury deltaReserve deltaTreasury) ->
          Just $ LedgerMirDist fromReserve fromTreasury deltaReserve deltaTreasury
        NoMirTransfer{} -> -- FIXME: create an event for this
          Nothing
    ShelleyLedgerEventTICK (TickNewEpochEvent (Shelley.EpochEvent poolReap)) ->
      case poolReap of
        PoolReapEvent (RetiredPools refunded unclaimed epoch) ->
          Just $ LedgerPoolReaping epoch refunded unclaimed
        SnapEvent _ -> Nothing
        UpecEvent _ -> Nothing
    ShelleyLedgerEventBBODY {} ->
      Just LedgerBody
    ShelleyLedgerEventTICK {} ->
      Just LedgerTick

instance ConvertLedgerEvent (ShelleyBlock proto (ShelleyEra StandardCrypto)) where
  toLedgerEvent = toLedgerEventShelley

instance ConvertLedgerEvent (ShelleyBlock proto (MaryEra StandardCrypto)) where
  toLedgerEvent = toLedgerEventShelley

instance ConvertLedgerEvent (ShelleyBlock proto (AllegraEra StandardCrypto)) where
  toLedgerEvent = toLedgerEventShelley

instance ConvertLedgerEvent (ShelleyBlock proto (AlonzoEra StandardCrypto)) where
  toLedgerEvent = toLedgerEventShelley

instance ConvertLedgerEvent (ShelleyBlock proto (BabbageEra StandardCrypto)) where
  toLedgerEvent = toLedgerEventShelley

instance ConvertLedgerEvent (ShelleyBlock proto (ConwayEra StandardCrypto)) where
  -- TODO: do something with conway epoch events
  toLedgerEvent = const Nothing

eventCodecVersion
  :: forall crypto. Crypto crypto
  => OneEraLedgerEvent (CardanoEras crypto)
  -> Version
eventCodecVersion = \case
  OneEraLedgerEvent (          S(Z{})     ) -> eraProtVerLow @(ShelleyEra crypto)
  OneEraLedgerEvent (        S(S(Z{}))    ) -> eraProtVerLow @(AllegraEra crypto)
  OneEraLedgerEvent (      S(S(S(Z{})))   ) -> eraProtVerLow @(MaryEra crypto)
  OneEraLedgerEvent (    S(S(S(S(Z{}))))  ) -> eraProtVerLow @(AlonzoEra crypto)
  OneEraLedgerEvent (  S(S(S(S(S(Z{}))))) ) -> eraProtVerLow @(BabbageEra crypto)
  OneEraLedgerEvent (S(S(S(S(S(S(Z{}))))))) -> eraProtVerLow @(ConwayEra crypto)

data AnchoredEvent =
  AnchoredEvent
    { headerHash :: ShortByteString
    , slotNo :: SlotNo
    , ledgerEvent :: LedgerEvent StandardCrypto
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
foldEvent
  :: (a -> AnchoredEvent -> IO a)
  -> a
  -> Handle
  -> IO a
foldEvent fn st0 h =
  LBS.hGetContents h >>= go st0
  where
    go st bytes = do
      eof <- hIsEOF h
      if eof then
        return st
      else do
        (rest, version :: Version) <- unsafeDeserialiseFromBytes
          fromCBOR
          bytes

        (events, event :: AnchoredEvent) <- unsafeDeserialiseFromBytes
          (toPlainDecoder version decCBOR)
          rest

        st' <- fn st event

        go st' events

    unsafeDeserialiseFromBytes
      :: (forall s. CBOR.Decoder s a)
      -> LBS.ByteString
      -> IO (LBS.ByteString, a)
    unsafeDeserialiseFromBytes decoder =
      either (panic . show) pure . deserialiseFromBytes decoder

filterRewards
  :: StakeCredential StandardCrypto
  -> Map EpochNo Coin
  -> AnchoredEvent
  -> Map EpochNo Coin
filterRewards credential st = \case
  AnchoredEvent{ledgerEvent = LedgerTotalRewards epoch rewardsMap} ->
    let update = Map.lookup credential rewardsMap
          & maybe identity (Map.insert epoch . mergeRewards)
     in update st
  _ ->
    st
 where
   mergeRewards = Set.foldr (<>) mempty . Set.map Ledger.rewardAmount
