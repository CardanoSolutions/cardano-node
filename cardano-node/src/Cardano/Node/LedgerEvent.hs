{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Local representation for display purpose of cardano-ledger events.
--
-- Shamelessly stolen from db-sync.
module Cardano.Node.LedgerEvent (
    ConvertLedgerEvent (..)
  , ShelleyEventsConstraints
  , ConwayEventsConstraints
  , LedgerEvent (..)
  , LedgerNewEpochEvent (..)
  , AnchoredEvent (..)
  , fromAuxLedgerEvent
  , ledgerEventName
  , eventCodecVersion
  , serializeAnchoredEvent
  , deserializeAnchoredEvent
  , foldEvent
  , filterRewards
  , parseStakeCredential
  , withLedgerEventsServerStream
  ) where

import           Cardano.Prelude hiding (All, Sum)

import           Control.Monad.Fail (MonadFail(..))
import           Cardano.Ledger.Binary (DecCBOR(..), EncCBOR(..), Version,
                   decodeFull', fromCBOR, serialize', toCBOR)
import           Cardano.Ledger.Binary.Coders (Decode(..), Encode (..), encode, (!>),
                   (<!), decode)
import           Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import           Cardano.Ledger.Credential(Credential, StakeCredential)
import           Cardano.Ledger.Rewards (Reward(..))
import qualified Cardano.Ledger.Shelley.Rules as Rules
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
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Control.State.Transition (Event)
import           Data.ByteString.Short(ShortByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString  as BS
import qualified Data.ByteString.Base16 as Hex
import qualified Data.Map.Strict as Map
import           Data.SOP.Strict (All, K (..), NS(..), hcmap, hcollapse)
import qualified Data.Set as Set
import           Data.String (String)
import           Network.Socket(PortNumber, defaultProtocol, listen, accept, bind, close, socket, socketToHandle, withSocketsDo, SockAddr(..), SocketType(Stream), Family(AF_INET))
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.Cardano.Block (AllegraEra, AlonzoEra,
                     BabbageEra, CardanoEras, ConwayEra, HardForkBlock,
                     MaryEra, ShelleyEra)
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
                     (OneEraLedgerEvent(..), getOneEraHash, getOneEraLedgerEvent)
import           Ouroboros.Consensus.Ledger.Abstract (AuxLedgerEvent,
                     LedgerEventHandler(..))
import qualified Ouroboros.Consensus.Ledger.Abstract as Abstract
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock,
                     ShelleyLedgerEvent (..))
import           Ouroboros.Consensus.TypeFamilyWrappers
import           System.IO(hIsEOF)
import Cardano.Ledger.Conway.Rules (ConwayNewEpochEvent, ConwayEpochEvent)
import qualified Cardano.Ledger.Conway.Rules as Conway
import qualified Cardano.Ledger.Shelley.API as ShelleyAPI

type LedgerState crypto =
  ExtLedgerState (HardForkBlock (CardanoEras crypto))

data LedgerEvent crypto
  = LedgerNewEpochEvent !(LedgerNewEpochEvent crypto)
  -- TODO complete those vvv
  | LedgerBody
  -- | LedgerUtxoTotalDeposits
  -- | LedgerNewEpoch
  -- | LedgerRegisterPool
  -- | LedgerReRegisterPool
  | LedgerTick
  deriving (Eq, Show)

-- TODO(KtorZ): Discuss that design choice; I believe we should favor a more
-- 'flat' structure for events instead of preserving whatever the ledger imposes
-- on us.
data LedgerNewEpochEvent crypto
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
  | LedgerStakeDistEvent
      !(Map (Credential 'Staking crypto) (Coin, KeyHash 'StakePool crypto))
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
  | LedgerTotalAdaPots
      !Coin
      -- ^ Treasury Ada pot
      !Coin
      -- ^ Reserves Ada pot
      !Coin
      -- ^ Rewards Ada pot
      !Coin
      -- ^ Utxo Ada pot
      !Coin
      -- ^ Key deposit Ada pot
      !Coin
      -- ^ Pool deposit Ada pot
      !Coin
      -- ^ Deposits Ada pot
      !Coin
      -- ^ Fees Ada pot
  | LedgerStartAtEpoch !EpochNo
  deriving (Eq, Show)

-- TODO: Review encoding & make future-proof (i.e. favor records over lists/tuples)
instance Crypto crypto => EncCBOR (LedgerEvent crypto) where
  encCBOR = encode . \case
    LedgerNewEpochEvent e -> Sum LedgerNewEpochEvent 0 !> To e
    LedgerBody -> Sum LedgerBody 1
    LedgerTick -> Sum LedgerTick 2

instance Crypto crypto => EncCBOR (LedgerNewEpochEvent crypto) where
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
    LedgerStakeDistEvent stakeDist ->
      Sum LedgerStakeDistEvent 2
        !> To stakeDist
    LedgerIncrementalRewards epoch rewards ->
      Sum LedgerIncrementalRewards 3
        !> To epoch
        !> To rewards
    LedgerDeltaRewards epoch rewards ->
      Sum LedgerDeltaRewards 4
        !> To epoch
        !> To rewards
    LedgerRestrainedRewards epoch rewards credentials ->
      Sum LedgerRestrainedRewards 5
        !> To epoch
        !> To rewards
        !> To credentials
    LedgerTotalRewards epoch rewards ->
      Sum LedgerTotalRewards 6
        !> To epoch
        !> To rewards
    LedgerStartAtEpoch epoch ->
      Sum LedgerStartAtEpoch 7
        !> To epoch
    LedgerTotalAdaPots treasuryAdaPot reservesAdaPot rewardsAdaPot utxoAdaPot keyDepositAdaPot poolDepositAdaPot depositsAdaPot feesAdaPot ->
      Sum LedgerTotalAdaPots 8
        !> To treasuryAdaPot
        !> To reservesAdaPot
        !> To rewardsAdaPot
        !> To utxoAdaPot
        !> To keyDepositAdaPot
        !> To poolDepositAdaPot
        !> To depositsAdaPot
        !> To feesAdaPot

instance Crypto crypto => DecCBOR (LedgerEvent crypto) where
  decCBOR = decode (Summands "LedgerEvent" decRaw)
    where
      decRaw 0 = SumD LedgerNewEpochEvent <! From
      decRaw 1 = SumD LedgerBody
      decRaw 2 = SumD LedgerTick
      decRaw n = Invalid n

instance Crypto crypto => DecCBOR (LedgerNewEpochEvent crypto) where
  decCBOR = decode (Summands "LedgerNewEpochEvent" decRaw)
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
      decRaw 2 = SumD LedgerStakeDistEvent
        <! From
      decRaw 3 = SumD LedgerIncrementalRewards
        <! From
        <! From
      decRaw 4 = SumD LedgerDeltaRewards
        <! From
        <! From
      decRaw 5 = SumD LedgerRestrainedRewards
        <! From
        <! From
        <! From
      decRaw 6 = SumD LedgerTotalRewards
        <! From
        <! From
      decRaw 7 = SumD LedgerStartAtEpoch
        <! From
      decRaw 8 = SumD LedgerTotalAdaPots
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
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
  LedgerNewEpochEvent LedgerMirDist {}            -> 0
  LedgerNewEpochEvent LedgerPoolReaping {}        -> 1
  LedgerNewEpochEvent LedgerStakeDistEvent {}     -> 2
  LedgerNewEpochEvent LedgerIncrementalRewards {} -> 3
  LedgerNewEpochEvent LedgerDeltaRewards {}       -> 4
  LedgerNewEpochEvent LedgerRestrainedRewards {}  -> 5
  LedgerNewEpochEvent LedgerTotalRewards {}       -> 6
  LedgerNewEpochEvent LedgerStartAtEpoch {}       -> 7
  LedgerNewEpochEvent LedgerTotalAdaPots {}       -> 8
  LedgerBody                                      -> 9
  LedgerTick                                      -> 10

ledgerEventName :: LedgerEvent crypto -> Text
ledgerEventName = \case
  LedgerNewEpochEvent e -> ledgerNewEpochEventName e
  LedgerBody {}               -> "LedgerBody"
  LedgerTick {}               -> "LedgerTick"

ledgerNewEpochEventName :: LedgerNewEpochEvent crypto -> Text
ledgerNewEpochEventName = \case
  LedgerMirDist {}            -> "LedgerMirDist"
  LedgerPoolReaping {}        -> "LedgerPoolReaping"
  LedgerStakeDistEvent {}     -> "LedgerStakeDistEvent"
  LedgerIncrementalRewards {} -> "LedgerIncrementalRewards"
  LedgerDeltaRewards {}       -> "LedgerDeltaRewards"
  LedgerRestrainedRewards {}  -> "LedgerRestrainedRewards"
  LedgerTotalRewards {}       -> "LedgerTotalRewards"
  LedgerStartAtEpoch {}       -> "LedgerStartAtEpoch"
  LedgerTotalAdaPots {}       -> "LedgerTotalAdaPots"

fromAuxLedgerEvent
  :: forall xs crypto. (All ConvertLedgerEvent xs, crypto ~ StandardCrypto)
  => AuxLedgerEvent (Abstract.LedgerState (HardForkBlock xs))
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

type ShelleyEventsConstraints era =
  ( Event (Ledger.EraRule "TICK" era) ~ ShelleyTickEvent era
  , Event (Ledger.EraRule "NEWEPOCH" era) ~ ShelleyNewEpochEvent era
  , Event (Ledger.EraRule "MIR" era) ~ ShelleyMirEvent era
  , Event (Ledger.EraRule "EPOCH" era) ~ ShelleyEpochEvent era
  , Event (Ledger.EraRule "POOLREAP" era) ~ ShelleyPoolreapEvent era
  , Event (Ledger.EraRule "RUPD" era) ~ RupdEvent (EraCrypto era)
  , Event (Ledger.EraRule "SNAP" era) ~ Rules.SnapEvent era
  , Event (Ledger.EraRule "UPEC" era) ~ Void
  )

toLedgerEventShelley
  :: forall era proto. ( EraCrypto era ~ StandardCrypto
     , ShelleyEventsConstraints era
     )
  => WrapLedgerEvent (ShelleyBlock proto era)
  -> Maybe (LedgerEvent (EraCrypto era))
toLedgerEventShelley evt =
  case unwrapLedgerEvent evt of
    ShelleyLedgerEventTICK (TickNewEpochEvent (Shelley.TotalRewardEvent epoch rewards)) ->
      Just $ LedgerNewEpochEvent $ LedgerTotalRewards epoch rewards
    ShelleyLedgerEventTICK (TickNewEpochEvent (Shelley.RestrainedRewards epoch rewards credentials)) ->
      Just $ LedgerNewEpochEvent $ LedgerRestrainedRewards epoch rewards credentials
    ShelleyLedgerEventTICK (TickNewEpochEvent (Shelley.DeltaRewardEvent (RupdEvent epoch rewards))) ->
      Just $ LedgerNewEpochEvent $ LedgerDeltaRewards epoch rewards
    ShelleyLedgerEventTICK (TickRupdEvent (RupdEvent epoch rewards)) ->
      Just $ LedgerNewEpochEvent $ LedgerIncrementalRewards epoch rewards
    ShelleyLedgerEventTICK (TickNewEpochEvent (Shelley.MirEvent transfer)) ->
      case transfer of
        MirTransfer (InstantaneousRewards fromReserve fromTreasury deltaReserve deltaTreasury) ->
          Just $ LedgerNewEpochEvent $ LedgerMirDist fromReserve fromTreasury deltaReserve deltaTreasury
        NoMirTransfer{} -> -- FIXME: create an event for this
          Nothing
    ShelleyLedgerEventTICK (TickNewEpochEvent (Shelley.EpochEvent (Shelley.PoolReapEvent (RetiredPools refunded unclaimed epoch)))) ->
       Just $ LedgerNewEpochEvent $ LedgerPoolReaping epoch refunded unclaimed
    ShelleyLedgerEventTICK (TickNewEpochEvent (Shelley.EpochEvent (Shelley.SnapEvent (Shelley.StakeDistEvent stakeDist)))) ->
      Just $ LedgerNewEpochEvent $ LedgerStakeDistEvent stakeDist
    ShelleyLedgerEventTICK (TickNewEpochEvent (Shelley.EpochEvent (Shelley.UpecEvent _))) ->
      -- There isn't any data associated with UpecEvent: Event (EraRule "UPEC" era) ~ Void
      Nothing
    ShelleyLedgerEventTICK (TickNewEpochEvent (Shelley.TotalAdaPotsEvent adaPots)) -> -- TODO: create an event for this
      Just
        $ LedgerNewEpochEvent
        $ LedgerTotalAdaPots
            (ShelleyAPI.treasuryAdaPot adaPots)
            (ShelleyAPI.reservesAdaPot adaPots)
            (ShelleyAPI.rewardsAdaPot adaPots)
            (ShelleyAPI.utxoAdaPot adaPots)
            (ShelleyAPI.keyDepositAdaPot adaPots)
            (ShelleyAPI.poolDepositAdaPot adaPots)
            (ShelleyAPI.depositsAdaPot adaPots)
            (ShelleyAPI.feesAdaPot adaPots)
    ShelleyLedgerEventBBODY _ ->
      Just LedgerBody

type ConwayEventsConstraints era =
  ( Event (Ledger.EraRule "TICK" era) ~ ShelleyTickEvent era
  , Event (Ledger.EraRule "NEWEPOCH" era) ~ ConwayNewEpochEvent era
  , Event (Ledger.EraRule "MIR" era) ~ ShelleyMirEvent era
  , Event (Ledger.EraRule "EPOCH" era) ~ ConwayEpochEvent era
  , Event (Ledger.EraRule "POOLREAP" era) ~ ShelleyPoolreapEvent era
  , Event (Ledger.EraRule "RUPD" era) ~ RupdEvent (EraCrypto era)
  , Event (Ledger.EraRule "SNAP" era) ~ Rules.SnapEvent era
  )
toConwayEventShelley
  :: forall era proto. ( EraCrypto era ~ StandardCrypto
     , ConwayEventsConstraints era
     )
  => WrapLedgerEvent (ShelleyBlock proto era)
  -> Maybe (LedgerEvent (EraCrypto era))
toConwayEventShelley evt =
  case unwrapLedgerEvent evt of
    ShelleyLedgerEventTICK (TickNewEpochEvent (Conway.TotalRewardEvent epoch rewards)) ->
      Just $ LedgerNewEpochEvent $ LedgerTotalRewards epoch rewards
    ShelleyLedgerEventTICK (TickNewEpochEvent (Conway.RestrainedRewards epoch rewards credentials)) ->
      Just $ LedgerNewEpochEvent $ LedgerRestrainedRewards epoch rewards credentials
    ShelleyLedgerEventTICK (TickNewEpochEvent (Conway.DeltaRewardEvent (RupdEvent epoch rewards))) ->
      Just $ LedgerNewEpochEvent $ LedgerDeltaRewards epoch rewards
    ShelleyLedgerEventTICK (TickNewEpochEvent (Conway.DeltaRewardEvent _)) ->
      Nothing -- Or else getting "Pattern not exhaustif" warning, but can't seem to find the missing constructor.
    ShelleyLedgerEventTICK (TickNewEpochEvent (Conway.EpochEvent (Conway.PoolReapEvent (RetiredPools refunded unclaimed epoch)))) ->
       Just $ LedgerNewEpochEvent $ LedgerPoolReaping epoch refunded unclaimed
    ShelleyLedgerEventTICK (TickNewEpochEvent (Conway.EpochEvent (Conway.SnapEvent (Shelley.StakeDistEvent stakeDist)))) ->
      Just $ LedgerNewEpochEvent $ LedgerStakeDistEvent stakeDist
    ShelleyLedgerEventTICK (TickNewEpochEvent (Conway.EpochEvent _)) ->
      Nothing -- Or else getting "Pattern not exhaustif" warning, but can't seem to find the missing constructor.
    ShelleyLedgerEventTICK (TickNewEpochEvent (Conway.TotalAdaPotsEvent adaPots)) ->
      Just
        $ LedgerNewEpochEvent
        $ LedgerTotalAdaPots
            (ShelleyAPI.treasuryAdaPot adaPots)
            (ShelleyAPI.reservesAdaPot adaPots)
            (ShelleyAPI.rewardsAdaPot adaPots)
            (ShelleyAPI.utxoAdaPot adaPots)
            (ShelleyAPI.keyDepositAdaPot adaPots)
            (ShelleyAPI.poolDepositAdaPot adaPots)
            (ShelleyAPI.depositsAdaPot adaPots)
            (ShelleyAPI.feesAdaPot adaPots)
    ShelleyLedgerEventTICK (TickRupdEvent (RupdEvent epoch rewards)) ->
      Just $ LedgerNewEpochEvent $ LedgerIncrementalRewards epoch rewards
    ShelleyLedgerEventBBODY _ ->
      Nothing

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
  toLedgerEvent = toConwayEventShelley

eventCodecVersion ::
     forall crypto. Crypto crypto
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
    { headerHash :: !ShortByteString
    , slotNo :: !SlotNo
    , ledgerEvent :: !(LedgerEvent StandardCrypto)
    }
  deriving (Eq, Show)

instance EncCBOR AnchoredEvent where
  encCBOR AnchoredEvent{headerHash, slotNo, ledgerEvent} =
    encode $ Rec AnchoredEvent
      !> To headerHash
      !> To slotNo
      !> To ledgerEvent

instance DecCBOR AnchoredEvent where
  decCBOR =
    decode $ RecD AnchoredEvent
      <! From
      <! From
      <! From

serializeAnchoredEvent :: Version -> AnchoredEvent -> ByteString
serializeAnchoredEvent version event =
  CBOR.toStrictByteString encoding
 where
  encoding =
    CBOR.encodeListLen 2
    <>
    toCBOR version
    <>
    CBOR.encodeBytes (serialize' version (encCBOR event))

deserializeAnchoredEvent
  :: LBS.ByteString
  -> Either CBOR.DeserialiseFailure (LBS.ByteString, AnchoredEvent)
deserializeAnchoredEvent =
  CBOR.deserialiseFromBytes decoder
 where
   decoder = do
    -- TODO: ensure map len is 2
    _ <- CBOR.decodeListLen
    version <- fromCBOR
    bytes <- CBOR.decodeBytes
    either (fail . show) pure (decodeFull' version bytes)

-- IO action to read ledger events in binary form
foldEvent
  :: Handle
  -> a
  -> (a -> AnchoredEvent -> IO a)
  -> IO a
foldEvent h st0 fn =
  LBS.hGetContents h >>= go st0
  where
    go st bytes = do
      eof <- hIsEOF h
      if eof then
        pure st
      else do
        (events, event) <- either (panic . show) pure $ deserializeAnchoredEvent bytes
        st' <- fn st event
        go st' events

filterRewards
  :: StakeCredential StandardCrypto
  -> Map EpochNo Coin
  -> AnchoredEvent
  -> Map EpochNo Coin
filterRewards credential st = \case
  AnchoredEvent{ledgerEvent = LedgerNewEpochEvent (LedgerTotalRewards epoch rewardsMap)} ->
    let update = Map.lookup credential rewardsMap
          & maybe identity (Map.insert epoch . mergeRewards)
     in update st
  _otherEvent -> st
 where
   mergeRewards = Set.foldr (<>) mempty . Set.map Ledger.rewardAmount

withLedgerEventsServerStream
  :: PortNumber
  -> (LedgerEventHandler IO (LedgerState StandardCrypto) -> IO ())
  -> IO ()
withLedgerEventsServerStream port handler = do
  withSocketsDo $ do
    bracket open closeSockets go
 where
  go s = do
    h <- socketToHandle s WriteMode
    handler $ LedgerEventHandler $ writeLedgerEvents h

  open = do
    sock <- socket AF_INET Stream defaultProtocol
    bind sock (SockAddrInet port 0)
    listen sock 1
    putStrLn ("Waiting for client to connect to socket..." :: String)
    (clientSock, _) <- accept sock
    pure clientSock

  closeSockets = close

  writeLedgerEvents h headerHash slotNo event = do
    case fromAuxLedgerEvent event of
      Nothing -> pure ()
      Just e -> do
        let anchoredEvent = AnchoredEvent (getOneEraHash headerHash) slotNo e
        BS.hPut h $ serializeAnchoredEvent (eventCodecVersion event) anchoredEvent
