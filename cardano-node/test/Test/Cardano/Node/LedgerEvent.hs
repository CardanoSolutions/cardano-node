{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Node.LedgerEvent where

import           Prelude

import           Cardano.Node.LedgerEvent

import           Cardano.Crypto.Hash (Hash, HashAlgorithm, hashFromBytes)
import           Cardano.Ledger.Alonzo.Rules (AlonzoBbodyEvent (ShelleyInAlonzoEvent))
import qualified Cardano.Ledger.Conway.Rules as Conway
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import           Cardano.Ledger.Shelley.API (InstantaneousRewards (..), KeyHash (..),
                   ScriptHash (..))
import           Cardano.Ledger.Shelley.Rules (RupdEvent (..), ShelleyEpochEvent (..),
                   ShelleyMirEvent (..), ShelleyNewEpochEvent, ShelleyPoolreapEvent (..),
                   ShelleyTickEvent (..))
import qualified Codec.CBOR.Schema as CDDL
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Hex
import           Data.ByteString.Lazy (fromStrict)
import           Data.ByteString.Short (ShortByteString, toShort)
import qualified Data.ByteString.Short as SB
import           Data.Char (ord)
import           Data.Foldable (for_, toList)
import           Data.IORef (modifyIORef, newIORef, readIORef)
import           Data.Map (Map)
import           Data.Maybe (fromJust)
import           Data.Set (Set)
import           Data.SOP.Index (Index, injectNS)
import           Data.String (IsString (..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Hedgehog ((===), Property, discover, footnote)
import qualified Hedgehog
import qualified Hedgehog.Extras.Test.Process as Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Property as Hedgehog
import qualified Hedgehog.Range as Range
import           Ouroboros.Consensus.Cardano.Block (CardanoEras, HardForkBlock)
import           Ouroboros.Consensus.HardFork.Combinator.Ledger ()
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (OneEraHash (OneEraHash), OneEraLedgerEvent (OneEraLedgerEvent))
import           Ouroboros.Consensus.Ledger.Abstract (AuxLedgerEvent, LedgerEventHandler (handleLedgerEvent), LedgerState)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock, ShelleyLedgerEvent (..))
import           Ouroboros.Consensus.TypeFamilyWrappers (WrapLedgerEvent (WrapLedgerEvent))
import           Ouroboros.Network.Block (ChainHash (BlockHash, GenesisHash), HeaderHash)
import           System.FilePath ((</>))
import           System.IO.Unsafe (unsafePerformIO)
import           Text.Printf (printf)

specification :: Text
specification =
  unsafePerformIO $ do
    base <- either (fail . show) pure . fst =<< Hedgehog.runTestT Hedgehog.getProjectBase
    TIO.readFile $ base </> "cardano-node/test-data/ledger_events.cddl"
{-# NOINLINE specification #-}

prop_roundtrip_LedgerEvent_CBOR :: Property
prop_roundtrip_LedgerEvent_CBOR =
  Hedgehog.property $ do
    version <- Hedgehog.forAll Gen.enumBounded
    event <- Hedgehog.forAll genAnchoredEvents
    footnote ("serialized event: " <> show (Hex.encode $ serializeVersioned $ Versioned version event))
    Hedgehog.tripping (Versioned version event)
      serializeVersioned
      (fmap snd . deserializeVersioned . fromStrict)

prop_LedgerEvent_CDDL_conformance :: Property
prop_LedgerEvent_CDDL_conformance =
  Hedgehog.property $ do
  version <- Hedgehog.forAll Gen.enumBounded
  event <- Hedgehog.forAll genAnchoredEvents
  Hedgehog.label (labelName event)
  -- FIXME: We do want to validate full anchored events here, not just ledger events.
  -- This requires the `cddl-cat` Rust crate to support the '.cbor' control
  -- operator which should make for a straightforward and nice contribution.
  for_ (ledgerEvents event) $ \le -> do
    let bytes = serialize' version le
    case CDDL.validate specification bytes of
      Right () ->
        Hedgehog.success
      Left (CDDL.ValidationError { CDDL.cbor = cbor, CDDL.hint = hint }) -> do
        Hedgehog.footnote hint
        Hedgehog.footnote cbor
        Hedgehog.failure

prop_LedgerEventHandler_sequentialEvents :: Property
prop_LedgerEventHandler_sequentialEvents =
  Hedgehog.property $ do

  auxEvents <- Hedgehog.forAll $
    Gen.list (Range.linear 2 20) $
      Gen.list (Range.linear 1 3)
        genAuxLedgerEvent

  start <- Hedgehog.forAll $ Gen.word $ Range.constant 1 99

  let slots = zip [start ..] auxEvents

  anchoredEvents <- liftIO $ do
    ref <- newIORef []
    let writer aes = modifyIORef ref (aes :)
        handler = handleLedgerEvent $ mkLedgerEventHandler writer
    for_ slots $ \(s, es) -> do
      let p = dummyChainHash (pred s)
          h = dummyHeaderHash s
      handler p h (fromIntegral s) 1 es
    map versionedData . reverse <$> readIORef ref

  let prevs = map prevBlockHeaderHash anchoredEvents
      currs = map (At . blockHeaderHash) anchoredEvents

  tail prevs === init currs

dummyHeaderHash :: Word -> HeaderHash (HardForkBlock (CardanoEras StandardCrypto))
dummyHeaderHash = OneEraHash . SB.pack . map (fromIntegral . ord) . printf "%032d"

dummyChainHash :: Word -> ChainHash (HardForkBlock (CardanoEras StandardCrypto))
dummyChainHash 0 = GenesisHash
dummyChainHash i = BlockHash $ dummyHeaderHash i

--
-- Generators
--

type StakePoolId = KeyHash 'StakePool StandardCrypto

type StakeCredential = Credential 'Staking StandardCrypto

genAuxLedgerEvent :: forall xs. Hedgehog.Gen (AuxLedgerEvent (LedgerState (HardForkBlock xs)))
genAuxLedgerEvent =
  Gen.choice
    -- TODO: Add more types
    (
    [ injectLedgerEvent undefined . ShelleyLedgerEventTICK . TickNewEpochEvent <$> (Conway.TotalRewardEvent <$> genEpoch <*> genRewardDistribution)
    , injectLedgerEvent undefined . ShelleyLedgerEventBBODY . ShelleyInAlonzoEvent . Shelley.LedgersEvent . Shelley.LedgerEvent . Conway.GovEvent <$> pure _
    ]
    :: [Hedgehog.Gen (AuxLedgerEvent (LedgerState (HardForkBlock xs)))]
    )
  where
    injectLedgerEvent :: Index xs blk -> AuxLedgerEvent (LedgerState blk) -> OneEraLedgerEvent xs
    injectLedgerEvent index =
          OneEraLedgerEvent
        . injectNS index
        . WrapLedgerEvent


genAnchoredEvents :: Hedgehog.Gen AnchoredEvents
genAnchoredEvents =
  AnchoredEvents
    <$> (At <$> genBlockHeaderHash)
    <*> genBlockHeaderHash
    <*> genSlotNo
    <*> genBlockNo
    <*> Gen.list
          (Range.linear 0 20)
          (Gen.choice
            (mconcat
              [ fmap LedgerNewEpochEvent <$> genLedgerNewEpochEvent
              , fmap LedgerRewardUpdateEvent <$> genLedgerRewardUpdateEvent
              ]))

genLedgerNewEpochEvent :: [Hedgehog.Gen (LedgerNewEpochEvent StandardCrypto)]
genLedgerNewEpochEvent =
  [ LedgerMirDist
      <$> genStakeDistribution
      <*> genStakeDistribution
      <*> genDeltaCoin
      <*> genDeltaCoin
  , LedgerPoolReaping
      <$> genEpoch
      <*> genStakePoolRefunds
      <*> genStakePoolRefunds
  , LedgerStakeDistEvent
      <$> genExtendedStakeDistribution
  , LedgerRestrainedRewards
      <$> genEpoch
      <*> genRewardDistribution
      <*> Gen.set (Range.linear 0 3) genCredential
  , LedgerTotalRewards
      <$> genEpoch
      <*> genRewardDistribution
  , LedgerTotalAdaPots
      <$> genCoin
      <*> genCoin
      <*> genCoin
      <*> genCoin
      <*> genCoin
      <*> genCoin
      <*> genCoin
      <*> genCoin
  ]

genLedgerRewardUpdateEvent :: [Hedgehog.Gen (LedgerRewardUpdateEvent StandardCrypto)]
genLedgerRewardUpdateEvent =
  [ LedgerIncrementalRewards
      <$> genEpoch
      <*> genRewardDistribution
  ]

genBlockHeaderHash :: Hedgehog.Gen ShortByteString
genBlockHeaderHash =
  toShort <$> Gen.bytes (Range.constant 32 32)

genCoin :: Hedgehog.Gen Coin
genCoin =
  Coin . fromIntegral <$> Gen.word32 Range.constantBounded

genCredential :: Hedgehog.Gen StakeCredential
genCredential = Gen.choice
  [ ScriptHashObj <$> genScriptHash
  , KeyHashObj <$> genKeyHash
  ]

genDeltaCoin :: Hedgehog.Gen DeltaCoin
genDeltaCoin =
  DeltaCoin <$> Gen.integral_ (Range.linear (-100) 100)

genEpoch :: Hedgehog.Gen EpochNo
genEpoch =
  fromIntegral <$> Gen.word16 Range.constantBounded

genExtendedStakeDistribution :: Hedgehog.Gen (Map StakeCredential (Coin, StakePoolId))
genExtendedStakeDistribution =
  genStakeCredentialMap $ (,) <$> genCoin <*> genKeyHash

genKeyHash :: Hedgehog.Gen (KeyHash any StandardCrypto)
genKeyHash =
  KeyHash . unsafeHashFromBytes <$> Gen.bytes (Range.singleton 28)

genReward :: Hedgehog.Gen (Reward StandardCrypto)
genReward = Reward
  <$> Gen.enumBounded
  <*> genKeyHash
  <*> genCoin

genRewardDistribution :: Hedgehog.Gen (Map StakeCredential (Set (Reward StandardCrypto)))
genRewardDistribution =
  genStakeCredentialMap $ Gen.set (Range.linear 1 3) genReward

genScriptHash :: Hedgehog.Gen (ScriptHash StandardCrypto)
genScriptHash =
  ScriptHash . unsafeHashFromBytes <$> Gen.bytes (Range.singleton 28)

genSlotNo :: Hedgehog.Gen SlotNo
genSlotNo =
  fromIntegral <$> Gen.word64 Range.constantBounded

genBlockNo :: Hedgehog.Gen BlockNo
genBlockNo =
  fromIntegral <$> Gen.int Range.constantBounded

genStakeDistribution :: Hedgehog.Gen (Map StakeCredential Coin)
genStakeDistribution =
  genStakeCredentialMap genCoin

genStakePoolRefunds :: Hedgehog.Gen (Map StakeCredential (Map StakePoolId Coin))
genStakePoolRefunds =
  genStakeCredentialMap $ Gen.map (Range.linear 1 3) $ (,) <$> genKeyHash <*> genCoin

--
-- Helpers
--

genStakeCredentialMap :: Hedgehog.Gen a -> Hedgehog.Gen (Map StakeCredential a)
genStakeCredentialMap genValue =
  Gen.map (Range.linear 0 3) ((,) <$> genCredential <*> genValue)

labelName
  :: AnchoredEvents
  -> Hedgehog.LabelName
labelName =
  fromString . T.unpack . T.intercalate "," . map ledgerEventName . toList . ledgerEvents

unsafeHashFromBytes
  :: (HashAlgorithm algo)
  => ByteString
  -> Hash algo a
unsafeHashFromBytes =
  fromJust . hashFromBytes

--
-- Auto-discover tests / Template Haskell
--

tests :: IO Bool
tests =
  Hedgehog.checkParallel $$discover
