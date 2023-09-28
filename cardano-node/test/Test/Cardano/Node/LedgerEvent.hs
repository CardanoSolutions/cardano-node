{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Node.LedgerEvent where

import Prelude

import           Cardano.Node.LedgerEvent

import           Cardano.Crypto.Hash (Hash, HashAlgorithm, hashFromBytes)
import qualified Codec.CBOR.Schema as CDDL
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Hex
import           Data.ByteString.Lazy(fromStrict)
import           Data.ByteString.Short (toShort)
import           Data.Map (Map)
import           Data.Maybe (fromJust)
import           Data.String (IsString(..))
import           Data.Text (Text)
import           Hedgehog (Property, discover, footnote, (===))
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

specification :: Text
specification =
  unsafePerformIO $ TIO.readFile "./ledger_events.cddl"

prop_roundtrip_LedgerEvent_CBOR :: Property
prop_roundtrip_LedgerEvent_CBOR =
  Hedgehog.property $ do
    version <- Hedgehog.forAll Gen.enumBounded
    event <- Hedgehog.forAll genAnchoredEvent
    footnote ("serialized event: " <> show (Hex.encode $ serializeAnchoredEvent version event))
    Hedgehog.tripping event
      (serializeAnchoredEvent version)
      (fmap snd . deserializeAnchoredEvent . fromStrict)

prop_LedgerEvent_CDDL_conformance :: Property
prop_LedgerEvent_CDDL_conformance =
  Hedgehog.property $ do
  version <- Hedgehog.forAll Gen.enumBounded
  event <- Hedgehog.forAll genAnchoredEvent
  Hedgehog.label (labelName event)
  -- FIXME: We do want to validate full anchored events here, not just ledger events.
  -- This requires the `cddl-cat` Rust crate to support the '.cbor' control
  -- operator which should make for a straightforward and nice contribution.
  let bytes = serialize' version (ledgerEvent event)
  CDDL.validate specification bytes === Right ()

--
-- Generators
--

genAnchoredEvent :: Hedgehog.Gen AnchoredEvent
genAnchoredEvent =
  AnchoredEvent
    <$> (toShort <$> Gen.bytes (Range.constant 32 32))
    <*> (fromIntegral <$> Gen.word64 Range.constantBounded)
    <*> Gen.choice
      [ LedgerNewEpochEvent <$> genLedgerNewEpochEvent
      ]

genLedgerNewEpochEvent :: Hedgehog.Gen (LedgerNewEpochEvent StandardCrypto)
genLedgerNewEpochEvent = Gen.choice
  [ LedgerMirDist
      <$> genStakeDistribution
      <*> genStakeDistribution
      <*> genDeltaCoin
      <*> genDeltaCoin
  , LedgerPoolReaping
      <$> genEpoch
      <*> genStakePoolRefunds
      <*> genStakePoolRefunds
  , LedgerStartAtEpoch
      <$> genEpoch
  ]

genCoin :: Hedgehog.Gen Coin
genCoin =
  Coin . fromIntegral <$> Gen.word32 Range.constantBounded

genCredential :: Hedgehog.Gen (Credential 'Staking StandardCrypto)
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

genKeyHash :: Hedgehog.Gen (KeyHash any StandardCrypto)
genKeyHash =
  KeyHash . unsafeHashFromBytes <$> Gen.bytes (Range.singleton 28)

genScriptHash :: Hedgehog.Gen (ScriptHash StandardCrypto)
genScriptHash =
  ScriptHash . unsafeHashFromBytes <$> Gen.bytes (Range.singleton 28)

genStakeDistribution :: Hedgehog.Gen (Map (Credential 'Staking StandardCrypto) Coin)
genStakeDistribution =
  Gen.map
    (Range.linear 0 3)
    ((,) <$> genCredential <*> genCoin)

genStakePoolRefunds
  :: Hedgehog.Gen
      (Map
        (Credential 'Staking StandardCrypto)
        (Map
          (KeyHash 'StakePool StandardCrypto)
          Coin
        )
      )
genStakePoolRefunds =
  Gen.map
    (Range.linear 0 3)
    ((,) <$> genCredential
         <*> Gen.map
               (Range.linear 0 3)
               ((,) <$> genKeyHash <*> genCoin)
    )

--
-- Helpers
--

labelName
  :: AnchoredEvent
  -> Hedgehog.LabelName
labelName =
  fromString . T.unpack . ledgerEventName . ledgerEvent

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
