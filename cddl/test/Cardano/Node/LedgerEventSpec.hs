{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Node.LedgerEventSpec where

import Prelude

import           Cardano.Crypto.Hash (Hash, HashAlgorithm, hashFromBytes)
import           Cardano.Node.LedgerEvent
import qualified Codec.CBOR.Schema as CDDL
import           Data.ByteString (ByteString)
import           Data.ByteString.Short (toShort)
import           Data.Map (Map)
import           Data.Maybe (fromJust)
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Hedgehog ((===))
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           System.IO.Unsafe (unsafePerformIO)
import           Test.Hspec (Spec, describe, shouldReturn, specify)

spec :: Spec
spec = describe "Ledger Event CDDL" $ do
  specify "All events are compliant with their cddl definitions" $ do
    flip shouldReturn True $ Hedgehog.checkParallel $ Hedgehog.Group "Properties"
      [ ( "prop_cddl_compliance"
        , Hedgehog.property $ do
          version <- Hedgehog.forAll Gen.enumBounded
          event <- Hedgehog.forAll genAnchoredEvent
          Hedgehog.label (labelName event)
          -- FIXME: We do want to validate full anchored events here, not just ledger events.
          let bytes = serialize' version (ledgerEvent event)
          CDDL.validate specification bytes === Right ()
        )
      ]

specification :: Text
specification =
  unsafePerformIO $ TIO.readFile "../cardano-node/ledger_events.cddl"

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
