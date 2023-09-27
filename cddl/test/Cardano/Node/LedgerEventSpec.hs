{-# LANGUAGE OverloadedStrings #-}

module Cardano.Node.LedgerEventSpec where

import Prelude

import           Cardano.Node.LedgerEvent
import qualified Codec.CBOR.Schema as CDDL
import qualified Data.ByteString.Base16 as Hex
import           Data.ByteString.Lazy (fromStrict)
import           Data.ByteString.Short (toShort)
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Hedgehog (Property, discover, footnote, (===))
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           System.IO.Unsafe (unsafePerformIO)
import           Test.Hspec (Spec, describe, specify)
import           Test.Hspec.Hedgehog (hedgehog)

specification :: Text
specification =
  unsafePerformIO $ TIO.readFile "../cardano-node/ledger_events.cddl"

spec :: Spec
spec = describe "Ledger Event CDDL" $ do
  specify "All events are compliant with their cddl definitions" $ hedgehog $ do
    version <- Hedgehog.forAll Gen.enumBounded
    event <- Hedgehog.forAll genEvent
    CDDL.validate specification (serializeAnchoredEvent version event) === Right ()

genEvent :: Hedgehog.Gen AnchoredEvent
genEvent =
  AnchoredEvent
   <$> (toShort <$> Gen.bytes (Range.constant 32 32))
   <*> (fromIntegral <$> Gen.word64 Range.constantBounded)
   <*> (LedgerNewEpochEvent . LedgerStartAtEpoch . fromIntegral <$> Gen.word16 Range.constantBounded)
