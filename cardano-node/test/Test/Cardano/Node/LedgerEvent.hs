{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Cardano.Node.LedgerEvent where

import           Cardano.Node.LedgerEvent
import           Data.ByteString.Short(toShort)
import           Data.ByteString.Lazy(fromStrict)
import qualified Data.ByteString.Base16 as Hex
import           Hedgehog (Property, discover, footnote)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

prop_roundtrip_LedgerEvent_CBOR :: Property
prop_roundtrip_LedgerEvent_CBOR =
  Hedgehog.property $ do
    version <- Hedgehog.forAll Gen.enumBounded
    event <- Hedgehog.forAll genEvent
    footnote ("serialized event: " <> show (Hex.encode $ serializeEvent version event))
    Hedgehog.tripping event (serializeEvent version) (deserializeEvent . fromStrict)

genEvent :: Hedgehog.Gen AnchoredEvent
genEvent =
  AnchoredEvent
   <$> (toShort <$> Gen.bytes (Range.constant 32 32))
   <*> (fromIntegral <$> Gen.word64 Range.constantBounded)
   <*> (LedgerNewEpochEvent . LedgerStartAtEpoch . fromIntegral <$> Gen.word16 Range.constantBounded)

tests :: IO Bool
tests =
  Hedgehog.checkParallel $$discover
