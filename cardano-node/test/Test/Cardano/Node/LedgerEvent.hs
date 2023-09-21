{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Cardano.Node.LedgerEvent where

import           Cardano.Node.LedgerEvent
import           Data.ByteString.Short(toShort)
import           Hedgehog (Property, discover)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


prop_roundtrip_LedgerEvent_CBOR :: Property
prop_roundtrip_LedgerEvent_CBOR =
  Hedgehog.property $ do
    event <- Hedgehog.forAll genEvent
    Hedgehog.tripping event (serializeEvent maxBound) (deserializeEvent maxBound)

genEvent :: Hedgehog.Gen AnchoredEvent
genEvent =
  AnchoredEvent
   <$> (toShort <$> Gen.bytes (Range.constant 16 16))
   <*> (fromIntegral <$> Gen.word64 Range.constantBounded)
   <*> (LedgerStartAtEpoch . fromIntegral <$> Gen.word16 Range.constantBounded)

tests :: IO Bool
tests =
  Hedgehog.checkParallel $$discover
