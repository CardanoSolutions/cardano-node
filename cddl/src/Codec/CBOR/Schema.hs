{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.CBOR.Schema where

import Prelude

import Control.Exception (SomeException(..), Exception(..), catch)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Foreign.C.String (CString, withCStringLen)
import System.IO.Unsafe (unsafePerformIO)

validate
  :: Text
    -- ^ A CDDL specification
  -> ByteString
    -- ^ Some CBOR value
  -> Either String ()
validate cddl cbor = unsafePerformIO $ do
  withCStringLen (Text.unpack cddl) $ \(cddl_ptr, cddl_len) ->
    BS.useAsCStringLen cbor $ \(cbor_ptr, cbor_len) ->
      ( Right <$> validate_cbor
        cddl_ptr (fromIntegral cddl_len)
        cbor_ptr (fromIntegral cbor_len)
      ) `catch` \(SomeException e) -> return (Left (displayException e))

foreign import ccall "cbits/libcddl.h validate_cbor"
  validate_cbor
    :: CString
    -> Word
    -> CString
    -> Word
    -> IO ()
