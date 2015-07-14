{-# OPTIONS_GHC -Wall #-}

module Cipher (encrypt, decrypt) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (pack, unpack)
import Data.Bits (xor)
import Data.List (unfoldr)
import Data.Word (Word8)
import Unsafe.Coerce (unsafeCoerce)

newtype Crypted a = Crypted a

instance Show a => Show (Crypted a) where
  show (Crypted a) = show a

gen :: Integral a => a -> [Word8]
gen = unsafeCoerce . map (`mod` 255) . drop 1 . unfoldr coalg where
  coalg j = Just (j, (1664525 * j + 1013904223) `mod` (2 ^ (32 :: Int)))

toggle :: Integral a => ByteString -> a -> ByteString
toggle text key = BS.pack $ zipWith xor unpacked rands where
  rands    = gen key
  unpacked = BS.unpack text

encrypt :: Integral a => ByteString -> a -> Crypted ByteString
encrypt text = Crypted . toggle text

decrypt :: Integral a => Crypted ByteString -> a -> ByteString
decrypt (Crypted text) = toggle text

