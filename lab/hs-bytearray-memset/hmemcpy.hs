{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module CopyArray
  ( smallCpy
  ) where

import GHC.Exts
import GHC.IO

data ByteArray = ByteArray ByteArray#

smallCpy :: ByteArray -> IO ByteArray
smallCpy (ByteArray ba) = IO $ \s0 -> case newByteArray# 32# s0 of
  (# s1, mut #) -> case copyByteArray# ba 0# mut 0# 32# s1 of
    s2 -> case unsafeFreezeByteArray# mut s2 of
          (# s3, frozen #) -> (# s3, ByteArray frozen #)
