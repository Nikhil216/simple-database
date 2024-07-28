module Cksum (main) where

import System.Environment (getArgs)
import Data.Digest.CRC32 (crc32)
import Data.ByteString.Lazy (readFile, ByteString)
import Data.Word (Word32)
import Prelude hiding (readFile)

cksum :: ByteString -> Word32
cksum = crc32 0x04c11db7 0x00000000 False False 0xffffffff

main :: IO ()
main = do filePath:_ <- getArgs
          content <- readFile filePath
          print $ cksum content
          