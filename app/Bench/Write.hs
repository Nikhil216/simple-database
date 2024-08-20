module Write (main) where

import Prelude hiding (readFile)
import Database.Bitcask.Engine
import Data.ByteString.Lazy (ByteString, readFile, foldlChunks, fromStrict)
import Data.ByteString.Lazy.UTF8 (fromString)
import Control.Monad (foldM)
import System.Environment (getArgs)

main :: IO ()
main = do
    dirname : filepath : _ <- getArgs
    bytes <- readFile filepath
    let kv = convert bytes
    result <- runSession $ do
        hdl <- open dirname
        hdl' <- write hdl kv
        close hdl'
    print result
    where convert = snd . foldlChunks go (0, [])
          go (i, xs) chunk = (i', (k, v) : xs)
            where i' = i + 1
                  k  = showBS i
                  v  = fromStrict chunk
          showBS :: Integer -> ByteString
          showBS = fromString . show
          write = foldM (\hdl (k, v) -> put k v hdl)