module Open (main) where

import Database.Bitcask.Engine
import System.Environment (getArgs)

main :: IO ()
main = do
    dirname : _ <- getArgs
    result <- runSession $ do
        hdl@(_, _, _, ke) <- open dirname
        close hdl
        return ke
    print result