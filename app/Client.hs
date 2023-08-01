{-# LANGUAGE OverloadedStrings #-}

module Client (main) where

import qualified Control.Exception as E
import Control.Monad (unless)
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

data ConsoleLoopAction = Close | Continue
    deriving (Show, Eq)

main :: IO ()
main = consoleLoop

consoleLoop :: IO ()
consoleLoop = do
    inputStr <- getLine
    action <- runConsole inputStr
    unless (action == Close) $ do
        consoleLoop

runConsole :: String -> IO ConsoleLoopAction
runConsole inputStr = case inputStr of
    "" -> return Continue
    "quit" -> return Close
    "connect" -> do
        connectNetwork
        return Continue
    _ -> return Continue
    
connectNetwork :: IO ()
connectNetwork = runTCPClient "127.0.0.1" "3000" $ \s -> do
    sendAll s "Hello, world!"
    msg <- recv s 1024
    putStr "Received: "
    C.putStrLn msg

-- from the "network-run" package.
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock