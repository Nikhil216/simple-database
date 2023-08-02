{-# LANGUAGE OverloadedStrings #-}

module Client (main) where

import qualified Control.Exception as E
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan as Chan
import Control.Monad (unless, forever)
import qualified Data.ByteString as BS
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

data ConsoleLoopAction = Close | Continue
    deriving (Show, Eq)

main :: IO ()
main = do
    netChan <- Chan.newChan
    conChan <- Chan.newChan
    _ <- forkIO (networkLoop netChan conChan)
    BS.putStr "simple-db-client started\n"
    consoleLoop netChan conChan

consoleLoop :: Chan.Chan BS.ByteString -> Chan.Chan BS.ByteString -> IO ()
consoleLoop netChan conChan = do
    inputStr <- BS.getLine
    action <- runConsole netChan conChan (BS.init inputStr) -- getting rid of trailing \r
    unless (action == Close) $ do
        consoleLoop netChan conChan

runConsole :: Chan.Chan BS.ByteString -> Chan.Chan BS.ByteString -> BS.ByteString -> IO ConsoleLoopAction
runConsole netChan conChan inputStr = case inputStr of
    "" -> return Continue
    "quit" -> return Close
    cmd -> do
        Chan.writeChan netChan cmd
        msg <- Chan.readChan conChan
        BS.putStr $ msg <> "\n"
        return Continue

runNetwork :: (Socket -> IO a) -> IO a
runNetwork = runTCPClient "127.0.0.1" "3000"

networkLoop :: Chan.Chan BS.ByteString -> Chan.Chan BS.ByteString -> IO ()
networkLoop netChan conChan = runNetwork $ \s -> forever $ do
    cmd <- Chan.readChan netChan
    sendAll s cmd
    msg <- recv s 2048
    Chan.writeChan conChan msg
    
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