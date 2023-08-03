{-# LANGUAGE OverloadedStrings #-}

module Server (main) where

import Control.Concurrent (forkFinally, forkIO)
import Control.Concurrent.Chan as Chan
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as BS
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Environment (getArgs)
import qualified Storage as Sto

data ServerLoopAction = Close | Continue
    deriving (Show, Eq)

main :: IO ()
main = do
    args <- getArgs
    netChan <- Chan.newChan
    stoChan <- Chan.newChan
    let port = head args
    putStrLn $ "Server listening on port: " <> port
    _ <- forkIO (execStorage netChan stoChan Sto.emptyStore)
    runServer netChan stoChan port

execCmd :: Chan.Chan BS.ByteString -> Chan.Chan Sto.StorageAction -> Socket -> BS.ByteString -> IO ServerLoopAction
execCmd netChan stoChan s cmdStr = do
    BS.putStr $ "Client: " <> cmdStr <> "\n"
    case parseCmd cmdStr of
        ("connect":_) -> do
            sendAll s "Connected to simple-db server"
            return Continue
        ("disconnect":_) -> do
            sendAll s "Closing connection..."
            close s
            return Continue
        ("set":key:value:_) -> do
            Chan.writeChan stoChan (Sto.SetStorage key value)
            msg <- Chan.readChan netChan
            sendAll s msg
            return Continue
        ("get":key:_) -> do
            Chan.writeChan stoChan (Sto.GetStorage key)
            msg <- Chan.readChan netChan
            sendAll s msg
            return Continue
        [] -> do
            sendAll s $ ""
            return Continue
        _ -> do
            sendAll s $ "Invalid command: " <> cmdStr
            return Continue

parseCmd :: BS.ByteString -> [BS.ByteString]
parseCmd = BS.split 32
 
execStorage :: Chan.Chan BS.ByteString -> Chan.Chan Sto.StorageAction -> Sto.Store -> IO a
execStorage netChan stoChan store = do
    op <- Chan.readChan stoChan
    (msg, newStore) <- case op of
        Sto.GetStorage key -> do
            let (maybeValue, newStore) = Sto.getStore key store
                msg = case maybeValue of
                    Nothing -> "Key is not present: " <> key
                    Just value -> value
            return (msg, newStore)
        Sto.SetStorage key value -> return (Sto.setStore key value store)
    Chan.writeChan netChan msg
    execStorage netChan stoChan newStore
    
runServer :: Chan.Chan BS.ByteString -> Chan.Chan Sto.StorageAction -> String -> IO () 
runServer netChan stoChan port = runTCPServer Nothing port talk
  where
    talk s = do
        cmd <- recv s 1024
        action <- execCmd netChan stoChan s cmd
        unless (action == Close) $ do
            talk s

-- from the "network-run" package.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
        let hints = defaultHints { addrFlags = [AI_PASSIVE]
                                 , addrSocketType = Stream
                                 }
        head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    loop sock = forever $ E.bracketOnError (accept sock) (close . fst)
        $ \(conn, _peer) -> void $
            -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
            -- but 'E.bracketOnError' above will be necessary if some
            -- non-atomic setups (e.g. spawning a subprocess to handle
            -- @conn@) before proper cleanup of @conn@ is your case
            forkFinally (server conn) (const $ gracefulClose conn 5000)