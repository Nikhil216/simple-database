{-# OPTIONS_GHC -Wno-unused-top-binds #-}


module Database.Bitcask.Engine
    ( BitcaskHandle
    , Session (..)
    , open
    , sync
    , close
    , listKeys
    , put
    , get
    ) where

import System.IO
import System.Directory
import System.IO.Error (catchIOError)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Digest.CRC32 (crc32)
import qualified Data.List                 as L
import qualified Data.Word                 as W
import qualified Data.ByteString.Lazy      as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Builder   as B
import qualified Data.Map.Strict           as M

-- data DataEntry = DataEntry { dCRC    :: W.Word32
--                            , dTStamp :: W.Word64
--                            , dKSize  :: W.Word32
--                            , dVSize  :: W.Word32
--                            , dKey    :: BL.ByteString
--                            , dVal    :: BL.ByteString
--                            }

data KeyEntry = KeyEntry   { kFileId :: W.Word16
                           , kVSize  :: W.Word32
                           , kVPos   :: W.Word32
                           , kTStamp :: W.Word64
                           }

type Keydir = M.Map BL.ByteString KeyEntry

type BitcaskHandle = (Integer, Handle, Keydir)

newtype Session a = Session { run :: IO (Either IOError a) }

instance Functor Session where
    fmap f (Session x) = Session ((fmap . fmap) f x)

instance Applicative Session where
    pure x = Session $ (pure . pure) x
    Session f <*> Session x = Session ((<*>) <$> f <*> x)

getDataFilePath :: Integer -> FilePath
getDataFilePath fileId = padLeft 3 '0' (show fileId) <> ".data"
    where padLeft sz char str = go sz char (length str) str
          go sz char len str = if len < sz
                                   then go sz char (len + 1) (char:str)
                                   else str

cksum :: BL.ByteString -> W.Word32
cksum = crc32 0x04c11db7 0x00000000 False False 0xffffffff

cksumBuild :: B.Builder -> W.Word32
cksumBuild = cksum . B.toLazyByteString

builderDE :: W.Word64 -> W.Word32 -> W.Word32 -> BL.ByteString -> BL.ByteString -> B.Builder
builderDE tstamp ksz vsz key val = mconcat [ B.word64BE tstamp
                                           , B.word32BE ksz
                                           , B.word32BE vsz
                                           , B.lazyByteString key
                                           , B.lazyByteString val
                                           ]

catch :: IO (Either IOError a) -> IO (Either IOError a)
catch io = catchIOError io (pure . Left)

open :: FilePath -> IO (Either IOError BitcaskHandle)
open dirname = catch $ do createDirectoryIfMissing False dirname
                          setCurrentDirectory dirname
                          files <- listDirectory dirname
                          let base = L.dropWhileEnd (/= '.')
                              max' = L.foldl' max 1 . map (read . base)
                              maxId = max' files :: Integer
                              activeFileId = maxId + 1
                              filename = getDataFilePath activeFileId
                              filePath = dirname <> "\\" <> filename
                              keydir = M.empty
                          handle <- openFile filePath ReadWriteMode
                          pure $ if activeFileId > 999
                                     then Left $ userError "Error: Max file limit reached"
                                     else Right (activeFileId, handle, keydir)

sync :: BitcaskHandle -> IO (Either IOError ())
sync (_, handle, _) = catch $ Right <$> hFlush handle

close :: BitcaskHandle -> IO (Either IOError ())
close (_, handle, _) = catch $ do hFlush handle
                                  Right <$> hClose handle

put :: BL.ByteString -> BL.ByteString -> BitcaskHandle -> IO (Either IOError BitcaskHandle)
put key val (fileId, handle, keydir) = catch $ do tstamp <- round . (* 1000) <$> getPOSIXTime
                                                  hpos   <- hFileSize handle
                                                  let ksz = fromIntegral . BL.length $ key
                                                      vsz = fromIntegral . BL.length $ val
                                                      bld = builderDE tstamp ksz vsz key val
                                                      cks = cksumBuild bld
                                                      str = B.toLazyByteString $ B.word32BE cks <> bld
                                                      vps = fromIntegral $ fromIntegral hpos
                                                                         + BL.length str
                                                                         - fromIntegral vsz
                                                      ken = KeyEntry (fromIntegral fileId) vsz vps tstamp
                                                  update key str ken (fileId, handle, keydir)

get :: BL.ByteString -> BitcaskHandle -> IO (Either IOError BL.ByteString)
get key (fileId, handle, keydir) = catch $ do let msg = "KeyError: Could not find key: "
                                                      <> BLU.toString key
                                              case M.lookup key keydir of
                                                  Nothing -> pure . Left . userError $ msg
                                                  Just ke -> fetchValue ke (fileId, handle, keydir)

listKeys :: BitcaskHandle -> IO (Either IOError [BL.ByteString])
listKeys (_, _, keydir) = pure . Right . M.keys $ keydir

update :: BL.ByteString -> BL.ByteString -> KeyEntry -> BitcaskHandle -> IO (Either IOError BitcaskHandle)
update key str keyEntry (fileId, handle, keydir) = catch $ do rest <- BL.hPutNonBlocking handle str
                                                              hsz  <- hFileSize handle
                                                              let keydir' = M.insert key keyEntry keydir
                                                                  msg     = "WriteError: Could not write to disk: "
                                                                          <> BLU.toString key
                                                                          <> " => "
                                                                          <> BLU.toString str
                                                              case BL.length rest of
                                                                  0 -> pure $ Right (fileId, handle, keydir')
                                                                  _ -> do hSetFileSize handle hsz
                                                                          hSeek handle SeekFromEnd 0
                                                                          pure . Left $ userError msg

fetchValue :: KeyEntry -> BitcaskHandle -> IO (Either IOError BL.ByteString)
fetchValue ke (fileId, handle, _) = catch $ do let vsize = fromIntegral $ kVSize ke
                                                   vpos  = fromIntegral $ kVPos ke
                                                   fId   = fromIntegral $ kFileId ke
                                                   fPath = getDataFilePath fId
                                                   msg   = "ReadError: Could not read from disk: "
                                                         <> "file: "
                                                         <> getDataFilePath fId
                                                         <> ", position: "
                                                         <> show vpos
                                                         <> ", size: "
                                                         <> show vsize
                                               hld <- if fId == fileId
                                                          then pure handle
                                                          else openFile fPath ReadMode
                                               hSeek hld AbsoluteSeek vpos
                                               str <- BL.hGetNonBlocking hld vsize
                                               if BL.length str == fromIntegral vsize
                                                   then pure . Right $ str
                                                   else pure . Left $ userError msg
