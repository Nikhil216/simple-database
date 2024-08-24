{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Database.Bitcask.Engine
    ( BitcaskHandle
    , Session
    , runSession
    , open
    , sync
    , close
    , put
    , get
    , listKeys
    ) where

import           System.IO
import           Control.DeepSeq               (force)
import           Data.Digest.CRC32             (crc32)
import           System.Directory              (createDirectoryIfMissing, listDirectory, doesDirectoryExist)
import           Data.Time.Clock.POSIX         (getPOSIXTime)
import           Control.Monad.Trans.Except    (ExceptT(..), runExceptT)
import           Data.Bifunctor                (Bifunctor(..))
import qualified Data.Binary.Get               as G
import qualified Data.List                     as L
import qualified Data.Word                     as W
import qualified Data.ByteString.Lazy.Internal as BLI
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Builder       as B
import qualified Data.Map.Strict               as M
import qualified System.IO.Error               as E

data DataEntry = DataEntry { dCRC    :: W.Word32
                           , dTStamp :: W.Word64
                           , dKSize  :: W.Word32
                           , dVSize  :: W.Word32
                           , dKey    :: BL.ByteString
                           , dVal    :: BL.ByteString
                           } deriving (Show)

data KeyEntry = KeyEntry   { kFileId :: W.Word16
                           , kVSize  :: W.Word32
                           , kVPos   :: W.Word32
                           , kTStamp :: W.Word64
                           } deriving (Show)

type Keydir = M.Map BL.ByteString KeyEntry

type BitcaskHandle = (FilePath, Integer, Handle, Keydir)

type Session a = ExceptT IOError IO a

runSession :: Session a -> IO (Either IOError a)
runSession = runExceptT

open :: FilePath -> Session BitcaskHandle
open dirname = do _          <- mkdir dirname
                  files      <- readDataFiles dirname
                  keydir     <- createKeydir files
                  let ids     = map fst files
                  (fId, hdl) <- getActiveHandle dirname ids
                  return (dirname, fId, hdl, keydir)

sync :: BitcaskHandle -> Session ()
sync (_, _, handle, _) = ExceptT $ Right <$> hFlush handle

close :: BitcaskHandle -> Session ()
close (_, _, handle, _) = ExceptT $ Right <$> do
    hFlush handle
    hClose handle

put :: BL.ByteString -> BL.ByteString -> BitcaskHandle -> Session BitcaskHandle
put key val (dirname, fileId, handle, keydir) = ExceptT $ do
    tstamp <- round . (* 1000) . force <$> getPOSIXTime
    size   <- hFileSize handle
    let update = updateValue key val tstamp
    if size > 2 ^ (27 :: Integer) -- 128 MiB
        then do
            hFlush handle
            hClose handle
            fnames <- listDirectory dirname
            let ids = map filename2id fnames
            runExceptT $ do (fId, hdl) <- getActiveHandle dirname ids
                            update 0 (dirname, fId, hdl, keydir)
        else
            runExceptT $ update size (dirname, fileId, handle, keydir)

get :: BL.ByteString -> BitcaskHandle -> Session BL.ByteString
get key (dirname, fileId, handle, keydir) = ExceptT $ do
    case M.lookup key keydir of
        Nothing -> pure . Left . userError $ msg
        Just ke -> fetchValue ke (dirname, fileId, handle, keydir)
    where msg = "KeyError: Could not find key: " <> show key

listKeys :: BitcaskHandle -> Session [BL.ByteString]
listKeys (_, _, _, keydir) = ExceptT . pure . Right . M.keys $ keydir

mkdir :: FilePath -> Session Bool
mkdir dirname = catchLift $ do
    exists <- doesDirectoryExist dirname
    createDirectoryIfMissing False dirname
    pure . Right $ not exists
    where catchLift = ExceptT . flip E.catchIOError handle
          handle err = if E.isDoesNotExistError err
                           then pure . Left $ userError msg
                           else E.ioError err
          msg = "DoesNotExists: The parent directory of "
              <> dirname
              <> " does not exists"

readDataFiles :: FilePath -> Session [(Integer, BL.ByteString)]
readDataFiles dirname = catchLift $ do
    fnames   <- listDirectory dirname
    contents <- mapM (BL.readFile . (dirname \\)) fnames
    filterLift $ zip (map filename2id fnames) contents
    where notEmpty x = BL.length (snd x) /= 0
          filterLift = pure . Right . filter notEmpty
          catchLift  = ExceptT . flip E.catchIOError handle
          handle err = if E.isAlreadyInUseError err
                           then pure . Left $ userError msg
                           else E.ioError err
          msg = "Data file is already active. Please close the"
              <> " running BitcaskHandle or close the file handle"

createKeydir :: [(Integer, BL.ByteString)] -> Session Keydir
createKeydir = lift . mkKeydir . distribute . mkRows
    where lift = ExceptT . pure . Right
          mkKeydir         = L.foldl' mkKeyEntry M.empty
          mkRows           = fmap (bimap fromIntegral parseDataFile)
          distribute       = fmap (\(fId, rs) -> map (fId,) (ckRows rs))
          ckRows           = filter validate
          mkKeyEntry kd rs = fst $ L.foldl' go (kd, 0) rs
          go (kd, pos) (fId, r) = (kd', pos')
              -- adding bytes taken by DataEntry
              where vpos  = pos + 4 + 8 + 4 + 4 + dKSize r
                    !keyE = KeyEntry fId (dVSize r) vpos (dTStamp r)
                    !pos' = vpos + dVSize r
                    !kd'  = M.insertWith latest (dKey r) keyE kd

getActiveHandle :: String -> [Integer] -> Session (Integer, Handle)
getActiveHandle dirname ids = ExceptT $ do
    hdl <- openFile fpath ReadWriteMode
    pure . Right $ (fId, hdl)
    where fId   = L.foldl max 0 ids + 1
          fname = getDataFilePath fId
          fpath = dirname \\ fname

parseDataFile :: BL.ByteString -> [DataEntry]
parseDataFile = go decoder
    where decoder = G.runGetIncremental getDataEntry
          go :: G.Decoder DataEntry -> BL.ByteString -> [DataEntry]
          go (G.Done rest _ entry) !input = case BLI.chunk rest input of
                                               BLI.Empty -> [entry]
                                               bs        -> entry : go decoder bs
          go (G.Partial k)         !input = go (k . takeHeadChunk $ input) (dropHeadChunk input)
          go (G.Fail _ _ msg)       _     = error msg
          takeHeadChunk (BLI.Chunk bs _)  = Just bs
          takeHeadChunk _                 = Nothing
          dropHeadChunk (BLI.Chunk _ lbs) = lbs
          dropHeadChunk _                 = BLI.Empty

getDataEntry :: G.Get DataEntry
getDataEntry = do crc    <- G.getWord32be
                  tstamp <- G.getWord64be
                  ksz    <- G.getWord32be
                  vsz    <- G.getWord32be
                  DataEntry crc tstamp ksz vsz
                      <$> G.getLazyByteString (fromIntegral ksz)
                      <*> G.getLazyByteString (fromIntegral vsz)

fetchValue :: KeyEntry -> BitcaskHandle -> IO (Either IOError BL.ByteString)
fetchValue ke (dirname, fileId, handle, _) = do
    hld <- if fId == fileId
               then pure handle
               else openFile fPath ReadMode
    hSeek hld AbsoluteSeek vpos
    str <- BL.hGetNonBlocking hld vsize
    if BL.length str == fromIntegral vsize
        then pure . Right $ str
        else pure . Left  $ userError msg
    where vsize = fromIntegral $ kVSize ke
          vpos  = fromIntegral $ kVPos ke
          fId   = fromIntegral $ kFileId ke
          fPath = dirname \\ getDataFilePath fId
          msg   = "ReadError: Could not read from disk: "
                <> "file: " <> getDataFilePath fId
                <> ", position: " <> show vpos
                <> ", size: " <> show vsize

updateValue :: BL.ByteString -> BL.ByteString -> W.Word64 -> Integer -> BitcaskHandle -> Session BitcaskHandle
updateValue key val  tstamp size (dirname, fileId, handle, keydir) = ExceptT $ do
    let !ksz = fromIntegral . BL.length $ key
        !vsz = fromIntegral . BL.length $ val
        !bld = builderDE tstamp ksz vsz key val
        !cks = cksumBuild bld
        !row = B.toLazyByteString $ B.word32BE cks <> bld
        !vps = fromIntegral $ fromIntegral size
                           + BL.length row
                           - fromIntegral vsz
        !ken = KeyEntry (fromIntegral fileId) vsz vps tstamp
        !kd' = M.insertWith latest key ken keydir
    BL.hPut handle row
    pure $ Right (dirname, fileId, handle, kd')

validate :: DataEntry -> Bool
validate (DataEntry crc tsp ksz vsz key val) = crc == cksumBuild (builderDE tsp ksz vsz key val)

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

-- | Convert a data file name into an id
--   Ex: "001.data" -> 1
filename2id :: String -> Integer
filename2id = read . L.takeWhile (/= '.')

-- | Path seperator (Windows)
(\\) :: FilePath -> FilePath -> FilePath
parent \\ child = parent <> "\\" <> child

-- | Return the latest key entry
latest :: KeyEntry -> KeyEntry -> KeyEntry
latest kel ker = if kTStamp ker > kTStamp kel
                     then ker
                     else kel