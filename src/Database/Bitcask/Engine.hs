{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Database.Bitcask.Engine
    ( BitcaskHandle
    , Session
    , runSession
    , open
    , sync
    , close
    , put
    , get
    , delete
    , listKeys
    , merge
    ) where

import           System.IO
import           Control.Monad                 (foldM_, when)
import           Control.DeepSeq               (force)
import           Data.Digest.CRC32             (crc32)
import           Data.Time.Clock.POSIX         (getPOSIXTime)
import           Control.Monad.Trans.Except    (ExceptT(..), runExceptT)
import           Data.Bifunctor                (Bifunctor(..))
import qualified System.Directory              as D
import qualified Data.Binary.Get               as G
import qualified Data.List                     as L
import qualified Data.Word                     as W
import qualified Data.ByteString.Lazy.UTF8     as BLU
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

type BitcaskHandle = (FilePath, W.Word16, Handle, Keydir)

type Session a = ExceptT IOError IO a

runSession :: Session a -> IO (Either IOError a)
runSession = runExceptT

fileSizeLimit :: Integral a => a
fileSizeLimit = 2 ^ (27 :: Integer) -- 128 MiB

open :: FilePath -> Session BitcaskHandle
open dirname = do _          <- mkdir dirname
                  files      <- readDataFiles dirname
                  hints      <- readHintFiles dirname
                  let ids    = map fst files
                      keydir = loadKeydir files hints
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
    if size > fileSizeLimit
        then do
            hFlush handle
            hClose handle
            fnames <- D.listDirectory dirname
            let ids = map filename2id fnames
            runExceptT $ do (fId, hdl) <- getActiveHandle dirname ids
                            update 0 (dirname, fId, hdl, keydir)
        else
            runExceptT $ update size (dirname, fileId, handle, keydir)

get :: BL.ByteString -> BitcaskHandle -> Session (Maybe BL.ByteString)
get key (dirname, fileId, handle, keydir) = do
    case M.lookup key keydir of
        Nothing -> pure Nothing
        Just ke -> do value <- fetchValue ke (dirname, fileId, handle, keydir)
                      return $ filterMaybe (isTombstone fileId) value

delete :: BL.ByteString -> BitcaskHandle -> Session BitcaskHandle
delete key hdl@(_, fId, _, _) = put key (tombstone fId) hdl

listKeys :: BitcaskHandle -> Session [BL.ByteString]
listKeys (_, _, _, keydir) = ExceptT . pure . Right . M.keys $ keydir

merge :: FilePath -> Session ()
merge dirname = do files  <- readDataFiles dirname
                   let ids       = fmap fst files
                       rows      = compactRows . createRows $ files
                       strs      = writeDataEntries rows
                       dataFiles = fmap ((dirname \\) . getDataFilePath) ids
                       hintFiles = fmap ((dirname \\) . getHintFilePath) ids
                   lift . mapM_ removeFileIfExists $ dataFiles <> hintFiles
                   foldM_ write [] strs
    where removeFileIfExists fp = (`when` D.removeFile fp) =<< D.doesFileExist fp
          lift = ExceptT . fmap pure
          hintFilePath dir fileId = dir \\ getHintFilePath fileId
          write ids (d, h) = do (fId, hdl) <- getActiveHandle dirname ids
                                lift $ do BL.hPut hdl d
                                          hFlush hdl
                                          hClose hdl
                                          BL.writeFile (hintFilePath dirname fId) h
                                          return (fId:ids)

mkdir :: FilePath -> Session Bool
mkdir dirname = catchLift $ do
    exists <- D.doesDirectoryExist dirname
    D.createDirectoryIfMissing False dirname
    pure . Right $ not exists
    where catchLift = ExceptT . flip E.catchIOError handle
          handle err = if E.isDoesNotExistError err
                           then pure . Left $ userError msg
                           else E.ioError err
          msg = "DoesNotExists: The parent directory of "
              <> dirname
              <> " does not exists"

readDataFiles :: FilePath -> Session [(W.Word16, BL.ByteString)]
readDataFiles = readFiles isDataFile

readHintFiles :: FilePath -> Session [(W.Word16, BL.ByteString)]
readHintFiles = readFiles isHintFile

readFiles :: (FilePath -> Bool) -> FilePath -> Session [(W.Word16, BL.ByteString)]
readFiles isFile dirname = catchLift $ do
    fnames   <- filter isFile <$> D.listDirectory dirname
    contents <- mapM (BL.readFile . (dirname \\)) fnames
    lift . sort . filter_ $ zip (map filename2id fnames) contents
    where notEmpty x = BL.length (snd x) /= 0
          lift       = pure . Right
          sort       = L.sortBy cmpFst
          cmpFst l r = compare (fst l) (fst r)
          filter_    = filter notEmpty
          catchLift  = ExceptT . flip E.catchIOError handle
          handle err = if E.isAlreadyInUseError err
                           then pure . Left $ userError msg
                           else E.ioError err
          msg = "Data file is already active. Please close the"
              <> " running BitcaskHandle or close the file handle"

getActiveHandle :: String -> [W.Word16] -> Session (W.Word16, Handle)
getActiveHandle dirname ids = ExceptT $ do
    hdl <- openFile fpath ReadWriteMode
    pure . Right $ (fId, hdl)
    where fId   = L.foldl max 0 ids + 1
          fname = getDataFilePath fId
          fpath = dirname \\ fname

parseDataFile :: BL.ByteString -> [DataEntry]
parseDataFile = parseFile getDataEntry

parseHintFile :: BL.ByteString -> Keydir
parseHintFile = M.fromList . parseFile getKeydirPair

parseFile :: G.Get a -> BL.ByteString -> [a]
parseFile g = go decoder
    where decoder = G.runGetIncremental g
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

-- | Note: Since the fileId is set to 0, caller should repair 
--         the fileId.
getKeydirPair :: G.Get (BL.ByteString, KeyEntry)
getKeydirPair = do tstamp <- G.getWord64be
                   ksz    <- G.getWord32be
                   vsz    <- G.getWord32be
                   vpos   <- G.getWord32be
                   key    <- G.getLazyByteString (fromIntegral ksz)
                   return (key, KeyEntry 0 vsz vpos tstamp)

fetchValue :: KeyEntry -> BitcaskHandle -> Session BL.ByteString
fetchValue ke (dirname, fileId, handle, _) = ExceptT $ do
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
          fId   = kFileId ke
          fPath = dirname \\ getDataFilePath fId
          msg   = "ReadError: Could not read from disk: "
                <> "file: " <> getDataFilePath fId
                <> ", position: " <> show vpos
                <> ", size: " <> show vsize

updateValue :: BL.ByteString -> BL.ByteString -> W.Word64 -> Integer -> BitcaskHandle -> Session BitcaskHandle
updateValue key val tstamp size (dirname, fileId, handle, keydir) = ExceptT $ do
    BL.hPut handle row
    pure $ Right (dirname, fileId, handle, kd')
    where !ksz = fromIntegral . BL.length $ key
          !vsz = fromIntegral . BL.length $ val
          !bld = builderDE tstamp ksz vsz key val
          !cks = cksumBuild bld
          !row = B.toLazyByteString $ B.word32BE cks <> bld
          !vps = fromIntegral $ fromIntegral size
                              + BL.length row
                              - fromIntegral vsz
          !ken = KeyEntry fileId vsz vps tstamp
          !kd' = insertKD key ken keydir

writeDataEntries :: [DataEntry] -> [(BL.ByteString, BL.ByteString)]
writeDataEntries = fmap (bimap B.toLazyByteString B.toLazyByteString . dropFst) . L.foldl' go []
    where go []               de = [(size de, buildD de, buildH de 0)]
          go ((fsz, d, h):xs) de =
              if fsz < fileSizeLimit
                   then (fsz + size de, d <> buildD de, h <> buildH de fsz) : xs
                   else (size de, buildD de, buildH de fsz) : (fsz, d, h) : xs
          dropFst (_, x, y) = (x, y)
          size   (DataEntry _   _      ksz vsz _   _  ) = 4 + 8 + 4 + 4 + ksz + vsz
          pos    (DataEntry _   _      ksz _   _   _  ) = 4 + 8 + 4 + 4 + ksz
          buildD (DataEntry crc tstamp ksz vsz key val) = B.word32BE crc
                                                        <> builderDE tstamp ksz vsz key val
          buildH de@(DataEntry _ tstamp ksz vsz key _) fsz = mconcat [ B.word64BE tstamp
                                                                     , B.word32BE ksz
                                                                     , B.word32BE vsz
                                                                     , B.word32BE (fsz + pos de)
                                                                     , B.lazyByteString key
                                                                     ]

-- | filter out the stale values and get rid of tombstones
compactRows :: [(W.Word16, [DataEntry])] -> [DataEntry]
compactRows = fmap snd . M.elems . M.filter chkTombstone . mkMap . concat . distribute
    where distribute = fmap (\(fId, rs) -> fmap (fId,) rs)
          chkTombstone (fId, de) = not $ isTombstone fId (dVal de)
          mkMap = L.foldl' go  M.empty
          go acc (fId, de) = M.insertWith latest key (fId, de) acc
              where latest = geMap (dTStamp . snd)
                    key    = dKey de

loadKeydir :: [(W.Word16, BL.ByteString)] -> [(W.Word16, BL.ByteString)] -> Keydir
loadKeydir files hints = L.foldl' append M.empty keydirs
    where keydirs     = kdFromData : kdsFromHint
          kdFromData  = createKeydir . createRows . filter noHint $ files
          kdsFromHint = fmap load hints
          noHint      = not . flip L.elem hintIds . fst
          hintIds     = fmap fst hints
          append      = M.foldlWithKey' insert
          load (fId, str)   = M.map (update fId) (parseHintFile str)
          update fId ken    = ken { kFileId = fId }
          insert kd key ken = insertKD key ken kd

createKeydir :: [(W.Word16, [DataEntry])] -> Keydir
createKeydir = mkKeydir . distribute
    where distribute       = fmap (\(fId, rs) -> map (fId,) rs)
          mkKeydir         = L.foldl' mkKeyEntry M.empty
          mkKeyEntry kd rs = fst $ L.foldl' go (kd, 0) rs
          go (kd, pos) (fId, r) = (kd', pos')
              -- adding bytes taken by DataEntry
              where vpos  = pos + 4 + 8 + 4 + 4 + dKSize r
                    !ken  = KeyEntry fId (dVSize r) vpos (dTStamp r)
                    !pos' = vpos + dVSize r
                    !kd'  = insertKD (dKey r) ken kd

createRows :: [(W.Word16, BL.ByteString)] -> [(W.Word16, [DataEntry])]
createRows = fmap (second (checkRows . parseDataFile))
    where checkRows = filter validate

validate :: DataEntry -> Bool
validate (DataEntry crc tsp ksz vsz key val) = crc == cksumBuild (builderDE tsp ksz vsz key val)

getDataFilePath :: W.Word16 -> FilePath
getDataFilePath fileId = showFileId fileId <> ".data"

getHintFilePath :: W.Word16 -> FilePath
getHintFilePath fileId = showFileId fileId <> ".hint"

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
--   Ex: "0001.data" -> 1
filename2id :: String -> W.Word16
filename2id = read . L.takeWhile (/= '.')

-- | Path seperator (Windows)
(\\) :: FilePath -> FilePath -> FilePath
parent \\ child = parent <> "\\" <> child

geMap :: Ord a => (t -> a) -> t -> t -> t
geMap fn l r = if fn l >= fn r
                   then l
                   else r

-- | tombstone value
tombstone :: W.Word16 -> BL.ByteString
tombstone fileId = BLU.fromString $ "bitcask_tombstone" <> showFileId fileId

showFileId :: W.Word16 -> String
showFileId fileId = padLeft 4 '0' (show fileId)

padLeft :: Int -> Char -> String -> String
padLeft size char str = go size char (length str) str
    where go sz c len s = if len < sz
                              then go sz char (len + 1) (c:s)
                              else s

-- | Check if the given value is the tombstone value
isTombstone :: W.Word16 -> BL.ByteString -> Bool
isTombstone fId val = val == tombstone fId

-- | Check if the file ends in extension ".data"
isDataFile :: FilePath -> Bool
isDataFile = (".data" ==) . L.dropWhile (/= '.')

-- | Check if the file ends in extension ".hint"
isHintFile :: FilePath -> Bool
isHintFile = (".hint" ==) . L.dropWhile (/= '.')

-- | insert keyentry into key dir
--   if the keys are same then the keyentry
--   with the later timestamp gets inserted
insertKD :: BL.ByteString -> KeyEntry -> Keydir -> Keydir
insertKD = M.insertWith latest
    where latest = geMap kTStamp

filterMaybe :: (a -> Bool) -> a -> Maybe a
filterMaybe p a
    | p a       = Just a
    | otherwise = Nothing
