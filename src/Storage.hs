module Storage
    ( Key
    , Value
    , Cursor (..)
    , KeyStore
    , ValueStore
    , KVStore
    , KVStorageAction (..)
    , emptyVS
    , initVS
    , appendVS
    , readVS
    , emptyKS
    , initKS
    , insertKS
    , lookupKS
    , emptyKVStore
    , initKVStore
    , setKVStore
    , getKVStore
    ) where

import qualified Data.Map as Map
import qualified Data.ByteString as BS

type Key = BS.ByteString
type Value = BS.ByteString
data Cursor = Cursor { getOffset :: Int
                     , getLength :: Int
                     }
              deriving (Show, Eq)
type KeyStore = Map.Map Key Cursor
type ValueStore = BS.ByteString
type KVStore = (KeyStore, ValueStore)
data KVStorageAction = SetKVStorage Key Value
                     | GetKVStorage Key

-- Value Store

emptyVS :: ValueStore
emptyVS = mempty

initVS :: BS.ByteString -> ValueStore
initVS value = value

appendVS :: Value -> ValueStore -> (ValueStore, Cursor)
appendVS value vs = (newVS, cursor)
    where vsLength = BS.length vs
          valueLength = BS.length value
          cursor = Cursor { getOffset = vsLength
                          , getLength = valueLength
                          }
          newVS = vs <> value

readVS :: Cursor -> ValueStore -> (ValueStore, Value)
readVS cursor vs = (vs, value)
    where offset = getOffset cursor
          len = getLength cursor
          value = BS.take len (BS.drop offset vs)

-- Key Store

emptyKS :: KeyStore
emptyKS = Map.empty

initKS :: [(Key, Cursor)] -> KeyStore
initKS = Map.fromList

insertKS :: Key -> Cursor -> KeyStore -> KeyStore
insertKS = Map.insert

lookupKS :: Key -> KeyStore -> Maybe Cursor
lookupKS = Map.lookup

-- Key Value Pair Store

emptyKVStore :: KVStore
emptyKVStore = (emptyKS, emptyVS)

initKVStore :: [(Key, Cursor)] -> BS.ByteString -> KVStore
initKVStore keyPairs value = (initKS keyPairs, initVS value)

setKVStore :: Key -> Value -> KVStore -> (Key, KVStore)
setKVStore key value (ks, vs) = (key, newStore)
    where (newVS, cursor) = appendVS value vs
          newKS = insertKS key cursor ks
          newStore = (newKS, newVS)

getKVStore :: Key -> KVStore -> (Maybe Value, KVStore)
getKVStore key store@(ks, vs) = let maybeCursor = lookupKS key ks
                                    maybeValue = case maybeCursor of
                                                    Nothing -> Nothing
                                                    Just cursor -> Just . snd . readVS cursor $ vs
                                in  (maybeValue, store)
