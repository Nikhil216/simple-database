module Storage
    ( Key
    , Value
    , Cursor (..)
    , KeyStore
    , ValueStore
    , empty_vs
    , init_vs
    , append_vs
    , read_vs
    , empty_ks
    , init_ks
    , insert_ks
    , lookup_ks
    , empty_store
    , init_store
    , set_store
    , get_store
    ) where

import qualified Data.Map as Map
import qualified Data.ByteString.UTF8 as BS

type Key = BS.ByteString
type Value = BS.ByteString
data Cursor = Cursor { get_offset :: Int
                     , get_length :: Int
                     }
              deriving (Show, Eq)
type KeyStore = Map.Map Key Cursor
type ValueStore = BS.ByteString
type Store = (KeyStore, ValueStore)

-- Value Store

empty_vs :: ValueStore
empty_vs = mempty

init_vs :: BS.ByteString -> ValueStore
init_vs value = value

append_vs :: Value -> ValueStore -> (ValueStore, Cursor)
append_vs value vs = (new_vs, cursor)
    where vs_length = BS.length vs
          value_length = BS.length value
          cursor = Cursor { get_offset = vs_length
                          , get_length = value_length
                          }
          new_vs = vs <> value

read_vs :: Cursor -> ValueStore -> (ValueStore, Value)
read_vs cursor vs = (vs, value)
    where offset = get_offset cursor
          len = get_length cursor
          value = BS.take len (BS.drop offset vs)

-- Key Store

empty_ks :: KeyStore
empty_ks = Map.empty

init_ks :: [(Key, Cursor)] -> KeyStore
init_ks = Map.fromList

insert_ks :: Key -> Cursor -> KeyStore -> KeyStore
insert_ks = Map.insert

lookup_ks :: Key -> KeyStore -> Maybe Cursor
lookup_ks = Map.lookup

-- Store

empty_store :: Store
empty_store = (empty_ks, empty_vs)

init_store :: [(Key, Cursor)] -> BS.ByteString -> Store
init_store key_pairs value = (init_ks key_pairs, init_vs value)

set_store :: Key -> Value -> Store -> Store
set_store key value (ks, vs) = new_store
    where (new_vs, cursor) = append_vs value vs
          new_ks = insert_ks key cursor ks
          new_store = (new_ks, new_vs)

get_store :: Key -> Store -> (Store, Maybe Value)
get_store key store@(ks, vs) = let maybe_cursor = lookup_ks key ks
                                   maybe_value = case maybe_cursor of
                                                    Nothing -> Nothing
                                                    Just cursor -> Just . snd . read_vs cursor $ vs
                               in  (store, maybe_value)
