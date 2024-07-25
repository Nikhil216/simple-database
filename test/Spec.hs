{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import qualified Storage         as S
import           Test.HUnit

main :: IO Counts
main = runTestTT testSuite

testSuite :: Test
testSuite = TestList [ testValueStore
                     , testKeyStore
                     , testKeyValueStore
                     ]

testValueStore :: Test
testValueStore = TestList [ TestCase (assertEqual "testValueStore - Append value store with given value"
                                        (S.appendVS " world" (S.initVS "hello"))
                                        ((S.initVS "hello world", S.Cursor 5 6)))
                          , TestCase (assertEqual "testValueStore - Read value store with given cursor"
                                        (S.readVS (S.Cursor 6 6) (S.initVS "hello world"))
                                        ((S.initVS "hello world", "world")))
                          ]
                          
testKeyStore :: Test
testKeyStore = TestList [ TestCase (assertEqual "testKeyStore - Insert key into the key store"
                                      (S.insertKS "pizza" (S.Cursor 5 6) (S.emptyKS))
                                      ((S.initKS [("pizza", S.Cursor 5 6)])))
                        , TestCase (assertEqual "testKeyStore - Read cursor store from key store"
                                      (S.lookupKS "pizza" (S.initKS [("pizza", S.Cursor 5 6)]))
                                      (Just (S.Cursor 5 6)))
                        ]

testKeyValueStore :: Test
testKeyValueStore = let key = "1" :: BS.ByteString
                        value = "One" :: BS.ByteString
                        store0 = S.emptyKVStore
                        (_, store1) = S.setKVStore key value store0
                        (maybeValue, store2) = S.getKVStore key store1
                    in  TestList [ TestCase (assertEqual "testKVStore - Setting and the getting the same value from the store"
                                                (Just value)
                                                maybeValue)]
