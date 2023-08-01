import qualified Data.ByteString.UTF8 as BS
import qualified Storage as S
import Test.HUnit

main :: IO Counts
main = runTestTT testSuite

testSuite :: Test
testSuite = TestList [ testValueStore
                     , testKeyStore
                     , testStore
                     ]

testValueStore :: Test
testValueStore = TestList [ TestCase (assertEqual "testValueStore - Append value store with given value"
                                        (S.appendVS (BS.fromString " world") (S.initVS (BS.fromString "hello")))
                                        ((S.initVS (BS.fromString "hello world"), S.Cursor 5 6)))
                          , TestCase (assertEqual "testValueStore - Read value store with given cursor"
                                        (S.readVS (S.Cursor 6 6) (S.initVS (BS.fromString "hello world")))
                                        ((S.initVS (BS.fromString "hello world"), (BS.fromString "world"))))
                          ]
                          
testKeyStore :: Test
testKeyStore = TestList [ TestCase (assertEqual "testKeyStore - Insert key into the key store"
                                      (S.insertKS (BS.fromString "pizza") (S.Cursor 5 6) (S.emptyKS))
                                      ((S.initKS [((BS.fromString "pizza"), S.Cursor 5 6)])))
                        , TestCase (assertEqual "testKeyStore - Read cursor store from key store"
                                      (S.lookupKS (BS.fromString "pizza") (S.initKS [((BS.fromString "pizza"), S.Cursor 5 6)]))
                                      (Just (S.Cursor 5 6)))
                        ]

testStore :: Test
testStore = let key = BS.fromString "1"
                value = BS.fromString "One"
                store0 = S.emptyStore
                store1 = S.setStore key value store0
                (store2, maybeValue) = S.getStore key store1
            in  TestList [ TestCase (assertEqual "testStore - Setting and the getting the same value from the store"
                                        (Just value)
                                        maybeValue)]