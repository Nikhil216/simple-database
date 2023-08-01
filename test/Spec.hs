import qualified Data.ByteString.UTF8 as BS
import qualified Storage as S
import Test.HUnit

main :: IO Counts
main = runTestTT testSuite

testSuite :: Test
testSuite = TestList [ testValueStore
                     , testKeyStore
                     ]

testValueStore :: Test
testValueStore = TestList [ TestCase (assertEqual "testValueStore - Append value store with given value"
                                        (S.append_vs (BS.fromString " world") (S.init_vs (BS.fromString "hello")))
                                        ((S.init_vs (BS.fromString "hello world"), S.Cursor 5 6)))
                          , TestCase (assertEqual "testValueStore - Read value store with given cursor"
                                        (S.read_vs (S.Cursor 6 6) (S.init_vs (BS.fromString "hello world")))
                                        ((S.init_vs (BS.fromString "hello world"), (BS.fromString "world"))))
                          ]
                          
testKeyStore :: Test
testKeyStore = TestList [ TestCase (assertEqual "testKeyStore - Insert key into the key store"
                                      (S.insert_ks (BS.fromString "pizza") (S.Cursor 5 6) (S.empty_ks))
                                      ((S.init_ks [((BS.fromString "pizza"), S.Cursor 5 6)])))
                        , TestCase (assertEqual "testKeyStore - Read cursor store from key store"
                                      (S.lookup_ks (BS.fromString "pizza") (S.init_ks [((BS.fromString "pizza"), S.Cursor 5 6)]))
                                      (Just (S.Cursor 5 6)))
                        ]

testStore :: Test
testStore = let key = BS.fromString "1"
                value = BS.fromString "One"
                store_0 = S.empty_store
                store_1 = S.set_store key value store_0
                (store_2, maybe_value) = S.get_store key store_1
            in  TestList [ TestCase (assertEqual "testStore - Setting and the getting the same value from the store"
                                        (Just value)
                                        maybe_value)]