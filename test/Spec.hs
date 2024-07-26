{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Digest.CRC32      as CRC32
import qualified Storage                as S
import           Test.HUnit

main :: IO Counts
main = runTestTT testSuite

testSuite :: Test
testSuite = TestList [ testValueStore
                     , testKeyStore
                     , testKeyValueStore
                     , testCRC
                     ]

testValueStore :: Test
testValueStore = TestList [ TestCase (assertEqual "testValueStore - Append value store with given value"
                                        (S.appendVS " world" (S.initVS "hello"))
                                        (S.initVS "hello world", S.Cursor 5 6))
                          , TestCase (assertEqual "testValueStore - Read value store with given cursor"
                                        (S.readVS (S.Cursor 6 6) (S.initVS "hello world"))
                                        (S.initVS "hello world", "world"))
                          ]

testKeyStore :: Test
testKeyStore = TestList [ TestCase (assertEqual "testKeyStore - Insert key into the key store"
                                      (S.insertKS "pizza" (S.Cursor 5 6) S.emptyKS)
                                      (S.initKS [("pizza", S.Cursor 5 6)]))
                        , TestCase (assertEqual "testKeyStore - Read cursor store from key store"
                                      (S.lookupKS "pizza" (S.initKS [("pizza", S.Cursor 5 6)]))
                                      (Just (S.Cursor 5 6)))
                        ]

testKeyValueStore :: Test
testKeyValueStore = let key = "1" :: BS.ByteString
                        value = "One" :: BS.ByteString
                        store0 = S.emptyKVStore
                        (_, store1) = S.setKVStore key value store0
                        (maybeValue, _) = S.getKVStore key store1
                    in  TestList [ TestCase (assertEqual "testKVStore - Setting and the getting the same value from the store"
                                                (Just value)
                                                maybeValue)]

-- | https://crccalc.com/?crc=123456789&method=crc32&datatype=ascii&outtype=0
testCRC :: Test
testCRC = TestList [ TestCase (assertEqual "AIXM CRC check"         (aixm msg)      0x3010bf7f)
                   , TestCase (assertEqual "AUTOSAR CRC check"      (autosar msg)   0x1697d06a)
                   , TestCase (assertEqual "BASE91-D CRC check"     (base91d msg)   0x87315576)
                   , TestCase (assertEqual "BZIP2 CRC check"        (bzip2 msg)     0xfc891918)
                   , TestCase (assertEqual "CD-ROM-EDC CRC check"   (cdromedc msg)  0x6ec2edc4)
                   , TestCase (assertEqual "CKSUM CRC check"        (cksum msg)     0x765e7680)
                   , TestCase (assertEqual "ISCSI CRC check"        (iscsi msg)     0xe3069283)
                   , TestCase (assertEqual "ISO-HDLC CRC check"     (isohdlc msg)   0xcbf43926)
                   , TestCase (assertEqual "JAMCRC check"           (jamcrc msg)    0x340bc6d9)
                   , TestCase (assertEqual "MEF CRC check"          (mef msg)       0xd2c22f51)
                   , TestCase (assertEqual "MPEG2 CRC check"        (mpeg2 msg)     0x0376e6e7)
                   , TestCase (assertEqual "XFER CRC check"         (xfer msg)      0xbd0be338)
                   ]
    where msg       = "123456789" :: BL.ByteString
          aixm      = CRC32.crc32 0x814141ab 0x00000000 False False 0x00000000
          autosar   = CRC32.crc32 0xf4acfb13 0xffffffff True True 0xffffffff
          base91d   = CRC32.crc32 0xa833982b 0xffffffff True True 0xffffffff
          bzip2     = CRC32.crc32 0x04c11db7 0xffffffff False False 0xffffffff
          cdromedc  = CRC32.crc32 0x8001801b 0x00000000 True True 0x00000000
          cksum     = CRC32.crc32 0x04c11db7 0x00000000 False False 0xffffffff
          iscsi     = CRC32.crc32 0x1edc6f41 0xffffffff True True 0xffffffff
          isohdlc   = CRC32.crc32 0x04c11db7 0xffffffff True True 0xffffffff
          jamcrc    = CRC32.crc32 0x04c11db7 0xffffffff True True 0x00000000
          mef       = CRC32.crc32 0x741b8cd7 0xffffffff True True 0x00000000
          mpeg2     = CRC32.crc32 0x04c11db7 0xffffffff False False 0x00000000
          xfer      = CRC32.crc32 0x000000af 0x00000000 False False 0x00000000
