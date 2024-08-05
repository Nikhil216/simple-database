{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString        as BS
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
testCRC = TestList [ crcTest aixm
                   , crcTest autosar
                   , crcTest base91d
                   , crcTest bzip2
                   , crcTest cdromedc
                   , crcTest cksum
                   , crcTest iscsi
                   , crcTest isohdlc
                   , crcTest jamcrc
                   , crcTest mef
                   , crcTest mpeg2
                   , crcTest xfer
                   ]
    where msg       = "123456789"
          aixm      = (CRC32.crc32 0x814141ab 0x00000000 False False 0x00000000, "AIXM CRC check", 0x3010bf7f)
          autosar   = (CRC32.crc32 0xf4acfb13 0xffffffff True True 0xffffffff, "AUTOSAR CRC check", 0x1697d06a)
          base91d   = (CRC32.crc32 0xa833982b 0xffffffff True True 0xffffffff, "BASE91-D CRC check", 0x87315576)
          bzip2     = (CRC32.crc32 0x04c11db7 0xffffffff False False 0xffffffff, "BZIP2 CRC check", 0xfc891918)
          cdromedc  = (CRC32.crc32 0x8001801b 0x00000000 True True 0x00000000, "CD-ROM-EDC CRC check", 0x6ec2edc4)
          cksum     = (CRC32.crc32 0x04c11db7 0x00000000 False False 0xffffffff, "CKSUM CRC check", 0x765e7680)
          iscsi     = (CRC32.crc32 0x1edc6f41 0xffffffff True True 0xffffffff, "ISCSI CRC check", 0xe3069283)
          isohdlc   = (CRC32.crc32 0x04c11db7 0xffffffff True True 0xffffffff, "ISO-HDLC CRC chekc", 0xcbf43926)
          jamcrc    = (CRC32.crc32 0x04c11db7 0xffffffff True True 0x00000000, "JAMCRC check", 0x340bc6d9)
          mef       = (CRC32.crc32 0x741b8cd7 0xffffffff True True 0x00000000, "MEF CRC check", 0xd2c22f51)
          mpeg2     = (CRC32.crc32 0x04c11db7 0xffffffff False False 0x00000000, "MPEG2 CRC check", 0x0376e6e7)
          xfer      = (CRC32.crc32 0x000000af 0x00000000 False False 0x00000000, "XFER CRC check", 0xbd0be338)
          crcTest (crc, text, hash) = TestCase (assertEqual text (crc msg) hash)
