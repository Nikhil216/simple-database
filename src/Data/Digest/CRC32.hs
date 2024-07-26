{-# LANGUAGE BangPatterns #-}


module Data.Digest.CRC32
    ( mul
    , genFunc
    , genTable
    , crc32
    ) where

import qualified Data.Bits            as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector.Unboxed  as VU
import qualified Data.Word            as W

emptyWord :: W.Word32
emptyWord = fromIntegral (0x000000 :: Integer)

-- | bit xor multiplication
mul :: W.Word32 -> W.Word32 -> W.Word32
mul a b = foldl B.xor emptyWord shifted
    where shifted = map (B.shiftL a) oneBits
          oneBits = filter (B.testBit b) [0..31]

-- | generator function for crc32 table
genFunc :: W.Word32 -> Bool -> Bool -> Int -> W.Word32
genFunc poly refIn refOut idx = rev32 refOut rem_
    where rem_ = foldl shiftXor ini polyList
          ini = fromIntegral (rev8 refIn (fromIntegral idx)) `B.shiftL` 24
          polyList = replicate 8 poly
          rev32 = choose W.bitReverse32
          rev8 = choose W.bitReverse8
          shiftXor b a = (b `B.shiftL` 1) `B.xor` (if B.testBit b 31 then a else emptyWord)
          choose fun flag num = if flag then fun num else num

-- | crc32 model table
genTable :: W.Word32 -> Bool -> Bool -> VU.Vector W.Word32
genTable poly refIn refOut = VU.generate 256 $ genFunc poly refIn refOut

-- | evaluate crc32
crc32 :: W.Word32 -> W.Word32 -> Bool -> Bool -> W.Word32 -> BL.ByteString -> W.Word32
crc32 poly init_ refIn refOut xorOut = B.xor xorOut . BL.foldl' rem_ init_
    where rem_ reg msgByte = shift reg `B.xor` fetch reg msgByte
          fetch reg byte = table VU.! fromIntegral (func reg byte)
          shift = if refOut then flip B.shiftR 8 else flip B.shiftL 8
          func = if refOut then xorLastByte else xorFirstByte
          xorFirstByte r = xorLastByte (r `B.shiftR` 24)
          xorLastByte r b = fromIntegral r `B.xor` b
          !table = genTable poly refIn refOut
