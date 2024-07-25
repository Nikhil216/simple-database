module Data.Digest.CRC32
    ( crc32Poly
    , mul
    , genFunc
    , genTable
    ) where

import qualified Data.Bits           as B
import qualified Data.ByteString     as BS
import qualified Data.List           as L
import qualified Data.Vector.Unboxed as VU
import qualified Data.Word           as W

emptyWord :: W.Word32
emptyWord = fromIntegral (0x000000 :: Integer)

crc32Poly :: W.Word32
crc32Poly = fromIntegral (0x04c11db7 :: Integer)

-- | bit xor multiplication
mul :: W.Word32 -> W.Word32 -> W.Word32
mul a b = foldl B.xor emptyWord shifted
    where shifted = map (B.shiftL a) oneBits
          oneBits = filter (B.testBit b) [0..31]

-- | generator function for crc32 table
genFunc :: W.Word32 -> Bool -> Bool -> Int -> W.Word32
genFunc poly refIn refOut idx = rev32 refOut rem 
    where rem = foldl shiftXor ini polyList
          ini = fromIntegral (rev8 refIn (fromIntegral idx)) `B.shiftL` 24
          polyList = replicate 8 poly
          rev32 = choose W.bitReverse32
          rev8 = choose W.bitReverse8
          shiftXor b a = (b `B.shiftL` 1) `B.xor` (if B.testBit b 31
                                                       then a
                                                       else emptyWord)
          choose fun flag num = if flag 
                               then fun num
                               else num

-- | crc32 model table
genTable :: W.Word32 -> Bool -> Bool -> VU.Vector W.Word32
genTable poly refIn refOut = VU.generate 256 $ genFunc poly refIn refOut

crc32Table :: VU.Vector W.Word32
crc32Table = genTable crc32Poly True True
