
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BinaryLiterals             #-}
-- {-# LANGUAGE NumericUnderscores #-}  -- ghc8.6 or later

module Data.GHex where

import           Data.Bits
import           Data.List   (intercalate)
import           Text.Printf (printf)
import           Debug.Trace (trace)

-- | Basic type
-- >>> 255 :: Hex
-- 0x0000_0000_0000_00ff
newtype Hex = Hex Word
              deriving (Eq, Ord, Num, Enum, Real, Bounded, Integral,
                        Bits, FiniteBits)


instance Show Hex where
    show = hex


----- Logical operations

-- | Bitwise "and"
-- >>> 0x1234 .& 0xff
-- 0x0000_0000_0000_0034
(.&) :: Hex -> Hex -> Hex
(.&) = (.&.)

-- | Bitwise "or"
-- >>> 0xf000 .| 0xa
-- 0x0000_0000_0000_f00a
(.|) :: Hex -> Hex -> Hex
(.|) = (.|.)

-- | Bitwise "xor"
-- >>> 0xf .^ 0xa
-- 0x0000_0000_0000_0005
(.^) :: Hex -> Hex -> Hex
(.^) = xor

-- | Bitwise "not"
-- >>> inv 1
-- 0xffff_ffff_ffff_fffe
inv :: Hex -> Hex
inv = complement


----- Arithmetic operations

-- | Negate
-- >>> neg 1
-- 0xffff_ffff_ffff_ffff
neg :: Hex -> Hex
neg = negate

-- | Integer div
-- >>> 0x1000 ./ 16
-- 0x0000_0000_0000_0100
(./) :: Hex -> Hex -> Hex
(./) = div

-- | Integer mod
-- >>> 18 .% 16
-- 0x0000_0000_0000_0002
(.%) :: Hex -> Hex -> Hex
(.%) = mod


----- Shift operations

-- | Logical left shift
-- >>> 1 .<< 16
-- 0x0000_0000_0001_0000
(.<<) :: Hex -> Int -> Hex
x .<< n = x `shift` n

-- | Logical right shift
-- >>> 0x0f00 .>> 4
-- 0x0000_0000_0000_00f0
(.>>) :: Hex -> Int -> Hex
x .>> n = x `shift` (negate n)


----- Generate bit and byte with position

-- | Set a bit
-- >>> bit1 15
-- 0x0000_0000_0000_8000
bit1 :: Int -> Hex
bit1 = bit

-- | Set bits from n1 to n2
-- >>> bits 15 8
-- 0x0000_0000_0000_ff00
bits :: Int -> Int -> Hex
bits upper lower = sum $ map bit [lower..upper]

-- | Set bits with List
-- >>> bitList [15, 8, 1]
-- 0x0000_0000_0000_8102
bitList :: [Int] -> Hex
bitList xs = foldr1 (.|) $ map bit1 xs

-- | Set a byte
-- >>> byte1 2
-- 0x0000_0000_00ff_0000
byte1 :: Int -> Hex
byte1 n = bits ((n+1)*8-1) (n*8)

-- | Set bytes from n1 to n2
-- >>> bytes  3 2
-- 0x0000_0000_ffff_0000
bytes :: Int -> Int -> Hex
bytes upper lower = bits ((upper+1)*8-1) (lower*8)

-- | Mask bits from 0 to n
-- >>> mask 7
-- 0x0000_0000_0000_00ff
mask :: Int -> Hex
mask n = bits n 0


----- Get asserted bit positions

-- | Get bit positions asserted with 1
-- >>> pos1 0x0080
-- [7]
pos1 :: Hex -> [Int]
pos1 x = bitSearch testBit x hexBitSeq

-- | Get bit positions asserted with 0
-- >>> pos0 $ inv 0x0100
-- [8]
pos0 :: Hex -> [Int]
pos0 x = bitSearch testBit (inv x) hexBitSeq

bitSearch :: (Hex -> Int -> Bool) -> Hex -> [Int] -> [Int]
bitSearch _ _ [] = []
bitSearch p x (n:ns)
  | p x n     = n : (bitSearch p x ns)
  | otherwise = bitSearch p x ns


----- Extract and replace bits

-- | Extract bits from n1 to n2
-- >>> gets 0xabcd 15 12
-- 0x0000_0000_0000_000a
gets :: Hex -> Int -> Int -> Hex
gets x upper lower
  | (upper >= lower) && (lower >= 0) = ((bits upper 0) .& x) .>> lower
  | otherwise = trace ("\ESC[2K" ++ "\nWarning: 3rd-arg larger than 2nd-arg") x

-- | Replace bits from n1 to n2
-- >>> puts 0xabcd 15 12 7
-- 0x0000_0000_0000_7bcd
puts :: Hex -> Int -> Int -> Hex -> Hex
puts x1 upper lower x2
  | (upper >= lower) && (lower >= 0) = (cbits x1 upper lower) .|
                                      ((mask (upper - lower) .& x2) .<< lower)
  | otherwise = trace ("\ESC[2K" ++ "\nWarning: 3rd-arg larger than 2nd-arg") x1

-- ToDo:
-- >>> getBit1 0b0100_0000 6
-- 0x0000_0000_0000_000a
getBit1 :: Hex -> Int -> Hex
getBit1 x n = gets x n n

-- >>> getByte1 0x12345678 2
-- 0x0000_0000_0000_0034
getByte1 :: Hex -> Int -> Hex
getByte1 x n = getBytes x n n

-- | Synonym to gets
getBits :: Hex -> Int -> Int -> Hex
getBits = gets

-- | Extract bytes from n1 to n2
-- >>> getBytes 0x12345678 2 1
-- 0x0000_0000_0000_3456
getBytes :: Hex -> Int -> Int -> Hex
getBytes x upper lower = gets x ((upper+1)*8-1) (lower*8)

-- ToDo:
-- >>> putBit1 0 7 1
-- 0x0000_0000_0000_8000
putBit1 :: Hex -> Int -> Hex -> Hex
putBit1 x1 n x2 = puts x1 n n x2

-- | Synonym to puts
putBits :: Hex -> Int -> Int -> Hex -> Hex
putBits = puts

-- | Replace bytes from n1 to n2
-- >>> putBytes 0x12345678 3 2 0xfedc
-- 0x0000_0000_fedc_5678
putBytes :: Hex -> Int -> Int -> Hex -> Hex
putBytes x1 upper lower x2 = puts x1 ((upper+1)*8-1) (lower*8) x2


----- Set and clear bits

-- | Set bits from n1 to n2 of x1
-- >>> sbits 0x1234 11 8
-- 0x0000_0000_0000_1f34
sbits :: Hex -> Int -> Int -> Hex
sbits x upper lower = x .| (bits upper lower)

-- | Clear bits from n1 to n2 of x1
-- >>> cbits 0x1234 7 4
-- 0x0000_0000_0000_1204
cbits :: Hex -> Int -> Int -> Hex
cbits x upper lower = x .& (inv (bits upper lower))


----- Permute; ToDo:

-- | Reverse bits
-- bitrev

-- | Reverse bytes
-- byterev


----- Split and merge; ToDo:

-- | Split bits to List
splitBits :: Hex -> [Int]
splitBits x = map (fromIntegral . getBit1 x) hexBitSeq

-- | Split bytes to List
-- splitBytes x1

-- ToDo:
-- | Merge bits from List
-- mergeBits :: [Int] -> Hex


-- | Merge bytes from List
-- mergeBytes [x1, x2, .. xn]


----- Predefined-constants

-- | Ei, Pi, Ti, Gi, Mi and Ki. It's not E(10^18), ... K(10^3).
-- >>> foldr1 (.|) [exa, peta, tera, giga, mega, kilo]
-- 0x1004_0100_4010_0400
exa  = bit 60 :: Hex
peta = bit 50 :: Hex
tera = bit 40 :: Hex
giga = bit 30 :: Hex
mega = bit 20 :: Hex
kilo = bit 10 :: Hex

-- | Utility numbers
-- >>> one - one == zero
-- True
-- >>> all0
-- 0x0000_0000_0000_0000
-- >>> all1
-- 0xffff_ffff_ffff_ffff
zero = 0 :: Hex
one  = 1 :: Hex
all0 = zero :: Hex
all1 = inv zero :: Hex

-- Test numbers
beef = 0xbeef :: Hex
cafe = 0xcafe :: Hex
bad  = 0x0bad :: Hex
test1 = 0x123456789abcdef0 :: Hex
test2 = 0xfedcba9876543210 :: Hex

-- ToDo: comment
hexBitSize :: Int
hexBitSize = finiteBitSize (1::Hex)

hexBitSeq :: [Int]
hexBitSeq = reverse [0..(hexBitSize-1)]

hexByteSize :: Int
hexByteSize = fromInteger ( ceiling ((fromIntegral hexBitSize) / 8))

hexByteSeq :: [Int]
hexByteSeq = reverse [0..(hexByteSize-1)]



----- Postfix notation (same as Data.Function.(&))
infixl 0 .@

-- | postfix notation
-- >>> 255 .@hex
-- "0x0000_0000_0000_00ff"
(.@) :: a -> (a -> b) -> b
x .@ f = f x


----- Formatting for hex, bin, dec and T/G/M/K unit

-- | Hexadecimal formatting
-- >>> hex 255
-- "0x0000_0000_0000_00ff"
hex :: Hex -> String
hex = hex64

hexN :: Int -> Hex -> String
hexN = formatHex "x"

hex8, hex16, hex32, hex64 :: Hex -> String
hex8  = hexN 2
hex16 = hexN 4
hex32 = hexN 8
hex64 = hexN 16

-- | Binary formatting
-- >>> bin 255
-- "0b1111_1111"
bin :: Hex -> String
bin = binN 0

binN :: Int -> Hex -> String
binN = formatHex "b"

bin8, bin16, bin32, bin64 :: Hex -> String
bin8  = binN 8
bin16 = binN 16
bin32 = binN 32
bin64 = binN 64

-- | Decimal formatting
-- >>> dec 0xff
-- "255"
dec :: Hex -> String
dec (Hex x) = show x

-- | Decimal formatting with units
-- >>> 4 * giga .@decG
-- "4"
decE x = dec $ x ./ exa
decP x = dec $ x ./ peta
decT x = dec $ x ./ tera
decG x = dec $ x ./ giga
decM x = dec $ x ./ mega
decK x = dec $ x ./ kilo

-- | Signed decimal formatting
-- signed :: Hex -> String
-- >>> 0xffffffffffffffff .@signed
-- "-1"
signed x = show (fromIntegral x :: Int)

-- format utilities
formatHex :: String -> Int -> Hex -> String
formatHex s len (Hex x) = "0" ++ s ++
                          (insertUnderScore 4 $
                           printf ("%0" ++ (show len) ++ s) x)

splitN :: Int -> [a] -> [[a]]
splitN n [] = []
splitN n xs = as : splitN n bs
    where (as, bs) = splitAt n xs

insertUnderScore :: Int -> String -> String
insertUnderScore n = reverse . intercalate "_" . splitN n .reverse


----- Miscellaneous

-- | Clear screen on VT100 terminal
cls :: IO ()
cls = putStr $ clearLines ++ gotoHead
    where
      clearLines = "\ESC[2J"
      gotoHead   = "\ESC[0;0H"

-- | Show simple help
-- ToDo:
usage :: IO ()
usage = putStr $
          "Example:\n" ++
          "  ghci> 15 :: Hex\n" ++
          ""

