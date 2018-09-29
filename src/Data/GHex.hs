{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE BinaryLiterals             #-}
-- LANGUAGE NumericUnderscores   -- ghc8.6 or later

module Data.GHex where

import           Data.Bits
import           Data.List   (foldl', intercalate)
import           Text.Printf (printf)
import           Debug.Trace (trace)

------------------------------------------------------------------------
-- Basic type
------------------------------------------------------------------------

-- | Basic type
--
-- >>> 255 :: Hex
-- 0x0000_0000_0000_00ff
newtype Hex = Hex Word
              deriving (Eq, Ord, Read, Num, Enum, Real, Bounded, Integral,
                        Bits, FiniteBits)

instance Show Hex where
    show = hex


------------------------------------------------------------------------
-- Infix definition of operators
------------------------------------------------------------------------
infixl 8 .<< , .>>
infixl 7 .& , ./ , .%
infixl 6 .^
infixl 5 .|
infixl 0 .@


------------------------------------------------------------------------
-- Logical operations
------------------------------------------------------------------------

-- | Bitwise "and"
--
-- >>> 0x1234 .& 0xff
-- 0x0000_0000_0000_0034
(.&) :: Hex -> Hex -> Hex
(.&) = (.&.)

-- | Bitwise "or"
--
-- >>> 0xf000 .| 0xa
-- 0x0000_0000_0000_f00a
(.|) :: Hex -> Hex -> Hex
(.|) = (.|.)

-- | Bitwise "xor"
--
-- >>> 0xf .^ 0xa
-- 0x0000_0000_0000_0005
(.^) :: Hex -> Hex -> Hex
(.^) = xor

-- | Bitwise "not"
--
-- >>> inv 1
-- 0xffff_ffff_ffff_fffe
inv :: Hex -> Hex
inv = complement


------------------------------------------------------------------------
-- Arithmetic operations
------------------------------------------------------------------------

-- | Integer div
--
-- >>> 0x1000 ./ 16
-- 0x0000_0000_0000_0100
(./) :: Hex -> Hex -> Hex
(./) = div

-- | Integer mod
--
-- >>> 18 .% 16
-- 0x0000_0000_0000_0002
(.%) :: Hex -> Hex -> Hex
(.%) = mod

-- | Negate
--
-- >>> neg 1
-- 0xffff_ffff_ffff_ffff
neg :: Hex -> Hex
neg = negate

-- | Sign extention
--
-- >>> signext 0x80 7
-- 0xffff_ffff_ffff_ff80
signext :: Hex -> Int -> Hex
signext x n
  | testBit x n = (bits (hexBitSize-1) n)
  | otherwise   = x


------------------------------------------------------------------------
-- Shift operations
------------------------------------------------------------------------

-- | Logical left shift
--
-- >>> 1 .<< 16
-- 0x0000_0000_0001_0000
(.<<) :: Hex -> Int -> Hex
x .<< n = x `shift` n

-- | Logical right shift
--
-- >>> 0x0f00 .>> 4
-- 0x0000_0000_0000_00f0
(.>>) :: Hex -> Int -> Hex
x .>> n = x `shift` (negate n)


------------------------------------------------------------------------
-- Generate bits and bytes with position
------------------------------------------------------------------------

-- | Set a bit
--
-- >>> bit1 15
-- 0x0000_0000_0000_8000
bit1 :: Int -> Hex
bit1 = bit

-- | Set bits from n1 to n2
--
-- >>> bits 15 8
-- 0x0000_0000_0000_ff00
bits :: Int -> Int -> Hex
bits upper lower = sum $ map bit [lower..upper]

-- | Set bits with List
--
-- >>> bitList [15, 8, 1]
-- 0x0000_0000_0000_8102
bitList :: [Int] -> Hex
bitList xs = foldr1 (.|) $ map bit1 xs

-- | Set a byte
--
-- >>> byte1 2
-- 0x0000_0000_00ff_0000
byte1 :: Int -> Hex
byte1 n = bits ((n+1)*8-1) (n*8)

-- | Set bytes from n1 to n2
--
-- >>> bytes  3 2
-- 0x0000_0000_ffff_0000
bytes :: Int -> Int -> Hex
bytes upper lower = bits ((upper+1)*8-1) (lower*8)

-- | Mask bits from 0 to n
--
-- >>> mask 7
-- 0x0000_0000_0000_00ff
mask :: Int -> Hex
mask n = bits n 0


------------------------------------------------------------------------
-- Get asserted bit positions
------------------------------------------------------------------------

-- | Get bit positions asserted with 1
--
-- >>> pos1 0x0080
-- [7]
pos1 :: Hex -> [Int]
pos1 x = bitSearch testBit x hexBitSeq

-- | Get bit positions asserted with 0
--
-- >>> pos0 $ inv 0x0100
-- [8]
pos0 :: Hex -> [Int]
pos0 x = bitSearch testBit (inv x) hexBitSeq

-- | Get upper and lower boundaries of asserted bits
--
-- >>> range1 0x0f000000
-- (27,24)
range1 :: Hex -> (Int,Int)
range1 x = let y = pos1 x
           in  (head y, last y)


------------------------------------------------------------------------
-- Extract and replace bits
------------------------------------------------------------------------

-- | Extract bits from n1 to n2
--
-- >>> gets 0xabcd 15 12
-- 0x0000_0000_0000_000a
gets :: Hex -> Int -> Int -> Hex
gets x upper lower
  | (upper >= lower) && (lower >= 0) = ((bits upper 0) .& x) .>> lower
  | otherwise = traceWarn "Warning: 3rd-arg larger than 2nd-arg" x

-- | Replace bits from n1 to n2
--
-- >>> puts 0xabcd 15 12 7
-- 0x0000_0000_0000_7bcd
puts :: Hex -> Int -> Int -> Hex -> Hex
puts x1 upper lower x2
  | (upper >= lower) && (lower >= 0) = (cbits x1 upper lower) .|
                                      ((mask (upper - lower) .& x2) .<< lower)
  | otherwise = traceWarn "Warning: 3rd-arg larger than 2nd-arg" x1


-- | Extract a bit
--
-- >>> getBit1 (bit1 6) 6
-- 0x0000_0000_0000_0001
getBit1 :: Hex -> Int -> Hex
getBit1 x n = gets x n n

-- | Extract a byte
--
-- >>> getByte1 0x12345678 2
-- 0x0000_0000_0000_0034
getByte1 :: Hex -> Int -> Hex
getByte1 x n = getBytes x n n

-- | Synonym to gets
getBits :: Hex -> Int -> Int -> Hex
getBits = gets

-- | Extract bytes from n1 to n2
--
-- >>> getBytes 0x12345678 2 1
-- 0x0000_0000_0000_3456
getBytes :: Hex -> Int -> Int -> Hex
getBytes x upper lower = gets x ((upper+1)*8-1) (lower*8)

-- | Replace a bit
--
-- >>> putBit1 0 7 1
-- 0x0000_0000_0000_0080
putBit1 :: Hex -> Int -> Hex -> Hex
putBit1 x1 n x2 = puts x1 n n x2

-- | Synonym to puts
putBits :: Hex -> Int -> Int -> Hex -> Hex
putBits = puts

-- | Replace bytes from n1 to n2
--
-- >>> putBytes 0x12345678 3 2 0xfedc
-- 0x0000_0000_fedc_5678
putBytes :: Hex -> Int -> Int -> Hex -> Hex
putBytes x1 upper lower x2 = puts x1 ((upper+1)*8-1) (lower*8) x2

-- | Gather bits
--
-- >>> gather 0x12345678 0x0ff000f0
-- 0x0000_0000_0000_0237
gather x1 x2 = let pairs = zip (splitBits x1) (splitBits x2)
               in  mergeBits $ map fst $ filter(\(x,y) -> y == 1) pairs


------------------------------------------------------------------------
-- Set and clear bits
------------------------------------------------------------------------

-- | Set bits from n1 to n2 of x1
--
-- >>> sbits 0x1234 11 8
-- 0x0000_0000_0000_1f34
sbits :: Hex -> Int -> Int -> Hex
sbits x upper lower = x .| (bits upper lower)

-- | Clear bits from n1 to n2 of x1
--
-- >>> cbits 0x1234 7 4
-- 0x0000_0000_0000_1204
cbits :: Hex -> Int -> Int -> Hex
cbits x upper lower = x .& (inv (bits upper lower))


------------------------------------------------------------------------
-- Permute
------------------------------------------------------------------------

-- | Reverse bits
--
-- >>> bitrev 0x00a1
-- 0x8500_0000_0000_0000
bitrev :: Hex -> Hex
bitrev = mergeBits . reverse . splitBits

-- | Reverse bytes
--
-- >>> byterev 0x12341111cdef
-- 0xefcd_1111_3412_0000
byterev :: Hex -> Hex
byterev = mergeBytes . reverse . splitBytes


------------------------------------------------------------------------
-- Split and merge
------------------------------------------------------------------------

-- | Split bits to List
--
-- >>> splitBits 0xa
-- [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0]
splitBits :: Hex -> [Int]
splitBits x = map (fromIntegral . getBit1 x) hexBitSeq

-- | Split bytes to List
--
-- >>> splitBytes 0xff10
-- [0,0,0,0,0,0,255,16]
splitBytes :: Hex -> [Int]
splitBytes x = map (fromIntegral . getByte1 x) hexByteSeq

-- | Merge bits from List
--
-- >>> mergeBits [1,0,1,0,0,0,0,0]
-- 0x0000_0000_0000_00a0
mergeBits :: [Int] -> Hex
mergeBits xs = foldl' f 0 xs
    where f v x = v*2 + (fromIntegral x) .& 0x1

-- | Merge bytes from List
--
-- >>> mergeBytes [0xff, 0x1, 0xa]
-- 0x0000_0000_00ff_010a
mergeBytes :: [Int] -> Hex
mergeBytes xs = foldl' f 0 xs
    where f v x = v*256 + ((fromIntegral x) .& 0xff)


------------------------------------------------------------------------
-- Predefined-constants
------------------------------------------------------------------------

-- | Ei, Pi, Ti, Gi, Mi and Ki. It's not E(10^18), ... K(10^3).
--
-- >>> foldr1 (.|) [exa, peta, tera, giga, mega, kilo]
-- 0x1004_0100_4010_0400
exa  = bit 60 :: Hex
peta = bit 50 :: Hex
tera = bit 40 :: Hex
giga = bit 30 :: Hex
mega = bit 20 :: Hex
kilo = bit 10 :: Hex

-- | Utility numbers
--
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

-- Numbers for test
beef = 0xbeef :: Hex
cafe = 0xcafe :: Hex
bad  = 0x0bad :: Hex
test1 = 0x123456789abcdef0 :: Hex
test2 = 0xfedcba9876543210 :: Hex

-- Implementation information for size
hexBitSize :: Int
hexBitSize = finiteBitSize (1::Hex)

hexBitSeq :: [Int]
hexBitSeq = reverse [0..(hexBitSize-1)]

hexByteSize :: Int
hexByteSize = fromInteger ( ceiling ((fromIntegral hexBitSize) / 8))

hexByteSeq :: [Int]
hexByteSeq = reverse [0..(hexByteSize-1)]


------------------------------------------------------------------------
-- Postfix notation (same as Data.Function.(&))
------------------------------------------------------------------------

-- | postfix notation
--
-- >>> 255 .@hex
-- "0x0000_0000_0000_00ff"
(.@) :: a -> (a -> b) -> b
x .@ f = f x


------------------------------------------------------------------------
-- Formatting for hex, bin, dec and T/G/M/K unit
------------------------------------------------------------------------

-- | Hexadecimal formatting
--
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
--
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
--
-- >>> dec 0xff
-- "255"
dec :: Hex -> String
dec (Hex x) = show x

-- | Decimal formatting with units
--
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
--
-- >>> 0xffffffffffffffff .@signed
-- "-1"
signed x = show (fromIntegral x :: Int)

-- | Strip strings
--
-- >>> strip "0b" "0b1101"
-- "1101"
-- >>> strip "_" "0x1234_5678_9abc_def0"
-- "0x123456789abcdef0"
strip :: String -> String -> String
strip "" xs  = xs
strip pat [] = ""
strip pat xxs@(x:xs)
  | (take len xxs == pat) = strip pat $ drop len xxs
  | otherwise             = x : strip pat xs
  where len = length pat

-- | Format for printf
--
-- >>> formatHex "x" 4 0xab
-- "0x00ab"
-- >>> formatHex "b" 8 127
-- "0b0111_1111"
formatHex :: String -> Int -> Hex -> String
formatHex s len (Hex x) = "0" ++ s ++
                          (insertUnderScore 4 $
                           printf ("%0" ++ (show len) ++ s) x)

-- | Insert underscore to string
--
-- >>> insertUnderScore 4  "123456789abcdef0"
-- "1234_5678_9abc_def0"
insertUnderScore :: Int -> String -> String
insertUnderScore n xs = insertElemBy "_" n xs


------------------------------------------------------------------------
-- Pretty print
------------------------------------------------------------------------

-- | Display in color specified bits
-- >> test1 .@color (bit 7)
-- >> test1 .@color 0xf0
-- >> test1 .@color giga
-- >> test1 .@color (bitList [54,53,4,3,2])
-- >> test1 .@color (bits 63 51 .| bits 11 2)
color :: Hex -> Hex -> IO ()
color x2 x1 = putStrLn $ pprColorBin x1 x2

-- | Pretty print with hilighting of inverted color
--
-- >>> pprColorBin 0x1 0xf
-- "0b0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_\ESC[7m0\ESC[0m\ESC[7m0\ESC[0m\ESC[7m0\ESC[0m\ESC[7m1\ESC[0m"
pprColorBin :: Hex -> Hex -> String
pprColorBin !x1 !x2 = let pairs = zip (splitBits x1) (splitBits x2)
                      in "0b" ++ (insertUnderScoreList 4 $ map putColor pairs)

-- | Insert ANSI sequences of color
--
-- >>> putColor (1,0)
-- "1"
-- >>> putColor (1,1)
-- "\ESC[7m1\ESC[0m"
putColor :: (Int,Int) -> String
putColor (x1,x2)
  | (x2 == 1)  = colorInv ++ (show x1) ++ colorReset
  | otherwise  = show x1

-- | Insert underscore to string
--
-- >>> insertUnderScoreList 4 ["0","1","1","1","1","0","0","0","0"]
-- "0_1111_0000"
insertUnderScoreList :: Int -> [String] -> String
insertUnderScoreList n = concat . insertElemBy ["_"] n

-- | Output with IO
--
-- >>> 0xf0 .@ppr bin
-- 0b1111_0000
ppr :: (Hex -> String) -> Hex -> IO ()
ppr f x = putStrLn $ f x


------------------------------------------------------------------------
-- Input & convert
------------------------------------------------------------------------

-- | Input hexadecimal string and convert to Hex type
inputRawHexIO :: IO Hex
inputRawHexIO = do
    x <- getLine
    return $ convertStrToHex x

-- | Convert string to Hex type
--
-- >>> convertStrToHex "123456789ABCDEF0"
-- 0x1234_5678_9abc_def0
-- >>> convertStrToHex "0xfedc_ba98_7654_3210"
-- 0xfedc_ba98_7654_3210
-- >>> convertStrToHex "ab cd ef 12"
-- 0x0000_0000_abcd_ef12
-- >>> convertStrToHex "12:34:56:78"
-- 0x0000_0000_1234_5678
-- >>> convertStrToHex "01,02,0e,0f"
-- 0x0000_0000_0102_0e0f
-- >>> convertStrToHex "0xff 0xee 0x22 0x11"
-- 0x0000_0000_ffee_2211
convertStrToHex :: String -> Hex
convertStrToHex x = read $ "Hex 0x" ++ (filterHexChar $ strip "0x" x)

-- | Filter charcters of hexadecimal
--
-- >>> filterHexChar "0_1:2:3-4,5.6;789abcdefABCDEFghiklmnopqrstuvwxyz@!"
-- "0123456789abcdefABCDEF"
filterHexChar :: String -> String
filterHexChar = filter (`elem` "0123456789abcdefABCDEF")


------------------------------------------------------------------------
-- Miscellaneous
------------------------------------------------------------------------

-- | Clear screen on VT100 terminal
cls :: IO ()
cls = putStr $ clearLines ++ gotoHead
    where
      clearLines = "\ESC[2J"
      gotoHead   = "\ESC[0;0H"

-- | Show simple usage
-- ToDo:
usage :: IO ()
usage = putStr $
          "Example:\n" ++
          "  ghci> 15 :: Hex\n" ++
          ""


------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------

----- Manipulation of data

-- | Search bit with predicate-function
-- pos1 x = bitSearch testBit x hexBitSeq
--
-- >>> bitSearch (\x n -> ((getBit1 x n) == 1)) (bit1 2 .| bit1 0) [3,2,1,0]
-- [2,0]
bitSearch :: (Hex -> Int -> Bool) -> Hex -> [Int] -> [Int]
bitSearch _ _ [] = []
bitSearch p x (n:ns)
  | p x n     = n : (bitSearch p x ns)
  | otherwise = bitSearch p x ns

-- | Split element
--
-- >>> splitN 2 [1,2,3,4,5,6,7]
-- [[1,2],[3,4],[5,6],[7]]
splitN :: Int -> [a] -> [[a]]
splitN n [] = []
splitN n xs = as : splitN n bs
    where (as, bs) = splitAt n xs

-- | Insert element each n
--
-- >>> insertElemBy "_" 4 "123456789"
-- "1_2345_6789"
-- >>> insertElemBy ["_"] 2 ["a","b","c","d","e"]
-- ["a","_","b","c","_","d","e"]
insertElemBy :: [a] -> Int -> [a] -> [a]
insertElemBy s n = reverse . intercalate s . splitN n .reverse

----- Warning message

-- | (!) IO output of string in pure code
traceWarn :: String -> a -> a
traceWarn str x = trace (colorMagenta ++ str ++ colorReset) x

----- ANSI Escape sequences

-- | Magenta color
colorMagenta :: String
colorMagenta = "\ESC[35m"

-- | Invert color
colorInv :: String
colorInv = "\ESC[7m"

-- | Reset attribute
colorReset :: String
colorReset = "\ESC[0m"

