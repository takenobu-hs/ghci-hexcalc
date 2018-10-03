{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE BinaryLiterals             #-}
-- LANGUAGE NumericUnderscores   -- ghc8.6 or later

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.GHex
-- Copyright   :  (c) 2018 Takenobu Tani
-- License     :  BSD3
--
-- Maintainer  :  Takenobu Tani <takenobu.hs@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- This module defines operations for an interactive hex-caluclator using GHCi.
-- This is a simple and casual interactive tool like Perl and Excel for daily work.
--
-- Interactive oriented features:
--
-- * Short-named operators and functions
--
-- * Show values in hexadecimal format by default
--
-- * Suppress type annotation of numeric literals by type inference
--
-- * Postfix-notation available 
--
-- * Highlight available
--
-- Example of use:
--
-- > ghci> (1 .<< 16) .| 0xf .& 3
-- > 0x0000_0000_0001_0003
--
-- > ghci> 0xff .@dec
-- > "255"
--
-- See also <https://github.com/takenobu-hs/ghci-hexcalc#readme web page>.
--
-----------------------------------------------------------------------------

module Data.GHex (

    -- ** Basic data type
    Hex,

    -- ** Logical operations
    (.&), (.|), (.^), inv,

    -- ** Arithmetic operations
    (./), (.%), neg, signext,

    -- ** Shift operations
    (.<<), (.>>),

    -- ** Generate bits and bytes with position
    bit1, bits, bitList, byte1, bytes, mask,

    -- ** Extract and replace bits
    gets, puts, getBit1, getByte1, getBits, getBytes,
    putBit1, putBits, putBytes,

    -- ** Set and clear bits
    sbits, cbits,

    -- ** Get asserted bit positions and count bits
    pos1, pos0, range1,
    count1, count0,

    -- ** Permute
    bitrev, byterev,
    gather, scatter,

    -- ** Split and merge
    splitBits, splitBytes, mergeBits, mergeBytes,

    -- ** Predefined-constants

    -- *** Unit constants
    -- $constants_unit
    exa, peta, tera, giga, mega, kilo,

    -- *** Utility constants
    -- $constants_unitity
    zero, one, all0, all1,

    -- *** Implementation constants
    -- $constants_impl
    hexBitSize, hexBitSeq, hexByteSize, hexByteSeq,

    -- ** Postfix notation
    (.@),

    -- ** Formatting for hex, bin, dec and T/G/M/K unit
    -- $formatting

    -- *** Hexadecimal formatting
    hex, hexN, hex8, hex16, hex32, hex64,

    -- *** Binary formatting
    bin, binN, bin8, bin16, bin32, bin64,

    -- *** Decimal formatting
    dec,
    decE, decP, decT, decG, decM, decK,

    -- *** Miscellaneous formatting
    signed,
    strip,

    -- ** Pretty print
    color, ppr,

    -- ** Input & convert
    inputRawHexIO,

    -- ** Miscellaneous
    cls,
    usage

    -- ** Properties
    -- $property

  ) where

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
-- >>> signext 0x7fff 15
-- 0x0000_0000_0000_7fff
signext :: Hex -> Int -> Hex
signext x n
  | testBit x n = sbits x (hexBitSize-1) n
  | otherwise   = cbits x (hexBitSize-1) n


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
-- >>> puts 0xabcd 15 12 0b111
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
-- Get asserted bit positions and count bits
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

-- | Count bit-1
--
-- >>> count1 0b11001
-- 3
count1 :: Hex -> Int
count1 = popCount

-- | Count bit-0
--
-- >>> count0 0xf
-- 60
count0 :: Hex -> Int
count0 = popCount . inv


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

-- | Gather bits
--
-- >>> gather 0x12345678 0x0ff000f0
-- 0x0000_0000_0000_0237
gather :: Hex -> Hex -> Hex
gather x1 x2 = let pairs = zip (splitBits x1) (splitBits x2)
               in  mergeBits $ map fst $ filter(\(x,y) -> y == 1) pairs

-- | Scatter bits
--
-- >>> scatter 0x12345678 0xff00ff00 0xabcd
-- 0x0000_0000_ab34_cd78
scatter :: Hex -> Hex -> Hex -> Hex
scatter x1 x2 x3 =
    let pairs = zip (reverse $ pos1 x2) (reverse $ splitBits x3)
    in  foldr (\(x,y) v -> putBit1 v x (fromIntegral y)) x1 pairs


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

-- $constants_unit
-- Ei, Pi, Ti, Gi, Mi and Ki. It's not E(10^18), ... K(10^3).
--
-- >>> exa == 2^60
-- True
-- >>> peta == 2^50
-- True
-- >>> tera == 2^40
-- True
-- >>> giga == 2^30
-- True
-- >>> mega == 2^20
-- True
-- >>> kilo == 2^10
-- True

-- | Ei: 2^60
exa  = bit 60 :: Hex

-- | Pi: 2^50
peta = bit 50 :: Hex

-- | Ti: 2^40
tera = bit 40 :: Hex

-- | Gi: 2^30
giga = bit 30 :: Hex

-- | Mi: 2^20
mega = bit 20 :: Hex

-- | Ki: 2^10
kilo = bit 10 :: Hex

-- $constants_unitity
-- >>> zero
-- 0x0000_0000_0000_0000
-- >>> one
-- 0x0000_0000_0000_0001
-- >>> all0
-- 0x0000_0000_0000_0000
-- >>> all1
-- 0xffff_ffff_ffff_ffff

-- | 0x0
zero = 0 :: Hex

-- | 0x1
one  = 1 :: Hex

-- | 0x0
all0 = zero :: Hex

-- | inv 0x0
all1 = inv zero :: Hex

-- Numbers for test
beef = 0xbeef :: Hex
cafe = 0xcafe :: Hex
bad  = 0x0bad :: Hex
test1 = 0x123456789abcdef0 :: Hex
test2 = 0xfedcba9876543210 :: Hex

-- $constants_impl
-- Implementation information of size.

-- | Bit size of Hex type. It's 64 on x86_64.
hexBitSize :: Int
hexBitSize = finiteBitSize (1::Hex)

-- | Number sequence. [hexBitSeq-1, hexBitSeq-2, .. 0]
hexBitSeq :: [Int]
hexBitSeq = reverse [0..(hexBitSize-1)]

-- | Byte size of Hex type. It's 8 of x86_64.
hexByteSize :: Int
hexByteSize = fromInteger ( ceiling ((fromIntegral hexBitSize) / 8))

-- | Number sequence. [hexByteSeq-1, hexByteSeq-2, .. 0]
hexByteSeq :: [Int]
hexByteSeq = reverse [0..(hexByteSize-1)]


------------------------------------------------------------------------
-- Postfix notation (same as Data.Function.(&))
------------------------------------------------------------------------

-- | Operator for postfix notation (same as Data.Function.(&))
--
-- >>> 255 .@hex
-- "0x0000_0000_0000_00ff"
-- >>> 0xf1 .@bin
-- "0b1111_0001"
-- >>> 2^12 .@dec
-- "4096"
-- >>> 4 * giga .@pos1
-- [32]
--
-- > 0x0 .@color (bits 31 24)
-- > 0b0000_0000_0000_0000_0000_0000_0000_0000_1111_1111_0000_0000_0000_0000_0000_0000
-- >                                           ^^^^ ^^^^

(.@) :: a -> (a -> b) -> b
x .@ f = f x


------------------------------------------------------------------------
-- Formatting for hex, bin, dec and T,G,M,K unit and any.
------------------------------------------------------------------------

-- $formatting
-- Formatting utilities.
--
-- >>> 255 .@hex
-- "0x0000_0000_0000_00ff"
-- >>> 255 .@bin
-- "0b1111_1111"
-- >>> 0xff .@dec
-- "255"
--
-- >>> 2^32 .@decG
-- "4"
--
-- >>> 0xffffffffffffffff .@signed
-- "-1"
--
-- >>> strip "0b" "0b1101"
-- "1101"
-- >>> strip "_" "0x1234_5678_9abc_def0"
-- "0x123456789abcdef0"

-- | Hexadecimal formatting with maximum-bit length
hex :: Hex -> String
hex = hex64

-- | Hexadecimal formatting with N-bit length
hexN :: Int -> Hex -> String
hexN = formatHex "x"

hex8, hex16, hex32, hex64 :: Hex -> String
-- | Hexadecimal formatting with 8-bit length
hex8  = hexN 2
-- | Hexadecimal formatting with 16-bit length
hex16 = hexN 4
-- | Hexadecimal formatting with 32-bit length
hex32 = hexN 8
-- | Hexadecimal formatting with 64-bit length
hex64 = hexN 16

-- | Binary formatting with auto-adjusted length
bin :: Hex -> String
bin = binN 0

-- | Binary formatting with N-bit length
binN :: Int -> Hex -> String
binN = formatHex "b"

bin8, bin16, bin32, bin64 :: Hex -> String
-- | Binary formatting with 8-bit length
bin8  = binN 8
-- | Binary formatting with 16-bit length
bin16 = binN 16
-- | Binary formatting with 32-bit length
bin32 = binN 32
-- | Binary formatting with 64-bit length
bin64 = binN 64

-- | Decimal formatting
dec :: Hex -> String
dec (Hex x) = show x

-- Decimal formatting with units
-- | Decimal formatting with exa unit
decE x = dec $ x ./ exa
-- | Decimal formatting with pata unit
decP x = dec $ x ./ peta
-- | Decimal formatting with tera unit
decT x = dec $ x ./ tera
-- | Decimal formatting with giga unit
decG x = dec $ x ./ giga
-- | Decimal formatting with mega unit
decM x = dec $ x ./ mega
-- | Decimal formatting with kilo unit
decK x = dec $ x ./ kilo

-- | Signed decimal formatting
signed :: Hex -> String
signed x = show (fromIntegral x :: Int)

-- | Strip strings
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
formatHex s len !(Hex x) = "0" ++ s ++
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

-- | Highlight the specified bit
--
-- > ghci> 0xff .@color (bits 7 4)
-- > 0b0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_1111_1111
-- >                                                                         ^^^^
--
-- > ghci> 0xffffffff .@color mega
-- > 0b0000_0000_0000_0000_0000_0000_0000_0000_1111_1111_1111_1111_1111_1111_1111_1111
-- >                                                        ^
--
-- > ghci> 0 .@color (bitList [54,53,4,3,2])
-- > 0b0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000
-- >              ^^                                                            ^ ^^

-- > ghci> 0xffff .@color (bitList [54,53,4,3,2])
--
-- > ghci> 0xffff .@color (bits 63 51 .| bits 11 2)
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

-- | Output value by IO (not String)
--
-- >>> 0xf0 .@ppr bin
-- 0b1111_0000
ppr :: (Hex -> String) -> Hex -> IO ()
ppr f x = putStrLn $ f x


------------------------------------------------------------------------
-- Input & convert
------------------------------------------------------------------------

-- | Input hexadecimal string and convert to Hex type
--
-- > ghci> inputRawHexIO
-- > ff aa  (your input)
-- > ghci>x = it
-- > ghci>x
-- > 0x0000_0000_0000_ffaa
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

-- | Clear screen with ANSI sequences
cls :: IO ()
cls = putStr $ clearLines ++ gotoHead
    where
      clearLines = "\ESC[2J"
      gotoHead   = "\ESC[0;0H"

-- | Show simple usage
usage :: IO ()
usage = putStr $
          " -- Commands by ghci:\n" ++
          "   :browse                     show function list\n" ++
          "   :doc <function>             show document of function\n" ++
          "\n" ++
          " -- Examples:\n" ++
          "   x = 15 :: Hex               annotate Hex type\n" ++
          "   y = it                      set previous result\n" ++
          "   2^16 + 3 :: Hex             arithmetic operation\n" ++
          "   (1 .<< 16) .| 0xf .& 3      logical operation\n" ++
          "   (inv 0xffff) .^ 0xff        logical operation\n" ++
          "   bits 15 2                   set bits from 15 to 2\n" ++
          "   bitList [15, 8, 1]          set bits by List\n" ++
          "   gets 0xabcd 15 12           get bits from 15 to 12\n" ++
          "   puts 0xabcd 15 12 7         replace bits from 15 to 12 by 7\n" ++
          "   4 * mega                    predefined constant\n" ++
          "   0xffff .@bin                formatting in binary\n" ++
          "   giga .@dec                  formatting in decimal\n" ++
          "   0x0a00 .@pos1               get asserted bit positions\n" ++
          "   0xff .@color (bits 7 4)     hilighting bit (7 to 4)\n" ++
          "\n"


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

-- | Split element for each N
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


----- Warning message

-- | (!) IO output of string in pure code
traceWarn :: String -> a -> a
traceWarn str x = trace (colorMagenta ++ str ++ colorReset) x


------------------------------------------------------------------------
-- Property test with quickcheck
------------------------------------------------------------------------

-- | Property test
-- $setup
-- >>> import Test.QuickCheck
-- >>> :set -XBinaryLiterals
-- >>> skip = True
-- >>> when c x = if c then x else skip
-- >>> :{
-- instance Arbitrary Hex where
--   arbitrary = Hex `fmap`
--               oneof [ choose(0x0000000000000000, 0x000000000000ffff)
--                     , choose(0x00000000ffffff00, 0x000000ffffffffff)
--                     , choose(0x00ffffffffffffff, 0xff00000000000000)
--                     , choose(0x0000000000000000, 0xffffffffffffffff) ]
-- >>> :}

-- $property
-- Property in quickcheck and doctest.
--
-- prop> (inv $ inv x) == x
-- prop> (neg $ neg x) == x
-- prop> (neg x) == (inv x + 1)
--
-- prop> (x .& y) == (inv ((inv x) .| (inv y)))             -- De Morgan
-- prop> (x .^ y) == (((x .& (inv y)) .| ((inv x) .& y)))   -- xor
--
-- prop> when (y /= 0) $ ((x ./ y)*y + (x .% y)) == x       -- div and mod
--
-- prop> when (n >= 0) $ (x .<< n) == (x * (2^n))                 -- left shift
-- prop> when (n >= 0) $ (x .>> n) == (bitrev ((bitrev x) .<< n)) -- right shift
--
-- prop> when (x >= 0) $ (bit1 x) == (2^x)
-- prop> when (x >= 0) $ (byte1 x) == (0xff .<< (8*x))
--
-- prop> when (x1 >= x2 && x2 >= 0) $ (bits x1 x2) == (sum $ map (2^) [x2..x1])
-- prop> when (x1 >= x2 && x2 >= 0) $ (bytes x1 x2) == (sum $ map byte1 [x2..x1])
--
-- prop> when (x1 >= x2 && x2 >= 0) $ (gets all0 x1 x2) == all0
-- prop> when (x1 >= x2 && x2 >= 0) $ ((gets x x1 x2) .<< x2) == (x .& bits x1 x2)
-- prop> when (x1 >= x2 && x2 >= 0) $ (puts x x1 x2 $ gets x x1 x2) == x
--
-- prop> when (x1 >= x2 && x2 >= 0) $ (gather x (bits x1 x2)) == (gets x x1 x2)
-- prop> when (x1 > x2 && x2 >= 0)  $ (gather x3 (bit1 x1 .| bit1 x2)) == (((getBit1 x3 x1) .<< 1) .| (getBit1 x3 x2))
-- prop> when (x1 >= x2 && x2 >= 0) $ (scatter x1 x2 $ gather x1 x2) == x1
--
-- prop> when (x1 >= x2 && x2 >= 0) $ (sbits all0 x1 x2) == (bits x1 x2)
-- prop> when (x1 >= x2 && x2 >= 0) $ (sbits all0 x1 x2) == (puts all0 x1 x2 all1)
-- prop> when (x1 >= x2 && x2 >= 0) $ (cbits all1 x1 x2) == (inv(bits x1 x2))
-- prop> when (x1 >= x2 && x2 >= 0) $ (cbits all1 x1 x2) == (puts all1 x1 x2 all0)
-- prop> when (x1 >= x2 && x2 >= 0) $ (cbits x x1 x2) == (inv (sbits (inv x) x1 x2))
--
-- prop> (x .@pos1 .@bitList) == x
-- prop> (x .@pos0 .@bitList) == (inv x)
-- prop> when (x1 >= x2 && x2 >= 0 && x1 < hexBitSize) $ (range1 $ bits x1 x2) == (x1,x2)
-- prop> (count1 x) == (length $ pos1 x)
-- prop> ((count1 x) + (count0 x)) == hexBitSize
--
-- prop> (mergeBits $ splitBits x) == x
-- prop> (mergeBytes $ splitBytes x) == x
-- prop> (bitrev $ bitrev x) == x
-- prop> (byterev $ byterev x) == x
--
-- prop> when (n >= 0 && x `testBit` n) $ ((signext x n) .| (sbits x (n-1) 0)) == all1
-- prop> when (n >= 0 && (not(x `testBit` n))) $ ((signext x n) .& (cbits x (n-1) 0)) == all0
-- prop> when (not(x `testBit` (hexBitSize-1))) $ (signed x) == (dec x)
-- prop> when (x `testBit` (hexBitSize-1)) $ (signed x) == show(-1 * (fromIntegral $ ((inv x) + 1))::Int)
