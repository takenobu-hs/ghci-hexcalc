<p align="left"><img src="http://takenobu-hs.github.io/downloads/images/haskell-logo-s.png"/></p>

GHCi as a Hex-Calculator interactive
====================================

The GHCi (REPL for Haskell) is a very useful interactive tool, it's not only for debugging.  
This package "ghci-hexcalc" is an interactive hex-calculator using Haskell/GHCi.
This is a simple and casual tool like Perl and Excel for our daily work.

Interactive oriented features:
* Short-named operators and functions
* Show values in hexadecimal format by default
* Suppress type annotation of numeric literals by type inference
* Postfix-notation available 
* Highlight available

See also [description on Hackage](https://hackage.haskell.org/package/ghci-hexcalc).

Contents
--------

  * [Run](#run)
  * [Example of use](#example-of-use)
  * [Specification](#specification)
  * [Appendix](#appendix)


Run
---

Bare GHCi:
```
$ ghci src/Data/GHex.hs
```
or
```
$ ghci -ghci-script example/example.ghci
```

Stack:
```
$ stack exec -- ghci  src/Data/GHex.hs
```


Example of use
--------------

#### Numeric literals by `Hex` type annotation

The value of Hex type is shown as hexadecimal format.

```
ghci> 1 :: Hex
0x0000_0000_0000_0001
```

```
ghci> 0xff :: Hex
0x0000_0000_0000_00ff
```

```
ghci> 0b1011 :: Hex
0x0000_0000_0000_000b
```

```
ghci> 16 + 3 :: Hex
0x0000_0000_0000_0013
```

#### Variables on GHCi

You could use variables of Haskell syntax on GHCi.

```
ghci> x = 255 :: Hex
ghci> x + 3
ghci> y = it       -- `it` is GHCi's variable. It stores the previous result.
```

#### Arithmetic operations

You could also use arithmetic operator in Hex type.

```
ghci> x + 3
ghci> (x * 256) -1
ghci> x + 2^10
ghci> neg x
```

#### Logical operations

Numeric literals applied to function of this package is inferred as B type.

```
ghci> 0xff .& 6
ghci> 256 .| 16
ghci> 100 .^ 5
ghci> inv 255
```

#### Shift operations

```
ghci> 1 .<< 16
ghci> 256 .>> 1
```


#### Div and mod operations

```
ghci> 0xff0000 ./ 256
ghci> 0xfedc .% 256
```

#### Generate bit and byte with position

```
ghci> bit1 15
0x0000_0000_0000_8000
```

```
ghci> bits 7 4
0x0000_0000_0000_00f0
```

```
ghci> bitList [15, 14, 1]
0x0000_0000_0000_c002
```

```
ghci> byte1 2
0x0000_0000_00ff_0000
```

```
ghci> bytes 4 3
0x0000_00ff_ff00_0000
```

#### Extract and replace bits
```
ghci> gets 0xabcd 15 12
0x0000_0000_0000_000a
```
```
ghci> puts 0xabcd 15 12 7
0x0000_0000_0000_7bcd
```

#### Set and clear bits

```
ghci> sbits 0x1234 11 8
0x0000_0000_0000_1f34
```

```
ghci> cbits 0x1234 7 4
0x0000_0000_0000_1204
```


#### Get asserted bit positions and count bits

```
ghci> pos1 0x0081
[7,0]
```

```
ghci> pos0 $ inv 0x0100
[8]
```

```
ghci> count1 0b11001
3
```


#### Permute, split and merge

```
ghci> gather 0x12345678 0x0ff000f0
0x0000_0000_0000_0237
```

```
ghci> scatter 0x12345678 0xff00ff00 0xabcd
0x0000_0000_ab34_cd78
```

```
ghci> (3,0b101) .++ (2,0b11)
(5,0x0000_0000_0000_0017)
```


#### Predefined-constants

```
ghci> mega
0x0000_0000_0010_0000
```

```
ghci> giga
0x0000_0000_4000_0000
```

```
ghci> 4 * giga - 1
0x0000_0000_ffff_ffff
```

```
ghci> 2^32 ./ giga
0x0000_0000_0000_0004
```

#### Postfix-notation

The operator `.@` is an operator for postfix notation.
It's the same as `Data.Function.(&)`.

The following two are the same:

```
ghci> pos1 0xf0
[7,6,5,4]
```

```
ghci> 0xf0 .@pos1
[7,6,5,4]
```


#### Formatting for hex, bin, dec, Tera/Giga/Mega/Kilo and signed

Formatting functions convert a Hex type value to a string type for each format.  

```
ghci> 2^16 .@hex
"0x0000_0000_0001_0000"
```

```
ghci> 100 .@bin
"0b110_0100"
```

```
ghci> 100 .@bin16
"0b0000_0000_0110_0100"
```

```
ghci> giga .@dec
"1073741824"
```

```
ghci> bit 43 .@decT
"8"
```

```
ghci> 0xffffffffffffffff .@signed
"-1"
```


#### Hilighting specified bits

The function `color` highlights specified bits. It inverts the color in the ANSI sequence for the specified bits.

```
ghci> 0xff .@color (bits 7 4)
0b0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_1111_1111
                                                                        ^^^^
```

```
ghci> 0xffffffff .@color mega
0b0000_0000_0000_0000_0000_0000_0000_0000_1111_1111_1111_1111_1111_1111_1111_1111
                                                       ^
```

```
ghci> 0 .@color (bitList [54,53,4,3,2])
0b0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000
             ^^                                                            ^ ^^
```


#### Input & convert

The function `inputRawHexIO`  inputs a string and converts it to a Hex type.

```
ghci> inputRawHexIO
ff aa  (your input)
ghci> x = it
ghci> x
0x0000_0000_0000_ffaa
```

```
ghci> x <- inputRawHexIO
ff aa  (your input)
ghci> x
0x0000_0000_0000_ffaa
```


#### Combination examples

```
ghci> x .| bit1 18
ghci> (x .<< 4) .& 0xf0
ghci> bit1 33 ./ giga
ghci> 2 * mega .@dec
ghci> 4 * tera .@pos1
ghci> foldr1 (.|) [0xa, 0xcc, 0xd1b]
ghci> 0 .@color (tera .| giga .| mega .| kilo)
```

#### Using Data.Bits library

Hex type is deriving Data.Bits type. So you could use functions of Data.Bits.

```
ghci> x `testBit` 8
ghci> x `clearBit` 15
ghci> x .&. 0xff
ghci> countLeadingZeros x
ghci> popCount x
```

#### Clear screen

```
ghci> cls
```

#### Simple help

Show simple usage:

```
ghci> usage
```

Listing APIs with `:browse` ghci command:

```
ghci> :browse
newtype Hex = Hex Word
(.&) :: Hex -> Hex -> Hex
(.|) :: Hex -> Hex -> Hex
(.^) :: Hex -> Hex -> Hex
inv :: Hex -> Hex
  :
```

When you run with `ghci -haddock`, you could also use `:doc` ghci command (ghc8.6 or later):

```
ghci> :doc bits
 Set bits from n1 to n2

 >>> bits 15 8
 0x0000_0000_0000_ff00
```


Specification
-------------

Please see also [Hackage document](http://hackage.haskell.org/package/ghci-hexcalc/docs/Data-GHex.html) in detail.

#### General

* Core type:
  * The core type of this package is the `Hex` type.
  * `Hex` type is implemented in unsigned `Word`.
  * `Hex` type is 64 bit length on x86_64.
* Operators:
  * Operators in this package begin with `.`(dot), like `.&` and `.|`.


#### Numeric literals by `Hex` type annotation

| Operation                     | Description                           |
|:------------------------------|:--------------------------------------|
| `:: Hex`                      | Type annotation for basic type `Hex`  |
| `255 :: Hex`                  | Decimal number literal                |
| `0xff :: Hex`                 | Hexadecimal number literal            |
| `0b1101 :: Hex`               | binary number literal                 |
				

#### Derived operations

| Operation                     | Description                           |
|:------------------------------|:--------------------------------------|
| Many operations               | Eq, Ord, Num, Enum, Real, Bounded, Integral, Bits and FiniteBits class available  |


#### Postfix operator

| Operation                     | Description                           |
|:------------------------------|:--------------------------------------|
| `.@`                          | Postfix-notation operator             |


#### Arithmetic operations

| Operation                     | Description                           |
|:------------------------------|:--------------------------------------|
| `+`, `-`, `*`, `^`, ...       | Num, Real class available             |
| `neg` x1                      | Negation. (inv x1 + 1)                |
| `signext` x1 n1               | Sign extention                        |
| x1 `./` x2                    | Integer division                      |
| x1 `.%` x2                    | Integer modulo                        |


#### Logical operations

| Operation                     | Description                           |
|:------------------------------|:--------------------------------------|
| x1 `.&` x2                    | Bitwise "and"                         |
| x1 `.\|` x2                   | Bitwise "or"                          |
| x1 `.^` x2                    | Bitwise "xor"                         |
| `inv` x1                      | Bitwise "not" (invert)                |


#### Shift operations

| Operation                     | Description                           |
|:------------------------------|:--------------------------------------|
| x1 `.<<` n1                   | Logical left shift                    |
| x1 `.>>` n1                   | Logical right shift                   |


#### Generate bit and byte with position

| Operation                     | Description                           |
|:------------------------------|:--------------------------------------|
| `bit1` n1                     | Set a bit                             |
| `bits` n1 n2                  | Set bits from n1 to n2                |
| `bitList` [n1, n2, ... nn]    | Set bits with List                    |
| `byte1` n1                    | Set a byte                            |
| `bytes` n1 n2                 | Set bytes from n1 to n2               |
| `mask` n1                     | Set bits from 0 to n1                 |


#### Extract and replace bits

| Operation                     | Description                           |
|:------------------------------|:--------------------------------------|
| `gets` x1 n1 n2               | Extract bits from n1 to n2            |
| `puts` x1 n1 n2 x2            | Replace bits from n1 to n2            |
|                               |                                       |
| `getBit1`  x1 n1              | Extract bit at n1                     |
| `getBits`  x1 n1 n2           | Synonym to gets                       |
| `getByte1` x1 n1              | Extract bytes from n1 to 0            |
| `getBytes` x1 n1 n2           | Extract bytes from n1 to n2           |
| `putBit1`  x1 n1 x2           | Replace byte at n1                    |
| `putBits`  x1 n1 n2 x2        | Synonym to puts                       |
| `putBytes` x1 n1 n2 x2        | Replace bytes from n1 to n2           |


#### Set and clear bits

| Operation                     | Description                           |
|:------------------------------|:--------------------------------------|
| `sbits` x1 n1 n2              | Set bits from n1 to n2 of x1          |
| `cbits` x1 n1 n2              | Clear bits from n1 to n2 of x1        |


#### Get asserted bit positions and count bits

| Operation                     | Description                           |
|:------------------------------|:--------------------------------------|
| `pos1` x1                     | Get bit positions asserted with 1     |
| `pos0` x1                     | Get bit positions asserted with 0     |
|                               |                                       |
| `range` x1                    | Get upper and lower boundaries        |
|                               |                                       |
| `count1` x1                   | Count bit-1                           |
| `count0` x1                   | Count bit-0                           |


#### Permute

| Operation                     | Description                           |
|:------------------------------|:--------------------------------------|
| `bitrev` x1                   | Reverse bits                          |
| `byterev` x1                  | Reverse bytes                         |
|                               |                                       |
| `gather` x1 x2                | Gather bits from x1 by x2             |
| `scatter` x1 x2 x3            | Scatter bits from x3 to x1 by x2      |


#### Split and merge

| Operation                     | Description                           |
|:------------------------------|:--------------------------------------|
| `splitBits` x1                | Split bits to List                    |
| `splitBytes` x1               | Split bytes to List                   |
| `mergeBits` [x1, x2, .. xn]   | Merge bits from List                  |
| `mergeBytes` [x1, x2, .. xn]  | Merge bytes from List                 |
|                               |                                       |
| `splitPairs` [n1, .. nn] x1   | Split bits to pair of (length,Hex)    |
| `mergePairs` [(n1,x1),..]     | Merge bits from pair of (length,Hex)  |
| (n1,x1) `.++` (n2,x2)         | Concatinate pairs of (length,Hex)     |


#### Predefined-constants

| Constant                      | Description                           |
|:------------------------------|:--------------------------------------|
| `exa`                         | 2^60 (It's not 10^18)                 |
| `peta`                        | 2^50 (It's not 10^15)                 |
| `tera`                        | 2^40 (It's not 10^12)                 |
| `giga`                        | 2^30 (It's not 10^9)                  |
| `mega`                        | 2^20 (It's not 10^6)                  |
| `kilo`                        | 2^10 (It's not 10^3)                  |
|                               |                                       |
| `zero`                        | 0                                     |
| `one`                         | 1                                     |
| `all0`                        | 0x0                                   |
| `all1`                        | inv all0                              |
|                               |                                       |
| `hexBitSize`                  | 64 on x86_64. Thus size of `Word`     |
| `hexBitSeq`                   | [hexBitSize-1, hexBitSize-2, .. 0]    |


#### Formatting for hex, bin, dec, Tera/Giga/Mega/Kilo and signed

| Operation                     | Description                           |
|:------------------------------|:--------------------------------------|
| `.@hex`                       | Show in hexadecimal string            |
| `.@hex8`                      | Show in hexadecimal string of 8bit    |
| `.@hex16`                     | Show in hexadecimal string of 16bit   |
| `.@hex32`                     | Show in hexadecimal string of 32bit   |
| `.@hex64`                     | Show in hexadecimal string of 64bit   |
| `.@hexN` n1                   | Show in hexadecimal string of n1 bit  |
|                               |                                       |
| `.@bin`                       | Show in binary string                 |
| `.@bin8`                      | Show in binary string of 8bit         |
| `.@bin16`                     | Show in binary string of 16bit        |
| `.@bin32`                     | Show in binary string of 32bit        |
| `.@bin64`                     | Show in binary string of 64bit        |
| `.@binN` n1                   | Show in binary string of n1 bit       |
|                               |                                       |
| `.@dec`                       | Show in decimal string                |
| `.@decT`                      | Show in decimal of Tera unit          |
| `.@decG`                      | Show in decimal of Giga unit          |
| `.@decM`                      | Show in decimal of Mega unit          |
| `.@decK`                      | Show in decimal of Kilo unit          |
|                               |                                       |
| `.@signed`                    | Show in singed decimal with `Word`    |


#### Pretty print

| Operation                     | Description                           |
|:------------------------------|:--------------------------------------|
| `color` x1 x2                 | Highlight bit of x1 specified with x2 |
| `ppr` f1 x1                   | Print x1 applied with f1              |


#### Input & convert

| Operation                     | Description                           |
|:------------------------------|:--------------------------------------|
| `inputRawHexIO`               | Input string and convert to Hex type  |


#### Miscellaneous

| Operation                     | Description                           |
|:------------------------------|:--------------------------------------|
| `cls`                         | Clear screen by ANSI sequence         |
| `usage`                       | Show simple help                      |


Appendix
--------

#### GHC language extention for numeric literals

When `-XBinaryLiterals` extention enabled, you can use binary literals on GHC and GHCi, like `0b1101`.

When `-XNumericUnderscores` extention enabled, you can use underscores in numeric literals on GHC and GHCi, like `0xff_ff`.
`-XNumericUnderscores` extension is available GHC 8.6 or later.

GHC language extensions can be described in `~/.ghci` or `./ghci` file:
```
:set -XBinaryLiterals
:set -XNumericUnderscores
```

GHC language extensions can also be specified as an option when starting GHC and GHCi:
```bash
$ ghci -XBinaryLiterals -XNumericUnderscores
```


#### Shell alias

It is useful to set the alias of the shell:
```bash
alias ghex="ghci  -haddock $XX/src/Data/GHex.hs"
```


#### Expression evaluation mode of GHC

You can also run in one shot mode (a expression evaluation mod) by `ghc -e`:
```bash
$ ghc -e '4 * giga'
0x0000_0004_0000_0000
```
