<p align="left"><img src="http://takenobu-hs.github.io/downloads/images/haskell-logo-s.png"/></p>

GHCi as a Hex-Calculator interactive
====================================

This is an interactive hex-calculator using Haskell/GHCi.  
This is a simple and casual interactive tool like Perl and Excel for daily work.

Contents:

  * [Run](#run)
  * [Example](#example)
  * [Specification](#specification)
  * [Appendix](#appendix)


Run
---

Bare GHCi:
```bash
$ ghci src/Data/GHex.hs
```

Stack:
```bash
$ stack ghci -- src/Data/GHex.hs
```


Example
-------

#### Numeric literals by `Hex` type annotation

```
ghci> 1 :: Hex
0x0000_0000_0000_0001

ghci> 0xff :: Hex
0x0000_0000_0000_00ff

ghci> 0b1011 :: Hex
0x0000_0000_0000_000b

ghci> 16 + 3 :: Hex
0x0000_0000_0000_0013
```

#### Variables on GHCi

```
ghci> x = 255 :: Hex
ghci> x + 3
ghci> y = it
```

#### Arithmetic operations

```
ghci> x + 3
ghci> (x * 256) -1
ghci> x + 2^10
```

#### Logical operations

```
ghci> 0xff .& 6
ghci> 256 .| 16
ghci> 100 .^ 0b0101
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
ghci> bits 7 4
ghci> bitList [15, 14, 1]
ghci> byte1 2
ghci> bytes 4 3
```

#### Extract and replace bits
```
ghci> gets x 7 4
ghci> puts x 7 4 0b1101
```

#### Get asserted bit positions

```
ghci> pos1 x
ghci> pos0 y
ghci> x .@pos1
```

#### Predefined-constants

```
ghci> mega
ghci> giga
ghci> 4 * giga - 1
ghci> x ./ giga
```

#### Clear screen

```
ghci> cls
```

#### Postfix-notation for hex, bin, dec and Tera/Giga/Mega/Kilo formatting

```
ghci> 100 .@bin
ghci> 2^16 .@hex
ghci> 4 * giga .@dec
ghci> x .@decG
ghci> bit 43 .@decT
```

#### Combination examples

```
ghci> x .| bit1 18
ghci> (x .<< 4) .& 0xf0
ghci> bit1 33 ./ giga
ghci> 2 * mega .@dec
ghci> 4 * tera .@pos1
ghci> foldr (.|) [0xa, 0xcc, 0xd1b]
```

#### Using Data.Bits library

```
ghci> x `testBit` 8
ghci> x `clearBit` 15
ghci> x .&. 0xff
```

#### Simple help

```
ghci> usage
```

####  Execution by a expression evaluation mode of GHC (`ghc -e`)

```bash
$ ghc -e '4 * giga .@pos1'
```


Specification
-------------

#### General

* Core type:
  * The core type of this package is `Hex` type.
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


#### Arithmetic operations

| Operation                     | Description                           |
|:------------------------------|:--------------------------------------|
| `+`, `-`, `*`, `^`, ...       | Num, Real class available             |
| `neg` x1                      | Negation. (inv x1 + 1)                |
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
| `mask` n1                     | Set bits from 0 to n1                 |
| `byte1` n1                    | Set a byte                            |
| `bytes` n1 n2                 | Set bytes from n1 to n2               |


#### Get asserted bit positions

| Operation                     | Description                           |
|:------------------------------|:--------------------------------------|
| `pos1` x1                     | Get bit positions asserted with 1      |
| `pos0` x1                     | Get bit positions asserted with 0      |


#### Extract and replace bits

| Operation                     | Description                           |
|:------------------------------|:--------------------------------------|
| `gets` x1 n1 n2               | Extract bits from n1 to n2            |
| `puts` x1 n1 n2 x2            | Replace bits from n1 to n2            |
|                               |                                       |
| `getBits` x1 n1 n2            | Synonym to gets                       |
| `getBytes` x1 n1 n2           | Extract bytes from n1 to n2           |
| `putBits` x1 n1 n2 x2         | Synonym to puts                       |
| `putBytes` x1 n1 n2 x2        | Replace bytes from n1 to n2           |


#### Set and clear bits

| Operation                     | Description                           |
|:------------------------------|:--------------------------------------|
| `sbits` x1 n1 n2              | Set bits from n1 to n2 of x1          |
| `cbits` x1 n1 n2              | Clear bits from n1 to n2 of x1        |


#### Permute

| Operation                     | Description                           |
|:------------------------------|:--------------------------------------|
| `bitrev` x1                   | Reverse bits                          |
| `byterev` x1                  | Reverse bytes                         |


#### Split and merge

| Operation                     | Description                           |
|:------------------------------|:--------------------------------------|
| `splitBits` x1                | Split bits to List                    |
| `splitBytes` x1               | Split bytes to List                   |
| `mergeBits` [x1, x2, .. xn]   | Merge bits from List                  |
| `mergeBytes` [x1, x2, .. xn]  | Merge bytes from List                 |


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


#### Postfix-notation for hex, bin, dec and T/G/M/K unit formatting

| Operation                     | Description                           |
|:------------------------------|:--------------------------------------|
| `.@hex`                       | Show in hexadecimal string            |
| `.@bin`                       | Show in binary string                 |
| `.@dec`                       | Show in decimal string                |
| `.@decT`                      | Show in decimal of Tera unit          |
| `.@decG`                      | Show in decimal of Giga unit          |
| `.@decM`                      | Show in decimal of Mega unit          |
| `.@decK`                      | Show in decimal of Kilo unit          |
|                               |                                       |
| `.@hex8`                      | Show in binary string of 8bit         |
| `.@hex16`                     | Show in binary string of 16bit        |
| `.@hex32`                     | Show in binary string of 32bit        |
| `.@hex64`                     | Show in binary string of 64bit        |
| `.@hexN` n1                   | Show in binary string of n1 bit       |
| `.@bin8`                      | Show in binary string of 8bit         |
| `.@bin16`                     | Show in binary string of 16bit        |
| `.@bin32`                     | Show in binary string of 32bit        |
| `.@bin64`                     | Show in binary string of 64bit        |
| `.@binN` n1                   | Show in binary string of n1 bit       |
|                               |                                       |
| `.@signed`                    | Show in singed decimal with `Word`    |


#### Miscellaneous

| Operation                     | Description                           |
|:------------------------------|:--------------------------------------|
| `cls`                         | Clear screen on VT100 terminal       |
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
alias ghex="ghci $XX/src/Data/GHex.hs"
```


#### Expression evaluation mode of GHC

You can also run in one shot mode (a expression evaluation mod) by `ghc -e`:
```bash
$ ghc -e '4 * giga'
0x0000_0004_0000_0000
```
