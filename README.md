<p align="left"><img src="http://takenobu-hs.github.io/downloads/images/haskell-logo-s.png"/></p>

GHCi as a Hex-Calculator interactive
====================================

This is an interactive hex-calculator on GHCi.  
This is a simple and casual interactive tool like Perl and Excel for daily work.

Contents:

  * [Run](#Run)
  * [Example](#Example)
  * [API](#API)


Run
---

On GHC:
```bash
$ ghci src/Data/GHex.hs
```

On Stack:
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

#### Setting bits and bytes

```
ghci> bit1 15
ghci> bits 7 4
ghci> bitList [15, 14, 1]
ghci> byte1 2
ghci> bytes 4 3
```

#### Extract bits

```
ghci> gets x 7 4
```

#### Get asserted bit positions

```
ghci> pos1 x
ghci> pos0 y
ghci> x .@pos1
```

#### Preset-constants

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


API
---
