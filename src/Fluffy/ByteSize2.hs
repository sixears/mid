{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ViewPatterns               #-}

module Fluffy.ByteSize2
  ( ByteSize, bytes, fileSizeBytes, formatBytes, gibibytes, gib )
where

import Prelude  ( Double, Integer, Integral, Num
                , (+), (-), (*), (/), (^), abs, floor, fromInteger, fromIntegral
                , signum, toInteger
                )

-- base --------------------------------

import qualified  Data.Ratio  as  Ratio

import Data.Char        ( Char, toUpper )
import Data.Eq          ( Eq, (==) )
import Data.Function    ( (.), ($) )
import Data.Maybe       ( Maybe( Just, Nothing ) )
import Data.Ord         ( Ord( compare ), Ordering( EQ ), (<), min )
import Data.Word        ( Word8 )
import Numeric          ( logBase )
import Numeric.Natural  ( Natural )
import Text.Show        ( Show )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- formatting --------------------------

import qualified  Formatting.Formatters  as  Formatters

import Formatting             ( (%), sformat )
import Formatting.Formatters  ( Buildable, fixed, int )

-- lens --------------------------------

import Control.Lens.Getter  ( (^.) )
import Control.Lens.TH      ( makeLenses )

-- text --------------------------------

import Data.Text  ( Text )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Fluffy.ToRational  ( Rationat )

--------------------------------------------------------------------------------

data BytesExponent = Bytes
  deriving (Eq, Show)

instance Ord BytesExponent where
  compare Bytes Bytes = EQ
--  compare Bytes _     = LT

mult :: BytesExponent -> BytesExponent -> (Natural, BytesExponent)
mult Bytes Bytes = (1, Bytes)

data ByteSize = ByteSize { __mantissa :: Natural, _exponent :: BytesExponent }
  deriving Show

$( makeLenses ''ByteSize )

fileSizeBytes :: Natural -> ByteSize
fileSizeBytes n = ByteSize n Bytes

bytes :: ByteSize -> Natural
bytes (ByteSize m Bytes) = m

gibibytes :: ByteSize -> Rationat
gibibytes (ByteSize m Bytes) = (Ratio.%) m (1024 ^ (3 :: Natural))

gib :: ByteSize -> Rationat
gib = gibibytes

-- | PARTIAL: Convert a mantissa to a new exponent.  Not defined if the given
--            exponent is larger than the exponent of the ByteSize.
__convert__ :: ByteSize -> BytesExponent -> Natural
__convert__ (ByteSize m Bytes) Bytes = m

instance Num ByteSize where
  -- let Natural underflow for -ve n propagate
  fromInteger n = ByteSize (fromInteger n) Bytes

  abs f = f

  signum _ = ByteSize 1 Bytes

  a + b =  let e = min (a ^. exponent) (b ^. exponent)
               m = __convert__ a e + __convert__ b e
            in ByteSize m e

  a - b =  let e = min (a ^. exponent) (b ^. exponent)
               m = __convert__ a e - __convert__ b e
            in ByteSize m e

  (ByteSize am ae) * (ByteSize bm be) = let (mm, e) = ae `mult` be
                                         in ByteSize (am*bm*mm) e

{- | whether to format a bytes value in terms of powers of 10^3, or 2^10 -}
data ByteFmtBase = B_1000 | B_1024
  deriving Eq

-- | try really hard to fit within 7 chars
formatBytes :: (Buildable a, Integral a) => ByteFmtBase -> a -> Text
formatBytes _ (toInteger -> 0) = "0"
formatBytes b bs =
    case b of
      B_1000 -> go 1000 bs -- (byteSize bs)
      B_1024 -> go 1024 bs -- (fromIntegral $ byteSize bs)
    where go :: (Buildable b, Integral b) => Double -> b -> Text
          go x btes =
            let ex :: Word8 = floor (logBase x $ fromIntegral btes)
                (pfx,exp) :: (Maybe Char, Word8)= case ex of
                              0 -> (Nothing,  0)
                              1 -> (Just 'k', 1)
                              2 -> (Just 'M', 2)
                              3 -> (Just 'G', 3)
                              4 -> (Just 'T', 4)
                              5 -> (Just 'P', 5)
                              6 -> (Just 'E', 6)
                              7 -> (Just 'Z', 7)
                              _ -> (Just 'Y', 8)
                formatB n = fixed n % Formatters.char % Formatters.string % "B"
                i = if b == B_1024 then "i" else ""
             in case pfx of
                 Nothing -> sformat (int % "B") btes
                 Just c  -> let mant = fromIntegral btes / (x^exp)
                                c_   = if b == B_1024 then toUpper c else c
                             in if mant < 10
                                then -- [fmt|%3.2f%T%sB|]
                                     sformat (formatB 2) mant c_ i
                                else if mant < 100
                                     then -- [fmt|%4.1f%T%sB|]
                                          sformat (formatB 1) mant (toUpper c) i
                                     else -- [fmt|%4f%T%sB|]
                                          sformat (formatB 0) mant (toUpper c) i

instance Printable ByteSize where
  print = P.text . formatBytes B_1024 . bytesI
          where bytesI :: ByteSize -> Integer
                bytesI = fromIntegral . bytes


-- that's all, folks! ----------------------------------------------------------
