{-# LANGUAGE FlexibleInstances #-}

module Fluffy.ToRational
  ( Rationat, ToRational( fromRational, toRational ) )
where

import Prelude  ( Fractional, fromIntegral )

-- base --------------------------------

import qualified  Prelude

import Data.Function    ( (.), id )
import Data.Ratio       ( Ratio, Rational, (%), denominator, numerator )
import Numeric.Natural  ( Natural )


--------------------------------------------------------------------------------

class ToRational α where
  toRational   :: α -> Rational
  fromRational :: Fractional c => α -> c
  fromRational = Prelude.fromRational . toRational

instance ToRational Rational where
  toRational = id

type Rationat = Ratio Natural

instance ToRational Rationat where
  toRational a = fromIntegral (numerator a) % fromIntegral (denominator a)

-- that's all, folks! ----------------------------------------------------------
