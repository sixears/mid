{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}

module Fluffy.Duration
  ( Duration, fromS, fromSi, hms, h, hours, s, seconds )
where

import Prelude ( Integral, Integer, Num, Real
               , (^), (*), (/)
               , floor, fromIntegral, quotRem, toRational
               )

-- base --------------------------------

import Data.Eq          ( Eq )
import Data.Function    ( (.), ($), const )
import Data.Ord         ( (>) )
import Data.Ratio       ( Rational )
import Data.Word        ( Word8, Word64 )
import Text.Show        ( Show )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- lens --------------------------------

import Control.Lens     ( Lens', lens )
import Control.Lens.TH  ( makeLenses )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tmft --------------------------------

import Text.Fmt  ( fmt )

--------------------------------------------------------------------------------

_NANOS :: Num α => α -- Word64
_NANOS = 10^ (9 :: Word64)

_ONE_HOUR_S :: Num α => α
_ONE_HOUR_S = 60*60

-- | unsigned time duration in seconds (as a Rational)
newtype Duration = Duration { _s :: Rational }
  deriving (Eq, Num, Show)

$( makeLenses ''Duration )

-- | construct a `Duration` from a `Real` number of seconds
fromS ::  Real α => α -> Duration
fromS = Duration . toRational

-- | construct a `Duration` from an `Integral` number of seconds
fromSi ::  Integral α => α -> Duration
fromSi = Duration . fromIntegral

seconds :: Lens' Duration Rational
seconds = s

-- | Duration as a number of hours
hours :: Lens' Duration Rational
hours = s . lens (/ 3600) (const $ (* 3600))

h :: Lens' Duration Rational
h = hours

hms :: (Integral α, Num β, Num γ) => Duration -> (α, β, γ)
hms (Duration s') = let (h',m_) = floor s' `quotRem` 3600
                        (m,s_)  = m_ `quotRem` 60
                     in (h',fromIntegral m,fromIntegral s_)

hms' :: Duration -> (Integer, Word8, Word8)
hms' = hms

instance Printable Duration where
  print d = let (h_,m_,s_) = hms' d
             in P.text $ if h_ > 0
                         then [fmt|%2dh%02dm%02ds|] h_ m_ s_
                         else if m_ > 0
                              then [fmt|%dm%02ds|] m_ s_
                              else [fmt|%ds|] s_

-- that's all, folks! ----------------------------------------------------------
