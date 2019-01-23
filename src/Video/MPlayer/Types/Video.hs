{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

{-# OPTIONS_GHC -Wall #-}

module Video.MPlayer.Types.Video
  ( Video, filename, fps, height, duration, video, width )
where

import Prelude  ( Float )

-- base --------------------------------

import Control.Applicative    ( (<*), (<*>), some )
import Control.Monad          ( (>>=), return, void )
import Data.Eq                ( Eq )
import Data.Function          ( ($) )
import Data.Functor           ( (<$>) )
import Data.Monoid            ( mappend )
import Data.Ratio             ( Rational )
import Data.String            ( String )
import Numeric                ( readFloat )
import Numeric.Natural        ( Natural )
import Text.Show              ( Show )

-- fluffy ------------------------------

import Fluffy.Duration            ( Duration, fromS )
import Fluffy.Parsec2             ( line )
import Fluffy.Parsec              ( Parsecable( parser ) )
import Fluffy.Parsec.Permutation  ( ParsecableP( parserP )
                                  , (<$$>),(<||>), runPermutation )

-- lens --------------------------------

import Control.Lens.TH    ( makeLenses )

-- parsec ------------------------------

import Text.Parsec.Char        ( char, digit, newline, noneOf )
import Text.Parsec.Combinator  ( option )
import Text.Parsec.Prim        ( Parsec, parserFail )

-- text --------------------------------

import Data.Text  ( Text, pack )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Video.MPlayer.Types.Identifier  ( ident )

--------------------------------------------------------------------------------

data Video = Video { _width    :: Natural
                   , _height   :: Natural
                   , _duration :: Duration
                   , _fps      :: Float
                   , _filename :: Text
                   }
  deriving (Eq, Show)

$( makeLenses ''Video )

parseRational :: Parsec String () Rational
parseRational =
  let parseOptDec = option "" ((:) <$> char '.' <*> some digit)
   in (mappend <$> some digit <*> parseOptDec) >>= parseFloat
      where {- ambigMsg s x y = "ambiguous parse as Rational: '" <> s
                          <> "' could be «" <> show x
                          <> "» or «" <> show y <> "» (or maybe others)" -}
            ambigMsg s x y = let a = "ambiguous parse as Rational"
                                 b = "could be"
                                 c = "(or maybe others)"
                              in [fmt|%s: '%s' %s «%w» or «%w» %s|] a s b x y c
            failedMsg = [fmt|failed to parse as Rational: '%s'|]
            incompMsg = [fmt|incomplete parse as Rational (%w) '%s' leaving «%w»|]
            parseFloat :: String -> Parsec String () Rational
            parseFloat s = case readFloat s of
                             [(r ::Rational, "")] -> return r
                             [(r ::Rational, x)]  -> parserFail $ incompMsg r s x
--                             [(x,_) : (y,_)]      -> parserFail $ ambigMsg s x y
                             ((x,_) : (y,_) :_)   -> parserFail $ ambigMsg s x y
                             []                   -> parserFail $ failedMsg s

instance ParsecableP Video where
  parserP = let runP = runPermutation (void line) newline (ident <* char '=')
             in runP $ Video <$$> ("ID_VIDEO_WIDTH" , parser)
                             <||> ("ID_VIDEO_HEIGHT", parser)
                             <||> ("ID_LENGTH",       fromS <$> parseRational)
                             <||> ("ID_VIDEO_FPS",    parser)
                             <||> ("ID_FILENAME",     pack <$> some (noneOf "\n"))

video :: Natural -> Natural -> Duration -> Float -> Text -> Video
video = Video

-- that's all, folks! ----------------------------------------------------------
