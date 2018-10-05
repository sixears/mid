{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -Wall #-}

module Video.MPlayer.Types.Video
  ( Video, filename, fps, height, lengthHours, lengthHoursNat, lengthSecs
  , lengthSecsNat, video, width )
where

import Prelude  ( Float, (*), (/), floor, fromIntegral )

-- base --------------------------------

import Control.Applicative    ( (<*), some )
import Control.Monad          ( void )
import Data.Eq                ( Eq )
import Data.Function          ( (.), ($), const )
import Data.Functor           ( (<$>) )
import Numeric.Natural        ( Natural )
import Text.Show              ( Show )

-- fluffy ------------------------------

import Fluffy.Parsec2             ( line )
import Fluffy.Parsec              ( Parsecable( parser ) )
import Fluffy.Parsec.Permutation  ( Parsecable_( parser_ )
                                  , (<$$>),(<||>), runPermutation )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens', lens )
import Control.Lens.TH    ( makeLenses )

-- parsec ------------------------------

import Text.Parsec.Char   ( char, newline, noneOf )

-- text --------------------------------

import Data.Text  ( Text, pack )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Video.MPlayer.Types.Identifier  ( ident )

--------------------------------------------------------------------------------

data Video = Video { _width      :: Natural
                   , _height     :: Natural
                   , _lengthSecs :: Float
                   , _fps        :: Float
                   , _filename   :: Text
                   }
  deriving (Eq, Show)

$( makeLenses ''Video )

instance Parsecable_ Video where
  parser_ = let runP = runPermutation (void line) newline (ident <* char '=')
             in runP $ Video <$$> ("ID_VIDEO_WIDTH" , parser)
                             <||> ("ID_VIDEO_HEIGHT", parser)
                             <||> ("ID_LENGTH",       parser)
                             <||> ("ID_VIDEO_FPS",    parser)
                             <||> ("ID_FILENAME",     (pack <$> some (noneOf "\n")))

video :: Natural -> Natural -> Float -> Float -> Text -> Video
video = Video

lensFloatNat :: Lens' Float Natural
lensFloatNat = lens floor (const fromIntegral)

lengthSecsNat :: Lens' Video Natural
lengthSecsNat = lengthSecs . lensFloatNat

lengthHours :: Lens' Video Float
lengthHours = lengthSecs . (lens (/ 3600) (const (* 3600)))

lengthHoursNat :: Lens' Video Natural
lengthHoursNat = lengthHours . lensFloatNat

-- that's all, folks! ----------------------------------------------------------
