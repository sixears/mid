{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -Wall #-}

module Video.MPlayer.Types.Video
  ( Video( Video ), height, width )
where

-- base --------------------------------

import Control.Applicative    ( (<*) )
import Control.Monad          ( void )
import Data.Eq                ( Eq )
import Data.Function          ( ($) )
import Numeric.Natural        ( Natural )
import Text.Show              ( Show )

-- fluffy ------------------------------

import Fluffy.Parsec2             ( line, nat )
import Fluffy.Parsec.Permutation  ( Parsecable_( parser_ )
                                  , (<$$>),(<||>), runPermutation )

-- lens --------------------------------

import Control.Lens.TH  ( makeLenses )

-- parsec ------------------------------

import Text.Parsec.Char   ( char, newline )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Video.MPlayer.Types.Identifier  ( ident )

--------------------------------------------------------------------------------

data Video = Video { _width  :: Natural
                   , _height :: Natural
                   }
  deriving (Eq, Show)

$( makeLenses ''Video )

instance Parsecable_ Video where
  parser_ = let runP = runPermutation (void line) newline (ident <* char '=')
             in runP $ Video <$$> ("ID_VIDEO_WIDTH" , nat)
                             <||> ("ID_VIDEO_HEIGHT", nat)

-- that's all, folks! ----------------------------------------------------------
