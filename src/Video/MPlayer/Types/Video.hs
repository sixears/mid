{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -Wall #-}

module Video.MPlayer.Types.Video
  ( Video( Video ), height, parsecV, videoP, width )
where

-- base --------------------------------

import Control.Applicative    ( (<*), (<*>), (*>), (<|>), many, some )
import Control.Monad          ( (>>), (>>=), return, void )
import Data.Char              ( Char )
import Data.Either            ( Either( Left, Right ), either )
import Data.Eq                ( Eq )
import Data.Function          ( (.), ($) )
import Data.Functor           ( (<$>) )
import Data.Functor.Identity  ( Identity )
import Data.Ord               ( Ord )
import Data.String            ( IsString( fromString ), String )
import Numeric.Natural        ( Natural )
import Text.Read              ( read )
import Text.Show              ( Show )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toString )

-- fluffy ------------------------------

import Fluffy.IO.Error      ( AsIOError )
import Fluffy.Lens          ( (##) )
import Fluffy.MonadIO       ( MonadIO, readFile )
import Fluffy.Parsec        ( Parsecable( parser ) )
import Fluffy.Parsec.Error  ( AsParseError( _ParseError ), IOParseError )

-- lens --------------------------------

import Control.Lens.TH  ( makeLenses )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- parsec ------------------------------

import Text.Parsec.Char   ( alphaNum, char, digit, letter, newline, noneOf )
import Text.Parsec.Error  ( ParseError )
import Text.Parsec.Prim   ( Parsec, ParsecT, Stream, parse, unexpected )

-- path --------------------------------

import Path  ( Path, File, toFilePath )

-- text --------------------------------

import Data.Text  ( Text, pack, unpack )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Fluffy.Parsec2  ( (<$$>), (<||>), runPermutation )

--------------------------------------------------------------------------------


newtype Identifier = Identifier Text
  deriving (Eq, Ord)

instance IsString Identifier where
  fromString = Identifier . pack
  
instance Printable Identifier where
  print (Identifier t) = print t

-- nat :: Parser Natural
nat :: Stream s m Char => ParsecT s u m Natural
nat = read <$> some digit


data Video = Video { _width  :: Natural
                   , _height :: Natural
                   }
  deriving (Eq, Show)

$( makeLenses ''Video )

-- | Parse an 'Identifier'.
ident :: Stream s m Char => ParsecT s u m Identifier
ident = fromString <$> ((:) <$> letter' <*> many alphaNum')
  where letter' = letter <|> underscore
        alphaNum' = alphaNum <|> underscore
        underscore = char '_'

-- | Skip (rest of) a line.
skipLine :: Stream s m Char => ParsecT s u m ()
skipLine = void $ many (noneOf "\n") >> newline

-- video :: Parser Video
videoP :: Parsec String () Video
videoP = runPermutation skipLine (ident <* char '=') $ Video <$$> ("ID_VIDEO_WIDTH" , nat)
                                <||> ("ID_VIDEO_HEIGHT", nat)

parsecV :: (AsParseError ε, MonadError ε η, Printable τ) =>
          τ -> Text -> η Video
parsecV sourceName t = case parse videoP (toString sourceName) (unpack t) of
                        Left  e -> throwError (_ParseError ## e)
                        Right s -> return s

parsecV' :: (MonadError ParseError η, Printable τ) =>
           τ -> Text -> η Video
parsecV' = parsecV

parsecFileV :: (MonadIO μ, AsIOError ε, AsParseError ε, MonadError ε μ) =>
              Path β File -> μ Video
parsecFileV fn = readFile fn >>= parsecV (pack $ toFilePath fn)

parsecFileV' :: (MonadIO μ, MonadError IOParseError μ) => Path β File -> μ Video
parsecFileV' = parsecFileV

-- that's all, folks! ----------------------------------------------------------
