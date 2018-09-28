{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wall #-}

-- see https://stackoverflow.com/questions/52438196/how-to-parse-a-series-of-lines-with-only-a-few-interesting-ones-with-parsec-in

module Video.MPlayer.Identify
  ( Video( Video ), height, parsecV, videoP, width )
where

-- base --------------------------------

import Control.Applicative    ( (<*), (<*>), (*>), (<|>), many, some )
import Control.Monad          ( (>>), (>>=), fail, return, void )
import Data.Char              ( Char )
import Data.Either            ( Either( Left, Right ), either )
import Data.Eq                ( Eq )
import Data.Function          ( (.), ($), const )
import Data.Functor           ( (<$>), fmap )
import Data.Functor.Identity  ( Identity )
import Data.List              ( intercalate )
import Data.Maybe             ( Maybe( Just, Nothing ) )
import Data.Monoid            ( (<>) )
import Data.Ord               ( Ord )
import Data.String            ( IsString( fromString ), String )
import Numeric.Natural        ( Natural )
import Text.Show              ( Show )

-- containers --------------------------

import qualified  Data.Map.Lazy  as  Map
import qualified  Data.Set       as  Set

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

import Text.Parsec.Char        ( alphaNum, char, digit, letter, newline, noneOf )
import Text.Parsec.Combinator  ( eof )
import Text.Parsec.Error       ( ParseError )
import Text.Parsec.Prim        ( Parsec, ParsecT, Stream, parse, unexpected )
import Text.Parsec.String      ( Parser )

-- path --------------------------------

import Path  ( Path, File, toFilePath )

-- text --------------------------------

import Data.Text  ( Text, pack, unpack )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Video.MPlayer.Types.Video  ( Video( Video )
                                  , height, parsecV, videoP, width )

--------------------------------------------------------------------------------
{-

-- | Left-hand side of a setting.
-- | Parse an 'Identifier'.
ident :: Stream s m Char => ParsecT s u m Identifier
ident = fromString <$> ((:) <$> letter' <*> many alphaNum')
  where letter' = letter <|> underscore
        alphaNum' = alphaNum <|> underscore
        underscore = char '_'

-- | Skip (rest of) a line.
skipLine :: Stream s m Char => ParsecT s u m ()
skipLine = void $ many (noneOf "\n") >> newline

-}
-- that's all, folks! ----------------------------------------------------------
