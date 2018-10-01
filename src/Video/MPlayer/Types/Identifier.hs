{-# LANGUAGE FlexibleContexts #-}

module Video.MPlayer.Types.Identifier
  ( Identifier, ident )
where

-- base --------------------------------

import Control.Applicative    ( (<*>), (<|>), many )
import Data.Char              ( Char )
import Data.Eq                ( Eq )
import Data.Function          ( (.) )
import Data.Functor           ( (<$>) )
import Data.Functor.Identity  ( Identity )
import Data.Ord               ( Ord )
import Data.String            ( IsString( fromString ) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- fluffy ------------------------------

import Fluffy.Parsec  ( Parsecable( parser ) )

-- parsec ------------------------------

import Text.Parsec.Char   ( alphaNum, char, letter )
import Text.Parsec.Prim   ( Parsec, Stream )

-- text --------------------------------

import Data.Text  ( Text, pack )

--------------------------------------------------------------------------------

newtype Identifier = Identifier Text
  deriving (Eq, Ord)

instance IsString Identifier where
  fromString = Identifier . pack

instance Printable Identifier where
  print (Identifier t) = print t

instance Parsecable Identifier where
  parser = fromString <$> ((:) <$> letter' <*> many alphaNum')
           where letter'    = letter <|> underscore
                 alphaNum'  = alphaNum <|> underscore
                 underscore = char '_'

ident :: Stream s Identity Char => Parsec s () Identifier
ident = parser

-- that's all, folks! ----------------------------------------------------------
