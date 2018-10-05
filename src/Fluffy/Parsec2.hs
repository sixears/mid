{-# LANGUAGE FlexibleContexts #-}

module Fluffy.Parsec2
  ( float, line, nat )
where

import Prelude  ( Float )

-- base --------------------------------

import Control.Applicative    ( (<*), (<*>), many, some )
import Data.Char              ( Char )
import Data.Functor           ( (<$>) )
import Data.Monoid            ( (<>) )
import Numeric.Natural        ( Natural )
import Text.Read              ( read )

-- parsec ------------------------------

import Text.Parsec.Char        ( char, digit, newline, noneOf, string )
import Text.Parsec.Combinator  ( option )
import Text.Parsec.Prim        ( ParsecT, Stream )

-- text --------------------------------

import Data.Text  ( Text, pack )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Fluffy.Parsec  ( Parsecable( parser ) )

--------------------------------------------------------------------------------

-- | Parse a Natural number.
nat :: Stream s m Char => ParsecT s u m Natural
nat = read <$> some digit

instance Parsecable Natural where
  parser = nat

float :: Stream s m Char => ParsecT s u m Float
float = (\ m n d -> read (m <> n <> d)) <$> option "" (string "-") <*> some digit <*> option "" ((:) <$> char '.' <*> some digit)

instance Parsecable Float where
  parser = float

-- | Consume the rest of a line, up to the next newline.  The newline is consumed,
--   but not returned as part of the text.
line :: Stream s m Char => ParsecT s u m Text
line = pack <$> many (noneOf "\n") <* newline

-- that's all, folks! ----------------------------------------------------------
