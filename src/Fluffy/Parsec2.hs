{-# LANGUAGE FlexibleContexts #-}

module Fluffy.Parsec2
  ( line, nat )
where

-- base --------------------------------

import Control.Applicative    ( (<*), many, some )
import Data.Char              ( Char )
import Data.Functor           ( (<$>) )
import Numeric.Natural        ( Natural )
import Text.Read              ( read )

-- parsec ------------------------------

import Text.Parsec.Char   ( digit, newline, noneOf )
import Text.Parsec.Prim   ( ParsecT, Stream )

-- text --------------------------------

import Data.Text  ( Text, pack )

--------------------------------------------------------------------------------

-- | Parse a Natural number.
nat :: Stream s m Char => ParsecT s u m Natural
nat = read <$> some digit

-- | Consume the rest of a line, up to the next newline.  The newline is consumed,
--   but not returned as part of the text.
line :: Stream s m Char => ParsecT s u m Text
line = pack <$> many (noneOf "\n") <* newline

-- that's all, folks! ----------------------------------------------------------
