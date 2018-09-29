{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

{-# OPTIONS_GHC -Wall #-}

module Fluffy.Parsec.Permutation
  ( (<$$>), (<||>), runPermutation )
where

-- | A framework parser for permutable strings, where the input is a set of
--   recognizable phrases, e.g.,
--
--     ID_FOO=1
--     ID_BAR="quux"
--     ID_BAZ=10:35
--
--   that may arrive in any order; you wish to parse certain phrases (e.g., the
--   (ID_FOO & ID_BAZ above); while skipping the rest, and causing parse errors
--   on missing or duplicated phrases.
--
--   The parser constucted with `runPermutation` is parameterized on the
--   identifier parser ("ID_" above), the skip phrase (to skip anything after
--   an identifier we don't care about), and the terminator (the newline after
--   any value that we *did* parse).

-- base --------------------------------

import Control.Applicative    ( (<*), (*>), (<|>) )
import Control.Monad          ( (>>=), fail, return )
import Data.Either            ( Either( Left, Right ), either )
import Data.Function          ( ($), const )
import Data.Functor           ( (<$>), fmap )
import Data.Functor.Identity  ( Identity )
import Data.Maybe             ( Maybe( Just, Nothing ) )
import Data.Ord               ( Ord )
import Data.String            ( String )

-- containers --------------------------

import qualified  Data.Map.Lazy  as  Map
import qualified  Data.Set       as  Set

-- data-textual ------------------------

import Data.Textual  ( Printable )

-- parsec ------------------------------

import Text.Parsec.Combinator  ( eof )
import Text.Parsec.Prim        ( Parsec, ParsecT, unexpected )
import Text.Parsec.String      ( Parser )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

--------------------------------------------------------------------------------

-- * Permutation parser for identifier settings

-- | General permutation parser for a type @α@.
data Permutation α χ = Permutation
  -- | "Seen" identifiers for flagging duplicates
  (Set.Set χ)
  -- | Either map of continuation parsers for more identifiers or α
  -- final value once we see eof.
  (Either (Map.Map χ (Parser (Permutation α χ))) α)

-- | Create a one-identifier 'Permutation' from a 'Parser'.
(<$$>) :: Ord χ => (α -> β) -> (χ, Parser α) -> Permutation β χ
f <$$> xq = Permutation Set.empty (Right f) <||> xq
infixl 2 <$$>

-- | Add a 'Parser' to a 'Permutation'.
(<||>) :: Ord χ => Permutation (α -> β) χ -> (χ, Parser α) -> Permutation β χ
p@(Permutation seen e) <||> (x, q)
  = Permutation seen (Left (Map.insert x q' m'))
  where
    q' = (\a -> addQ x a p) <$> q
    m' = case e of Right _ -> Map.empty
                   Left m -> Map.map (fmap (<||> (x, q))) m
infixl 1 <||>

-- | Helper to add a parsed component to a 'Permutation'.
addQ :: Ord χ => χ -> α -> Permutation (α -> β) χ -> Permutation β χ
addQ x a (Permutation seen e) = Permutation (Set.insert x seen) $
                                  case e of
                                    Right f -> Right (f a)
                                    Left m -> Left (Map.map (fmap (addQ x a)) m)

-- | Convert a 'Permutation' to a 'Parser' that detects duplicates
--   and skips unknown identifiers.
--
--   `parseIdent` is a parser to produce an identifier; if it's one of the
--   recognized ones, we handle that, and discard the identTerm; else we check
--   for duplicates, and else we skip whatever is parsed by `skippy`.
runPermutation :: (Ord χ, Printable χ) =>
                  ParsecT String () Identity () -- if we see an identifier but
                                                -- don't parse its value, this
                                                -- skips the unparsed section
               -> ParsecT String () Identity δ  -- if we see an identifier and
                                                -- do parse its value, this
                                                -- skips anything after the
                                                -- value (e.g., newline) until
                                                -- we start parsing again
               -> ParsecT String () Identity χ  -- this is used to parse an
                                                -- identifier
               -> Permutation α χ -> Parsec String () α
runPermutation skippy identTerm parseIdent p@(Permutation seen e)
  = -- if end of file, return the final answer (or error)
    eof *>
    case e of
      Left m  -> fail $ [fmt|eof before %L|] (Map.keys m)
      Right a -> return a
  <|>
    -- otherwise, parse the identifier
    do k <- parseIdent -- ident <* char '='
       -- is it one we're waiting for?
       case either (Map.lookup k) (const Nothing) e of
         -- no, it's not, so check for duplicates and skip
         Nothing -> if Set.member k seen
                    then unexpected ([fmt|duplicate %T|] k)
                    else skippy *> runPermutation skippy identTerm parseIdent p
         -- yes, it is
         Just prhs -> do
           -- parse the RHS to get a continuation Permutation
           -- and run it to parse rest of parameters
           (prhs <* identTerm) >>= runPermutation skippy identTerm parseIdent

-- that's all, folks! ----------------------------------------------------------
