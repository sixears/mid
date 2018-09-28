{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

{-# OPTIONS_GHC -Wall #-}

module Fluffy.Parsec2
  ( (<$$>), (<||>), runPermutation )
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
addQ x a (Permutation seen e)
  = Permutation (Set.insert x seen) $ case e of
      Right f -> Right (f a)
      Left m -> Left (Map.map (fmap (addQ x a)) m)

-- | Convert a 'Permutation' to a 'Parser' that detects duplicates
-- and skips unknown identifiers.
runPermutation :: (Ord χ, Printable χ) => ParsecT String () Identity () -> ParsecT String () Identity χ -> Permutation α χ -> Parsec String () α
runPermutation skippy parseIdent p@(Permutation seen e)
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
           else skippy *> runPermutation skippy parseIdent p
         -- yes, it is
         Just prhs -> do
           -- parse the RHS to get a continuation Permutation
           -- and run it to parse rest of parameters
           (prhs <* newline) >>= runPermutation skippy parseIdent

-- that's all, folks! ----------------------------------------------------------
