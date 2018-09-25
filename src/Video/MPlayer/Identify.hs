{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

{-# OPTIONS_GHC -Wall #-}

-- see https://stackoverflow.com/questions/52438196/how-to-parse-a-series-of-lines-with-only-a-few-interesting-ones-with-parsec-in

module Video.MPlayer.Identify
  ( Video( Video ), height, width, video )
where

-- base --------------------------------

import Control.Applicative    ( (<*), (<*>), (*>), (<|>), many, some )
import Control.Monad          ( (>>), (>>=), fail, return, void )
import Data.Char              ( Char )
import Data.Either            ( Either( Left, Right ), either )
import Data.Eq                ( Eq )
import Data.Function          ( ($), const )
import Data.Functor           ( (<$>), fmap )
import Data.Functor.Identity  ( Identity )
import Data.List              ( intercalate )
import Data.Maybe             ( Maybe( Just, Nothing ) )
import Data.Monoid            ( (<>) )
import Data.String            ( String )
import Numeric.Natural        ( Natural )
import Text.Read              ( read )
import Text.Show              ( Show )

-- containers --------------------------

import qualified  Data.Map.Lazy  as  Map
import qualified  Data.Set       as  Set

-- fluffy ------------------------------

import Fluffy.Parsec  ( Parsecable( parser ) )

-- lens --------------------------------

import Control.Lens.TH  ( makeLenses )

-- parsec ------------------------------

import Text.Parsec.Char        ( alphaNum, char, digit, letter, newline, noneOf )
import Text.Parsec.Combinator  ( eof )
import Text.Parsec.Prim        ( Parsec, ParsecT, Stream, unexpected )
import Text.Parsec.String      ( Parser )

--------------------------------------------------------------------------------

-- * Permutation parser for identifier settings

-- | General permutation parser for a type @a@.
data Permutation a = Permutation
  -- | "Seen" identifiers for flagging duplicates
  (Set.Set Identifier)
  -- | Either map of continuation parsers for more identifiers or a
  -- final value once we see eof.
  (Either (Map.Map Identifier (Parser (Permutation a))) a)

-- | Create a one-identifier 'Permutation' from a 'Parser'.
(<$$>) :: (a -> b) -> (Identifier, Parser a) -> Permutation b
f <$$> xq = Permutation Set.empty (Right f) <||> xq
infixl 2 <$$>

-- | Add a 'Parser' to a 'Permutation'.
(<||>) :: Permutation (a -> b) -> (Identifier, Parser a) -> Permutation b
p@(Permutation seen e) <||> (x, q)
  = Permutation seen (Left (Map.insert x q' m'))
  where
    q' = (\a -> addQ x a p) <$> q
    m' = case e of Right _ -> Map.empty
                   Left m -> Map.map (fmap (<||> (x, q))) m
infixl 1 <||>

-- | Helper to add a parsed component to a 'Permutation'.
addQ :: Identifier -> a -> Permutation (a -> b) -> Permutation b
addQ x a (Permutation seen e)
  = Permutation (Set.insert x seen) $ case e of
      Right f -> Right (f a)
      Left m -> Left (Map.map (fmap (addQ x a)) m)

-- | Convert a 'Permutation' to a 'Parser' that detects duplicates
-- and skips unknown identifiers.
runPermutation :: Permutation b -> ParsecT String () Identity b
runPermutation p@(Permutation seen e)
  = -- if end of file, return the final answer (or error)
    eof *>
    case e of
      Left m -> fail $
        "eof before " <> intercalate ", " (Map.keys m)
      Right a -> return a
  <|>
    -- otherwise, parse the identifier
    do k <- ident <* char '='
       -- is it one we're waiting for?
       case either (Map.lookup k) (const Nothing) e of
         -- no, it's not, so check for duplicates and skip
         Nothing -> if Set.member k seen
           then unexpected ("duplicate " <> k)
           else skipLine *> runPermutation p
         -- yes, it is
         Just prhs -> do
           -- parse the RHS to get a continuation Permutation
           -- and run it to parse rest of parameters
           (prhs <* newline) >>= runPermutation

-- | Left-hand side of a setting.

-- NEWTYPE
type Identifier = String

-- | Parse an 'Identifier'.
ident :: Parsec String u [Char]
ident = (:) <$> letter' <*> many alphaNum'
  where letter' = letter <|> underscore
        alphaNum' = alphaNum <|> underscore
        underscore = char '_'

-- | Skip (rest of) a line.
skipLine :: Stream s m Char => ParsecT s u m ()
skipLine = void $ many (noneOf "\n") >> newline

-- * Parsing video information

-- | Our video data.
data Video = Video { _width  :: Natural
                   , _height :: Natural
                   }
  deriving (Eq, Show)

$( makeLenses ''Video )

-- | Parsing naturals (RHS of width and height settings)
-- nat :: Parser Natural
nat :: Stream s m Char => ParsecT s u m Natural
nat = read <$> some digit

-- | `Video` parser based on `Permutation`.
-- video :: Parser Video
video :: ParsecT String () Identity Video
video = runPermutation $ Video <$$> ("ID_VIDEO_WIDTH" , nat)
                               <||> ("ID_VIDEO_HEIGHT", nat)

-- instance Parsecable Video where
--     parser = video
