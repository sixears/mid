{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

import Prelude  ( Integer, (/), floor, fromIntegral, mod, quot )
-- base --------------------------------

import Control.Applicative     ( (<*>), some )
import Control.Monad           ( Monad, (>>=), forM_, join, return )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bifunctor          ( first, second )
import Data.Bool               ( Bool, (&&) )
import Data.Char               ( Char )
import Data.Either             ( Either( Left, Right ), either )
import Data.Eq                 ( Eq, (==) )
import Data.Function           ( (.), ($), flip, id )
import Data.Functor            ( (<$>), fmap )
import Data.List               ( filter, sort )
import Data.Maybe              ( Maybe( Just, Nothing ) )
import Data.Monoid             ( (<>) )
import Data.Ord                ( (<) )
import Data.Tuple              ( fst )
import Numeric.Natural         ( Natural )
import System.IO               ( IO, print )
import Text.Show               ( Show( show ) )

-- fluffy ------------------------------

import Fluffy.IO.Error            ( AsIOError, userE )
import Fluffy.Lens                ( (##) )
import Fluffy.MonadError          ( splitMError )
import Fluffy.MonadIO.File        ( stat )
import Fluffy.Nat                 ( One, Two )
import Fluffy.Options             ( optParser )
import Fluffy.Parsec.Error        ( AsParseError( _ParseError ) )
import Fluffy.Parsec.Permutation  ( parsec_ )
import Fluffy.Path                ( AbsDir, AbsFile, AsFilePath( toFPath ), File
                                  , MyPath( resolve ), RelFile
                                  , getCwd_, parseFile'
                                  )

-- lens --------------------------------

import Control.Lens.Getter  ( (^.) )
import Control.Lens.TH      ( makeLenses )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )
import Control.Monad.Trans   ( lift )

-- optparse-applicative ----------------

import Options.Applicative  ( Parser, ReadM, argument, eitherReader, flag, help
                            , long, metavar, short )

-- path --------------------------------

import Path  ( Abs, Path )

-- proclib -----------------------------

import ProcLib.CommonOpt.DryRun       ( DryRunLevel
                                      , HasDryRunLevel( dryRunLevel )
                                      , dryRun2P
                                      )
import ProcLib.CommonOpt.Verbose      ( HasVerboseLevel( verboseLevel )
                                      , VerboseLevel, ifVerboseGE, verboseP )
import ProcLib.Process                ( doProcIO, mkProc_ )
import ProcLib.Types.CmdSpec          ( CmdSpec( CmdSpec ) )
import ProcLib.Types.ProcIO           ( ProcIO )

-- text --------------------------------

import Data.Text     ( Text, findIndex, isInfixOf, isPrefixOf, pack, splitAt
                     , tail, unlines )
import Data.Text.IO  ( putStrLn )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- unix --------------------------------

import System.Posix.Files  ( fileSize )
import System.Posix.Types  ( COff, FileOffset )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  Video.MPlayer.Paths  as  Paths

import Video.MPlayer.Identify.Error  ( ExecCreatePathIOParseError )
import Video.MPlayer.Types.Video     ( fps, height, lengthHours, lengthSecsNat
                                     , width )

--------------------------------------------------------------------------------

type FileSize = COff

-- XX USE DURATION TYPE (pre-existing?)

-- XX replace ExecCreatePathIOParseError with (As*Error)+
midentify :: MonadError ExecCreatePathIOParseError η =>
             Path Abs File -> ProcIO ExecCreatePathIOParseError η [Text]
midentify fn = let args = [ "-vo", "null", "-ao", "null", "-frames", "0"
                          , "-identify", pack (toFPath fn) ]
                   fstT :: ([Text],[Text]) -> [Text]
                   fstT = fst
                in fstT <$> (mkProc_ $ CmdSpec Paths.mplayer args)

handleE :: (Show ε) => Either ε () -> IO ()
handleE = either print return

data ShowAll = ShowAll | NoShowAll
  deriving Eq

data Options = Options { _fns      :: [Either AbsFile RelFile]
                       , _dryRunL  :: DryRunLevel  Two
                       , _verboseL :: VerboseLevel One
                       , _showAll  :: ShowAll
                       }
$( makeLenses ''Options )

instance HasVerboseLevel One Options where
  verboseLevel = verboseL

instance HasDryRunLevel Two Options where
  dryRunLevel = dryRunL

parseOpts :: Parser Options
parseOpts = let argMeta = metavar "FILE" <> help "file to query"
             in Options <$> some (argument fileReader argMeta)
                      <*> dryRun2P
                      <*> verboseP
                      <*> flag NoShowAll ShowAll
                             (short 'a' <> long "all"
                                        <> help "show all the info")

splitOne :: Char -> Text -> Maybe (Text,Text)
splitOne c t = second tail . flip splitAt t <$> findIndex (== c) t

filterIDs :: Text -> Bool
filterIDs t = "ID_" `isPrefixOf` t && "=" `isInfixOf` t

whenVerboseGE :: (HasVerboseLevel n φ, Monad η) => Natural -> φ -> η () -> η ()
whenVerboseGE n o a = ifVerboseGE n o a (return ())

fmtHMS :: Natural -> Text
fmtHMS s =
  if 60 < s
  then if 3600 < s
       then [fmt|%2dh%02dm%02ds|] (s `quot` 3600) ((s `quot` 60) `mod` 60) (s `mod` 60)
       else [fmt|   %02dm%02ds|] ((s `quot` 60) `mod` 60) (s `mod` 60)
  else [fmt|         %02ds|] s

fsize :: (MonadError ε μ, AsIOError ε, MonadIO μ) =>
         Path β File -> μ (Maybe FileOffset)
fsize = fmap (fmap fileSize) . stat

fileReader :: ReadM (Either AbsFile RelFile)
fileReader = eitherReader (first show . parseFile' . pack)

parsecMPI :: (MonadIO μ, AsParseError ε, MonadError ε μ) =>
               Text -> FileSize -> Text -> μ ()
parsecMPI fn sz idtxt =
  case {- parsecMPI -} parsec_ fn idtxt of
-- XX Fail/Exit here
-- XX add no-exit option
    Left  e -> do liftIO $ do putStrLn "FATAL: mplayer failed"
                              putStrLn (pack $ show e)
                  throwError $ _ParseError ## e
-- XX switch to using GiB every time, with a Fmt handler

    Right v -> liftIO . putStrLn $ [fmt|%t  %4dx%4d  %3.3ffps  %Y  (%Y/h)  %t|]
                                    (fmtHMS $ v ^. lengthSecsNat)
                                    (v ^. width)
                                    (v ^. height)
                                    (v ^. fps)
                                    sz
                                    ((floor $ fromIntegral sz / v ^. lengthHours) :: Integer)
                                    fn


doFile :: (MonadIO η, MonadError ExecCreatePathIOParseError η) =>
          Options -> AbsDir -> (Either AbsFile RelFile)
       -> ProcIO ExecCreatePathIOParseError η ()
doFile opts cwd f = do
  let fn = pack $ either toFPath toFPath f
      af = either id (resolve cwd) f
  out <- midentify af
  let nonSuch = [fmt|no such file: '%t'|] fn
  sz <- lift $ fsize af >>=
          \case Nothing -> throwError (userE nonSuch)
                Just z  -> return z
  let idtxt = unlines . sort $ filter filterIDs out
  if ( opts ^. showAll == ShowAll )
  then lift . liftIO $ putStrLn idtxt
--  else lift $ parsecMPI' fn sz idtxt
  else lift $ parsecMPI fn sz idtxt

main :: IO ()
main = do
  opts <- optParser "summarize video characteristics" parseOpts
  cwd :: AbsDir <- getCwd_
  forM_ (opts ^. fns) $ \ f -> join . fmap handleE . splitMError . doProcIO opts $ doFile opts cwd f

-- that's all, folks! ----------------------------

-- #!/bin/sh
-- #
-- # This is a wrapper around the -identify functionality.
-- # It is supposed to escape the output properly, so it can be easily
-- # used in shellscripts by 'eval'ing the output of this script.
-- #
-- # Written by Tobias Diedrich <ranma+mplayer@tdiedrich.de>
-- # Licensed under GNU GPL.
--
-- if [ -z "$1" ]; then
-- 	echo "Usage: midentify.sh <file> [<file> ...]"
-- 	exit 1
-- fi
--
-- mplayer -vo null -ao null -frames 0 -identify "$@" 2>/dev/null |
-- 	sed -ne '/^ID_/ {
-- 	                  s/[]()|&;<>`'"'"'\\!$" []/\\&/g;p
-- 	                }'


-- #!/usr/bin/perl
--
-- use 5.10.0;
-- use strict;
-- use warnings;
--
-- use FindBin              qw( $Bin );
-- use IPC::System::Simple  qw( capture );
--
-- my ($t_len_s, $t_size_b) = (0) x 2;
-- for my $a (@ARGV) {
--   my $midentify = capture "$Bin/midentify", $a;
--   my ($len_s)   = ($midentify =~ /^ID_LENGTH=(\d+)(?:\.\d+)?$/m);
--   my ($h)       = ($midentify =~ /^ID_VIDEO_HEIGHT=(\d+)$/m);
--   my ($w)       = ($midentify =~ /^ID_VIDEO_WIDTH=(\d+)$/m);
--   my ($fps)     = ($midentify =~ /^ID_VIDEO_FPS=([\d.]+)/m);
--   $t_size_b += my $size_b = -s $a;
--   $t_len_s += $len_s;
--   printf "%2dh%02dm%02ds  %-4dx%-4d  %3.2ffps  %6.3fGiB  (%5.3fGiB/h)  %s\n",
--          $len_s/(60*60),($len_s/60)%60,$len_s%60, $w, $h, $fps, $size_b / 1024**3,
--          $size_b / 1024**3 / ($len_s/(60*60)), $a;
-- }
--
-- printf "Total: %2dh%02dm%02ds  %5.3fGiB\n",
--        $t_len_s/(60*60), ($t_len_s/60)%60, $t_len_s%60, $t_size_b / 1_024**3;
--
-- --------------------------------------------------------------------------------
--
--
