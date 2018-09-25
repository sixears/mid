{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-- base --------------------------------

import Control.Applicative     ( (<*>), (*>) )
import Control.Monad           ( (>>), forM_, join, return )
import Control.Monad.IO.Class  ( liftIO )
import Data.Bifunctor          ( second )
import Data.Bool               ( (&&) )
import Data.Char               ( Char )
import Data.Either             ( Either( Left, Right ), either )
import Data.Eq                 ( (==) )
import Data.Function           ( (.), ($), (&), flip )
import Data.Functor            ( (<$>), fmap )
import Data.List               ( filter, sort )
import Data.Maybe              ( Maybe, catMaybes )
import Data.Monoid             ( (<>) )
import Numeric.Natural         ( Natural )
import System.IO               ( IO, print )
import Text.Read               ( read )
import Text.Show               ( Show( show ) )

-- fluffy ------------------------------

import Fluffy.MonadError    ( splitMError )
import Fluffy.Nat           ( One, Two )
import Fluffy.Options       ( optParser )
import Fluffy.Parsec        ( Parsecable( parser ), parsec )
import Fluffy.Parsec.Error  ( )

-- lens --------------------------------

import Control.Lens.Setter  ( (.~) )
import Control.Lens.TH      ( makeLenses )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )
import Control.Monad.Trans   ( lift )

-- optparse-applicative ----------------

import Options.Applicative  ( Parser )

-- parsec ------------------------------

import Text.Parsec.Char   ( digit, string )
import Text.Parsec.Error  ( ParseError )
import Text.Parsec.Prim   ( many )

-- proclib -----------------------------

import ProcLib.CommonOpt.DryRun       ( DryRunLevel
                                      , HasDryRunLevel( dryRunLevel )
                                      , dryRun2P
                                      )
import ProcLib.CommonOpt.Verbose      ( HasVerboseLevel( verboseLevel )
                                      , VerboseLevel, verboseP )
import ProcLib.Error.ExecCreateError  ( ExecCreateError )
import ProcLib.Process                ( doProcIO, mkIO, mkIO', mkProc'_
                                      , mkProc_ )
import ProcLib.Types.CmdSpec          ( CmdSpec( CmdSpec ) )
import ProcLib.Types.CreateProcOpts   ( MockLvl( MockLvl ), defCPOpts, mockLvl )
import ProcLib.Types.ProcIO           ( ProcIO )

-- text --------------------------------

import Data.Text     ( Text, findIndex, isInfixOf, isPrefixOf, pack, splitAt
                     , tail, unlines )
import Data.Text.IO  ( putStrLn )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  Video.MPlayer.Paths  as  Paths

--------------------------------------------------------------------------------

env :: (MonadError ExecCreateError η) => ProcIO ExecCreateError η ([Text],[Text])
env = mkProc_ $ CmdSpec Paths.mplayer [ "-vo", "null", "-ao", "null", "-frames", "0", "-identify", "/local/martyn/Inspector Morse - 04x01 - The Infernal Serpent.mkv" ]

handleE :: (Show ε) => Either ε () -> IO ()
handleE = either print return

data Options = Options { _dryRunL  :: DryRunLevel  Two
                       , _verboseL :: VerboseLevel One }
$( makeLenses ''Options )

instance HasVerboseLevel One Options where
  verboseLevel = verboseL

instance HasDryRunLevel Two Options where
  dryRunLevel = dryRunL  

parseOpts :: Parser Options
parseOpts = Options <$> dryRun2P <*> verboseP

splitOne :: Char -> Text -> Maybe (Text,Text)
splitOne c t = second tail . flip splitAt t <$> findIndex (== c) t

filterIDs t = "ID_" `isPrefixOf` t && "=" `isInfixOf` t

data MPlayerInfo = MPlayerInfo { _width :: Natural }
  deriving Show

instance Parsecable MPlayerInfo where
  parser = let id_line
    MPlayerInfo <$> (read <$> (string "ID_VIDEO_WIDTH=" *> many digit ))

parsecMPI :: Text -> Text -> Either ParseError MPlayerInfo
parsecMPI = parsec

main :: IO ()
main = do
  opts <- optParser "show env, pwd" parseOpts
  join . fmap handleE . splitMError . doProcIO opts $ do
    (os,_) <- env
    let -- ids = catMaybes $ splitOne '=' <$> (filter filterIDs os)
        idtxt = unlines . sort $ filter filterIDs os -- (\(x,y) -> x <> "=" <> y)
--    mkIO "print ids" (liftIO $ forM_ (sort ids) (putStrLn . (\(x,y) -> (x <> ": " <> y))))
    lift . liftIO $ putStrLn idtxt
    lift . liftIO $ case parsecMPI "--*--INPUT--*--" idtxt of
      Right (MPlayerInfo w) -> putStrLn (pack $ show w)
      Left e -> putStrLn "error!" >> putStrLn (pack $ show e)
                

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
