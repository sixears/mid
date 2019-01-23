{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}

-- see https://stackoverflow.com/questions/52438196/how-to-parse-a-series-of-lines-with-only-a-few-interesting-ones-with-parsec-in
-- and https://www.cs.ox.ac.uk/jeremy.gibbons/wg21/meeting56/loeh-paper.pdf

module Video.MPlayer.Identify
  ( midentify )
where

-- base --------------------------------

import Data.Tuple    ( fst )

-- fluffy ------------------------------

import Fluffy.Functor2  ( (⊳) )
import Fluffy.Path      ( AbsFile, AsFilePath( toFPath ) )

-- proclib -----------------------------

import ProcLib.Error.CreateProcError  ( AsCreateProcError )
import ProcLib.Error.ExecError        ( AsExecError )
import ProcLib.Types.CmdSpec          ( CmdSpec( CmdSpec ) )
import ProcLib.Process                ( mkProc_ )
import ProcLib.Types.ProcIO           ( ProcIO )

-- text --------------------------------

import Data.Text  ( Text, pack )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  Video.MPlayer.Paths  as  Paths

--------------------------------------------------------------------------------

midentify ∷ (AsCreateProcError ε, AsExecError ε) ⇒ AbsFile → ProcIO ε η [Text]
midentify fn = let args = [ "-vo", "null", "-ao", "null", "-frames", "0"
                          , "-identify", pack (toFPath fn) ]
                   fstT ∷ ([Text],[Text]) → [Text]
                   fstT = fst
                in fstT ⊳ mkProc_ (CmdSpec Paths.mplayer args)

-- that's all, folks! ----------------------------------------------------------
