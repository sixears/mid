{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-# LANGUAGE ViewPatterns          #-}

{- TODO --------------------------------

- mv Fluffy additions to fluffy
- Use Printable Exceptions rather than show
- except with -E, print error immediately rather than some results
- add -s to read names from stdin
- tidy and refactor

-} -------------------------------------


import Prelude  ( Double, Float, (+), (/), fromIntegral )

-- base --------------------------------

import Control.Applicative     ( many )
import Control.Monad           ( forM, return, sequence )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bifunctor          ( first )
import Data.Bool               ( Bool, (&&) )
import Data.Either             ( Either, either )
import Data.Eq                 ( Eq, (==) )
import Data.Foldable           ( foldl1 )
import Data.Function           ( ($), id )
import Data.Functor            ( fmap )
import Data.List               ( filter, sort )
import Data.List.NonEmpty      ( nonEmpty, unzip )
import Data.Maybe              ( Maybe( Just, Nothing ), catMaybes )
import Data.Monoid             ( (<>) )
import Data.Tuple              ( snd, uncurry )
import System.Exit             ( ExitCode( ExitFailure ) )
import System.IO               ( IO )
import System.IO.Error         ( doesNotExistErrorType, mkIOError )
import Text.Show               ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- fluffy ------------------------------

import Fluffy.Applicative         ( (⊵) )
import Fluffy.Duration            ( Duration, hours )
import Fluffy.ByteSize2           ( ByteSize, gibibytes )
import Fluffy.Functor2            ( (⊳) )
import Fluffy.IO.Error            ( AsIOError( _IOErr ) )
import Fluffy.Lens2               ( (⊣), (⋕) )
import Fluffy.Monad               ( (≫), (⩾) )
import Fluffy.MonadError          ( fromMaybe, splitMError )
import Fluffy.MonadIO             ( die, eitherIOThrow, say, warn )
import Fluffy.MonadIO.File        ( stat )
import Fluffy.Nat                 ( One, Two )
import Fluffy.Options             ( optParser )
import Fluffy.Parsec.Error        ( AsParseError )
import Fluffy.Parsec.Permutation  ( parsecP )
import Fluffy.Path                ( AbsDir, AbsFile, AsFilePath( toFPath ), Dir
                                  , File, MyPath( resolve ), RelFile
                                  , getCwd_, parseFile'
                                  )
import Fluffy.ToRational          ( fromRational )

-- lens --------------------------------

import Control.Lens.Getter  ( view )
import Control.Lens.TH      ( makeLenses )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )
import Control.Monad.Trans   ( lift )

-- optparse-applicative ----------------

import Options.Applicative  ( Parser, ReadM, argument, eitherReader, flag, help
                            , long, metavar, short )

-- path --------------------------------

import Path  ( Path )

-- proclib -----------------------------

import ProcLib.CommonOpt.DryRun       ( DryRunLevel
                                      , HasDryRunLevel( dryRunLevel )
                                      , dryRun2P
                                      )
import ProcLib.CommonOpt.Verbose      ( HasVerboseLevel( verboseLevel )
                                      , VerboseLevel, verboseP )
import ProcLib.Error.CreateProcError  ( AsCreateProcError )
import ProcLib.Error.CreateProcIOError  ( ExecCreatePathIOParseError )
import ProcLib.Error.ExecError        ( AsExecError )
import ProcLib.Process                ( doProcIO )
import ProcLib.Types.ProcIO           ( ProcIO )

-- text --------------------------------

import Data.Text     ( Text
                     , isInfixOf, isPrefixOf, lines, pack, unlines, unpack )
import Data.Text.IO  ( getContents )

-- tfmt --------------------------------

import Text.Fmt  ( fmt, fmtT )

-- unix --------------------------------

import System.Posix.Files  ( FileStatus, fileSize )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Video.MPlayer.Identify        ( midentify )
import Video.MPlayer.Types.Video     ( Video, duration, fps, height, width )

--------------------------------------------------------------------------------

-- whether to show all the values output by mplayer -identify rather than the
-- summary parsing
data ShowAll = ShowAll | NoShowAll
  deriving Eq

-- | whether to stop on the first bad file seen, or continue and summarize the
--   good data
data IgnoreBadFiles = IgnoreBadFiles | NoIgnoreBadFiles
  deriving Eq

-- | whether to read file names from stdin (one per line)
data FilesOnStdin = FilesOnStdin | NoFilesOnStdin
  deriving Eq

type AbsRelFile = Either AbsFile RelFile

data Options = Options { _fns            ∷ [AbsRelFile]
                       , _dryRunL        ∷ DryRunLevel  Two
                       , _verboseL       ∷ VerboseLevel One
                       , _showAll        ∷ ShowAll
                       , _ignoreBadFiles ∷ IgnoreBadFiles
                       , _filesOnStdin   ∷ FilesOnStdin
                       }
$( makeLenses ''Options )

instance HasVerboseLevel One Options where
  verboseLevel = verboseL

instance HasDryRunLevel Two Options where
  dryRunLevel = dryRunL

parseOpts ∷ Parser Options
parseOpts = let argMeta = metavar "FILE" <> help "file to query"
                filesOnStdinHelp = "read files from stdin one per line"
             in Options ⊳ many (argument fileReader argMeta)
                        ⊵ dryRun2P
                        ⊵ verboseP
                        ⊵ flag NoShowAll ShowAll
                                 (short 'a' <> long "all"
                                            <> help "show all the info")
                        ⊵ flag NoIgnoreBadFiles IgnoreBadFiles
                                 (short 'E' <> long "ignore-error-files"
                                            <> help "continue past bad files")

                        ⊵ flag NoFilesOnStdin FilesOnStdin
                                 (short 's' <> long "files-on-stdin"
                                            <> help filesOnStdinHelp)

filterIDs ∷ Text → Bool
filterIDs t = "ID_" `isPrefixOf` t && "=" `isInfixOf` t

{- | error for missing thing -}
noSuchErr ∷ AsIOError ε ⇒ Text → Path β τ → ε
noSuchErr t f =
  let fpath = toFPath f
   in _IOErr ⋕ mkIOError doesNotExistErrorType (unpack t) Nothing (Just fpath)

{- | throw error for missing thing -}
noSuchE ∷ (AsIOError ε, MonadError ε η) ⇒ Text → Path β τ → η ω
noSuchE t f = throwError $ noSuchErr t f

{- | throw error for missing file or directory -}
noSuchDFE ∷ (AsIOError ε, MonadError ε η) ⇒ Path β τ → η ω
noSuchDFE = noSuchE "file or directory"

{- | throw error for missing directory -}
noSuchDirE ∷ (AsIOError ε, MonadError ε η) ⇒ Path β Dir → η ω
noSuchDirE = noSuchE "directory"

{- | throw error for missing file -}
noSuchFileE ∷ (AsIOError ε, MonadError ε η) ⇒ Path β File → η ω
noSuchFileE = noSuchE "file"

{- | error for missing file -}
noSuchFileErr ∷ AsIOError ε ⇒ Path β File → ε
noSuchFileErr = noSuchErr "file"

{- | error for missing directory -}
noSuchDirErr ∷ AsIOError ε ⇒ Path β Dir → ε
noSuchDirErr = noSuchErr "directory"

{- | error for missing file or directory -}
noSuchFDErr ∷ AsIOError ε ⇒ Path β τ → ε
noSuchFDErr = noSuchErr "file or directory"

{- | throw error for missing file as indicated by `Nothing` -}
maybeNoSuchFileE ∷ (AsIOError ε, MonadError ε η) ⇒
                   Path β File → η (Maybe α) → η α
maybeNoSuchFileE fn g = g ⩾ fromMaybe (noSuchFileErr fn)

{- | Call a fn that returns a `Maybe` with `Nothing` for a missing file; throw a
     no such file IOError into IOError -}
maybeNoSuchFileE' ∷ (AsIOError ε, MonadError ε η) ⇒
                   Path β File → (Path β File → η (Maybe α)) → η α
maybeNoSuchFileE' fn g = g fn ⩾ fromMaybe (noSuchFileErr fn)

statF ∷ (MonadIO μ, AsIOError ε, MonadError ε μ) ⇒
        (FileStatus → α) → Path β τ -> μ (Maybe α)
statF g fn = fmap g ⊳ stat fn

statF' ∷ (MonadIO μ, AsIOError ε, MonadError ε μ) ⇒
        (FileStatus → α) -> Path β File → μ α
statF' g fn = statF g fn ⩾ fromMaybe (noSuchFileErr fn)

{- | The size of file; Nothing if file doesn't exist -}
fsize ∷ (MonadError ε μ, AsIOError ε, MonadIO μ) ⇒
        Path β File → μ (Maybe ByteSize)
fsize = statF (fromIntegral ∘ fileSize)

fileReader ∷ ReadM AbsRelFile
fileReader = eitherReader (first show ∘ parseFile' ∘ pack)

fmtDuration ∷ Video → Text → ByteSize → Text
fmtDuration v fn sz =
  let gib = fromRational (gibibytes sz)
      gibPerHour ∷ Double
      gibPerHour = ((gib / fromRational(v ⊣ duration ⊣ hours)))
   in [fmt|%9T  %4dx%4d  %3.3ffps  %3.2fGiB  (%3.2fGiB/h)  %t|]
          (v ⊣ duration) (v ⊣ width) (v ⊣ height) (v ⊣ fps) gib gibPerHour fn

parsecMPI ∷ (MonadIO μ, AsParseError ε, MonadError ε μ) ⇒
            Text → ByteSize → Text → μ (ByteSize, Duration)
parsecMPI fn sz idtxt =
  parsecP fn idtxt ⩾ \ v → say (fmtDuration v fn sz) ≫ return (sz, v ⊣ duration)

----------------------------------------

{- | Given a file (rel or abs), and a base dir, get an absolute file and a Text
     representation of the input suitable for error messages
 -}
resolveFile ∷ AbsDir → AbsRelFile → (AbsFile, Text)
resolveFile cwd f = (either id (resolve cwd) f, pack $ either toFPath toFPath f)

----------------------------------------

{- | Given a file, try to read its vital statistics with midentify
 -}
doFile ∷ (MonadIO μ,
          AsCreateProcError ε, AsExecError ε, AsParseError ε, AsIOError ε) ⇒
         Options → AbsFile → Text → ProcIO ε μ (Maybe (ByteSize, Duration))
doFile opts af fn = do
  out ← midentify af
  sz ← lift $ maybeNoSuchFileE' af fsize
  let idtxt = unlines ∘ sort $ filter filterIDs out
  if opts ⊣ showAll == ShowAll
  then lift $ say idtxt ≫ return Nothing
  else lift $ Just ⊳ parsecMPI fn sz idtxt

doFiles' ∷ MonadIO μ ⇒
           Options → AbsDir → [AbsRelFile]
         → μ [(AbsRelFile,
             Either ExecCreatePathIOParseError (Maybe (ByteSize, Duration)))]
doFiles' opts cwd filenames =
  let f fn = fmap (fn,) ∘ splitMError ∘ doProcIO opts $ uncurry (doFile opts) (resolveFile cwd fn)
   in sequence $ f ⊳ filenames

doFiles ∷ MonadIO μ ⇒
          Options → AbsDir → [AbsRelFile] → μ [Maybe (ByteSize, Duration)]

doFiles opts cwd filenames = do
  z ← doFiles' opts cwd filenames
  if opts ⊣ ignoreBadFiles == IgnoreBadFiles
  then forM z ( \ (fn ,ei) → either ( \ _ → warn ("ERROR file: '" <> pack (either toFPath toFPath fn) <> "'") ≫ return Nothing) return ei)
  else either (die (ExitFailure 255) ∘ pack ∘ show) return (sequence $ snd ⊳ z)

-- | if the options say so, read stdin as one file-per-line, and attempt to
--   interpret each as a file; on error, throws to IO
readStdinFiles ∷ MonadIO μ ⇒ Options → μ [AbsRelFile]
readStdinFiles (view filesOnStdin → FilesOnStdin) = 
  liftIO $ getContents ⩾ sequence ∘ fmap (eitherIOThrow ∘ parseFile') ∘ lines
readStdinFiles _ = return []


main ∷ IO ()
main = do
  opts ← optParser "summarize video characteristics" parseOpts
  cwd ∷ AbsDir ← getCwd_

  stdinFiles ← readStdinFiles opts

  let filenames = opts ⊣ fns <> stdinFiles

  z'' ← doFiles opts cwd filenames

  z' ← case nonEmpty (catMaybes z'') of
          Nothing → return Nothing
          Just xs → let (sizes, durations) = unzip xs
                      in return $ Just (foldl1 (+) sizes, foldl1 (+) durations)

  case z' of
    Nothing                         → return ()
    Just (sizeTotal, durationTotal) →
      let gbperh ∷ Float
          gbperh = fromRational (gibibytes sizeTotal) /
                   fromRational (durationTotal ⊣ hours)
       in say $ [fmtT|Total: %T  %T  (%3.2fGiB/h)|] sizeTotal durationTotal gbperh

  return ()

-- that's all, folks! ----------------------------
