{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Video.MPlayer.Identify.Error
  ( ExecCreatePathIOParseError(..)
  , _ECPIOPCreateE, _ECPIOPExecE, _ECPIOPPathE, _ECPIOPE
  , ecpipExecE', ecpipCreateE'
  )
where

-- base --------------------------------

import Data.Eq        ( Eq )
import Data.Function  ( (.), ($) )
import Text.Show      ( Show )

-- fluffy ------------------------------

import Fluffy.IO.Error      ( AsIOError( _IOError ), IOError )
import Fluffy.Parsec.Error  ( AsParseError( _ParseError ) )
import Fluffy.Path.Error    ( AsPathError( _PathError ), PathError )

-- lens --------------------------------

import Control.Lens.TH  ( makePrisms )

-- parsec ------------------------------

import Text.Parsec.Error  ( ParseError )

-- proclib -----------------------------

import ProcLib.Error.CreateProcError  ( AsCreateProcError( _CreateProcError )
                                      , CreateProcError( CreateProcError )
                                      )
import ProcLib.Error.ExecError        ( AsExecError( _ExecError )
                                      , ExecError
                                      , ToMaybeTexts
                                      , asExecError
                                      )
import ProcLib.Types.CmdSpec          ( CmdSpec )
import ProcLib.Types.ExitVal          ( ExitVal )

--------------------------------------------------------------------------------

data ExecCreatePathIOParseError = ECPIOPExecE ExecError
                                | ECPIOPCreateE CreateProcError
                                | ECPIOPPathE PathError
                                | ECPIOPE IOError 
                                | ECPIOPParseE ParseError
  deriving (Eq, Show)

$( makePrisms ''ExecCreatePathIOParseError )

instance AsExecError ExecCreatePathIOParseError where
  _ExecError = _ECPIOPExecE

instance AsCreateProcError ExecCreatePathIOParseError where
  _CreateProcError = _ECPIOPCreateE

instance AsPathError ExecCreatePathIOParseError where
  _PathError = _ECPIOPPathE

instance AsIOError ExecCreatePathIOParseError where
  _IOError = _ECPIOPE

instance AsParseError ExecCreatePathIOParseError where
  _ParseError = _ECPIOPParseE

ecpipExecE' :: ToMaybeTexts ω => CmdSpec -> ExitVal -> ω -> ExecCreatePathIOParseError
ecpipExecE' cmdspc xtvl x = ECPIOPExecE $ asExecError cmdspc xtvl x

ecpipCreateE' :: IOError -> ExecCreatePathIOParseError
ecpipCreateE' =  ECPIOPCreateE . CreateProcError

-- that's all, folks! ----------------------------------------------------------
