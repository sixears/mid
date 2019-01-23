{-# LANGUAGE UnicodeSyntax #-}

module Fluffy.IO.Error2
  ( userErr )
where

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( toString )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- text --------------------------------

import Data.Text  ( Text )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Fluffy.IO.Error  ( AsIOError, userE )

--------------------------------------------------------------------------------

userErr ∷ (AsIOError ε, MonadError ε η) ⇒ Text → η ω
userErr = throwError ∘ userE ∘ toString

-- that's all, folks! ----------------------------------------------------------
