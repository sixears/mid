{-# LANGUAGE UnicodeSyntax #-}

module Fluffy.Functor2
  ( (⊳), (⊲) )
where

-- base --------------------------------

import Data.Functor  ( Functor, fmap )

--------------------------------------------------------------------------------

infixl 4 ⊳
(⊳) ∷ Functor ψ ⇒ (α → β) → ψ α → ψ β
(⊳) = fmap

infixl 4 ⊲
(⊲) ∷ Functor ψ ⇒ ψ α → (α → β) → ψ β
as ⊲ f = fmap f as

-- that's all, folks! ----------------------------------------------------------
