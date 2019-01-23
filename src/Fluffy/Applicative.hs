{-# LANGUAGE UnicodeSyntax #-}

module Fluffy.Applicative
  ( (⊵), (⊴) )
where

-- base --------------------------------

import Control.Applicative  ( Applicative, (<*>), (<**>) )

--------------------------------------------------------------------------------

infixl 1 ⊵
(⊵) ∷ Applicative ψ ⇒ ψ (α → β) → ψ α → ψ β
(⊵) = (<*>)

infixr 1 ⊴
(⊴) ∷ Applicative ψ ⇒ ψ α → ψ (α → β) → ψ β
(⊴) = (<**>)

-- that's all, folks! ----------------------------------------------------------
