{-# LANGUAGE UnicodeSyntax #-}

module Fluffy.Lens2
  ( (⊣), (⋕) )
where

-- lens --------------------------------

import Control.Lens.Getter  ( Getting, (^.) )
import Control.Lens.Review  ( AReview, re )

--------------------------------------------------------------------------------

infixl 8 ⊣
(⊣) ∷ δ → Getting α δ α → α
(⊣) = (^.)

(⋕) :: AReview t s -> s -> t
x ⋕ y = y ^. re x

-- that's all, folks! ----------------------------------------------------------
