{-# LANGUAGE OverloadedStrings #-}

import Prelude ( )

-- base --------------------------------

import System.IO  ( IO )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, defaultMain, testGroup )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified Video.MPlayer.Types.T.Video  as  Video

-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "mid" [ Video.tests ]

-- that's all, folks! ----------------------------------------------------------
