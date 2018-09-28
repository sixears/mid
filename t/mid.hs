{-# LANGUAGE OverloadedStrings #-}

import Prelude ( )

-- base --------------------------------

import System.IO  ( IO )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, defaultMain, testGroup )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified Video.MPlayer.T.Identify  as  Identify

-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "mid" [ Identify.tests ]

-- that's all, folks! ----------------------------------------------------------
