module Video.MPlayer.T.Identify
  ( tests )
where

-- base --------------------------------

import Data.Either    ( Either( Right ) )
import Data.Function  ( ($) )
import Data.String    ( String, unlines )
import System.IO      ( IO )

-- fluffy ------------------------------

import Fluffy.Tasty  ( runTestsP_ )

-- parsec ------------------------------

import Text.Parsec.Prim  ( parse )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, defaultMain, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( Assertion, (@?=)
                         , assertBool, assertEqual, assertFailure, testCase )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Video.MPlayer.Identify  ( Video( Video ), video )

--------------------------------------------------------------------------------

-- | Some test data
testdata1 :: String
testdata1 = unlines [ "ID_SID_0_LANG=eng"
                    , "ID_VIDEO_FORMAT=H264"
                    , "ID_VIDEO_HEIGHT=574"
                    , "ID_START_TIME=0.00"
                    , "ID_SUBTITLE_ID=0"
                    , "ID_VIDEO_ID=0"
                    , "ID_VIDEO_FPS=25.000"
                    , "ID_VIDEO_WIDTH=700"
                    ]

parseTests =
  testGroup "parseTests"
    [ testCase "testdata1" $
        parse video "testdata1" testdata1 @?= Right (Video 700 574)
    ]

------------------------------------------------------------

_test :: IO ()
_test = defaultMain tests

_tests :: String -> IO ()
_tests p = runTestsP_ tests p

tests :: TestTree
tests = testGroup "Video.MPlayer.Identify" [ parseTests ]

-- that's all, folks! ----------------------------------------------------------
