{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Video.MPlayer.T.Identify
  ( tests )
where

-- base --------------------------------

import Data.Either    ( Either( Right ) )
import Data.Function  ( ($) )
import Data.String    ( String )
import System.IO      ( IO )

-- fluffy ------------------------------

import Fluffy.Tasty  ( runTestsP_ )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- parsec ------------------------------

import Text.Parsec.Error  ( ParseError )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, defaultMain, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@?=), testCase )

-- text --------------------------------

import Data.Text  ( Text, unlines )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Video.MPlayer.Identify  ( Video( Video ), parsecV )

--------------------------------------------------------------------------------

-- | Some test data
testdata1 :: Text
testdata1 = unlines [ "ID_SID_0_LANG=eng"
                    , "ID_VIDEO_FORMAT=H264"
                    , "ID_VIDEO_HEIGHT=574"
                    , "ID_START_TIME=0.00"
                    , "ID_SUBTITLE_ID=0"
                    , "ID_VIDEO_ID=0"
                    , "ID_VIDEO_FPS=25.000"
                    , "ID_VIDEO_WIDTH=700"
                    ]

parseTests :: TestTree
parseTests =
  let parsecV' :: (MonadError ParseError η) => String -> Text -> η Video
      parsecV'  = parsecV
   in testGroup "parseTests"
        [ testCase "testdata1" $
            parsecV' "testdata1" testdata1 @?= Right (Video 700 574)
        ]

------------------------------------------------------------

_test :: IO ()
_test = defaultMain tests

_tests :: String -> IO ()
_tests p = runTestsP_ tests p

tests :: TestTree
tests = testGroup "Video.MPlayer.Identify" [ parseTests ]

-- that's all, folks! ----------------------------------------------------------
