{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Video.MPlayer.Types.T.Video
  ( tests )
where

-- base --------------------------------

import Data.Either    ( Either( Right ) )
import Data.Function  ( (.), ($) )
import Data.Functor   ( fmap )
import Data.Maybe     ( Maybe( Just ) )
import Data.String    ( String )
import System.IO      ( IO )

-- fluffy ------------------------------

import Fluffy.Tasty  ( assertLeft, runTestsP_ )

-- mono-traversable --------------------

import Data.MonoTraversable  ( lastMay )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- parsec ------------------------------

import Text.Parsec.Error  ( ParseError, errorMessages, messageString )

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

dataSucc :: Text
dataSucc = unlines [ "ID_SID_0_LANG=eng"
                   , "ID_VIDEO_FORMAT=H264"
                   , "ID_VIDEO_HEIGHT=574"
                   , "ID_START_TIME=0.00"
                   , "ID_SUBTITLE_ID=0"
                   , "ID_VIDEO_ID=0"
                   , "ID_VIDEO_FPS=25.000"
                   , "ID_VIDEO_WIDTH=700"
                   ]

dupFPS :: Text
dupFPS = unlines [ "ID_SID_0_LANG=eng"
                 , "ID_VIDEO_FORMAT=H264"
                 , "ID_VIDEO_FPS=25.000"
                 , "ID_VIDEO_HEIGHT=574"
                 , "ID_START_TIME=0.00"
                 , "ID_SUBTITLE_ID=0"
                 , "ID_VIDEO_ID=0"
                 , "ID_VIDEO_FPS=25.000"
                 , "ID_VIDEO_WIDTH=700"
                 ]

missingHeight :: Text
missingHeight = unlines [ "ID_SID_0_LANG=eng"
                        , "ID_VIDEO_FORMAT=H264"
                        , "ID_START_TIME=0.00"
                        , "ID_SUBTITLE_ID=0"
                        , "ID_VIDEO_ID=0"
                        , "ID_VIDEO_FPS=25.000"
                        , "ID_VIDEO_WIDTH=700"
                        ]

dupHeight :: Text
dupHeight = unlines [ "ID_SID_0_LANG=eng"
                    , "ID_VIDEO_FORMAT=H264"
                    , "ID_VIDEO_HEIGHT=574"
                    , "ID_START_TIME=0.00"
                    , "ID_SUBTITLE_ID=0"
                    , "ID_VIDEO_ID=0"
                    , "ID_VIDEO_FPS=25.000"
                    , "ID_VIDEO_WIDTH=700"
                    , "ID_VIDEO_HEIGHT=574"
                    ]

parseTests :: TestTree
parseTests =
  let parsecV' :: (MonadError ParseError η) => String -> Text -> η Video
      parsecV'  = parsecV
      eMsgStrs = fmap messageString . errorMessages
      testFail msg = assertLeft ( \ e -> Just msg @?= lastMay (eMsgStrs e))
   in testGroup "parseTests"
        [ testCase "success" $
            parsecV' "dataSucc" dataSucc @?= Right (Video 700 574)
        , testCase "missing height" $
            testFail "eof before ID_VIDEO_HEIGHT"
                     (parsecV' "missingHeight" missingHeight)
        , testCase "duplicate FPS" $
            parsecV' "dupFPS" dupFPS @?= Right (Video 700 574)
        , testCase "duplicate height" $
            testFail "duplicate ID_VIDEO_HEIGHT"
                     (parsecV' "dupHeight" dupHeight)
        ]

------------------------------------------------------------

_test :: IO ()
_test = defaultMain tests

_tests :: String -> IO ()
_tests p = runTestsP_ tests p

tests :: TestTree
tests = testGroup "Video.MPlayer.Types.Video" [ parseTests ]

-- that's all, folks! ----------------------------------------------------------
