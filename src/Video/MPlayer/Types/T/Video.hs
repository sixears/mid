{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Video.MPlayer.Types.T.Video
  ( tests )
where

import Prelude  ( Int )

-- base --------------------------------

import Data.Either    ( Either( Right ) )
import Data.Function  ( (.), ($) )
import Data.Functor   ( fmap )
import Data.Maybe     ( Maybe( Just ) )
import Data.Ratio     ( Rational, (%) )
import Data.String    ( String )
import System.IO      ( IO )

-- fluffy ------------------------------

import Fluffy.Duration            ( fromS )
import Fluffy.Parsec.Permutation  ( parsec_' )
import Fluffy.Tasty               ( assertLeft, runTestsP_ )

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

import Video.MPlayer.Identify  ( Video, video )

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
                   , "ID_LENGTH=2653.52"
                   , "ID_FILENAME=/local/martyn/The Infernal Serpent.mkv"
                   ]

dupFormat :: Text
dupFormat = unlines [ "ID_SID_0_LANG=eng"
                    , "ID_VIDEO_FORMAT=H264"
                    , "ID_VIDEO_FPS=25.000"
                    , "ID_VIDEO_HEIGHT=574"
                    , "ID_START_TIME=0.00"
                    , "ID_SUBTITLE_ID=0"
                    , "ID_VIDEO_ID=0"
                    , "ID_LENGTH=6282.00"
                    , "ID_VIDEO_FORMAT=H264"
                    , "ID_VIDEO_WIDTH=700"
                    , "ID_FILENAME=/local/martyn/The Infernal Serpent.mkv"
                    ]

missingHeight :: Text
missingHeight = unlines [ "ID_SID_0_LANG=eng"
                        , "ID_VIDEO_FORMAT=H264"
                        , "ID_START_TIME=0.00"
                        , "ID_SUBTITLE_ID=0"
                        , "ID_LENGTH=6282.00"
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
                    , "ID_LENGTH=2653.52"
                    , "ID_FILENAME=/local/martyn/The Infernal Serpent.mkv"
                    ]

parseTests :: TestTree
parseTests =
  let parsecV' :: (MonadError ParseError η) => String -> Text -> η Video
      parsecV'  = parsec_'
      eMsgStrs = fmap messageString . errorMessages
      testFail msg = assertLeft ( \ e -> Just msg @?= lastMay (eMsgStrs e))
      fn = "/local/martyn/The Infernal Serpent.mkv"
      ratty :: Rational
      ratty = 66338 % 25
   in testGroup "parseTests"
        [ testCase "success" $
                parsecV' "dataSucc" dataSucc
            @?= Right (video 700 574 (fromS $ ratty) 25 fn)
        , testCase "missing height" $
            testFail "eof before ID_FILENAME,ID_VIDEO_HEIGHT"
                     (parsecV' "missingHeight" missingHeight)
        , testCase "duplicate FPS" $
                parsecV' "dupFormat" dupFormat
            @?= Right (video 700 574 (fromS (6282 :: Int)) 25 fn)
        , testCase "duplicate height" $
            testFail "duplicate ID_VIDEO_HEIGHT"
                     (parsecV' "dupHeight" dupHeight)
        ]

------------------------------------------------------------

_test :: IO ()
_test = defaultMain tests

_tests :: String -> IO ()
_tests = runTestsP_ tests

tests :: TestTree
tests = testGroup "Video.MPlayer.Types.Video" [ parseTests ]

-- that's all, folks! ----------------------------------------------------------
