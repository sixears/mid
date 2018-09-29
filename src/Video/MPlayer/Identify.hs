{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wall #-}

-- see https://stackoverflow.com/questions/52438196/how-to-parse-a-series-of-lines-with-only-a-few-interesting-ones-with-parsec-in
-- and https://www.cs.ox.ac.uk/jeremy.gibbons/wg21/meeting56/loeh-paper.pdf

module Video.MPlayer.Identify
  ( Video( Video ), height, parsecV, width )
where

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Video.MPlayer.Types.Video  ( Video( Video )
                                  , height, parsecV, width )

--------------------------------------------------------------------------------

-- that's all, folks! ----------------------------------------------------------
