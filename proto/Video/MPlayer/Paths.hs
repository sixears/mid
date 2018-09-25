{-# LANGUAGE QuasiQuotes #-}

module Video.MPlayer.Paths where

import Path  ( Abs, File, Path, absfile )

mplayer :: Path Abs File
mplayer = [absfile|__mplayer__/bin/mplayer|]

