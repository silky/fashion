{-# LANGUAGE FlexibleContexts   #-}

-- Run like:
--
--  > stack run -- -- --loop -w 200 -s src/Main.hs -o a.svg
--
module Main where

import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude
import Nvds.Designs.Cylinders (basic)

main :: IO ()
main = mainWith basic

