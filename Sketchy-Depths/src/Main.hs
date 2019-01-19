{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Nvds.Designs.SketchyDepths

main :: IO ()
main = mainWith (d "flower") >> putStrLn "Done!"

