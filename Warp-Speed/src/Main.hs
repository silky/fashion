{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Nvds.Designs.WarpSpeed

main :: IO ()
main = mainWith (frame 0.05 <$> d) >> putStrLn "Done!"

