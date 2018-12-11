{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude
import Nvds.Designs.MagicCarpet

main :: IO ()
main = mainWith (frame 0.2 <$> d) >> putStrLn "Done!"
