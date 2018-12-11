{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Main where

import Control.Monad
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Nvds.Designs.SunMountainScape

main :: IO ()
main = mainWith (frame 0.2 <$> d) >> putStrLn "Done!"


