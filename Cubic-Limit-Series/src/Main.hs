{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Nvds.Designs.CubicLimitSeries

main :: IO ()
main = mainWith design
