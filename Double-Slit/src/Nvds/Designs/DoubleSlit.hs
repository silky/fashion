{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Nvds.Designs.DoubleSlit where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

d :: Diagram B
d = vsep (-0.2) [ grouped
                , grouped
                ]
  where
    grouped = mconcat circles # scale 0.2
    n       = 10
    sizes   = [k/n | k <- [1..n]] :: [Double]
    circles = map mkC sizes 

    mkC :: Double -> Diagram B
    mkC s = circle s # (lw . local $ (1-s)/30)

