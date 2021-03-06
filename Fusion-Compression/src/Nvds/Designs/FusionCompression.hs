{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Nvds.Designs.FusionCompression where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

d :: Diagram B
d = boxes
  where
    boxes    = sq box
    allBoxes = sq boxes

    sq d = (
           (d # reflectX ||| d)
              ===
           (d ||| d # reflectX)
           )
           # centerXY

    box :: Diagram B
    box = (square 1 <> mconcat [ s # scale (1 - (0.1*s')) | s' <- [0..9] ]
            <> mconcat lines
          )
            # lc gold
            # bg blue
            # clipBy (square 1)
            # withEnvelope (square 1 :: Diagram B)
            # centerXY

    lines :: [Diagram B]
    lines = [ centerLine # translate (r2 (r, r)) 
            | r <- map (\s -> ((1 - 0.1 * s) - 0.5) / 3) [ 0..9 ] 
            ]

    centerLine = ((0.5) ^& (-0.5)) ~~ ((-0.5) ^& 0.5)

    s = square 1 # rotateBy (1/8)
                 # scaleX 0.9
                 # rotateBy (-1/8)
                 # lc gold

