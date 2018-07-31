{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine


main :: IO ()
main = mainWith (d # frame 0.2) >> putStrLn "Done!"


d :: Diagram B
d = allBoxes
  where
    boxes    = sq box
    allBoxes = sq boxes

    sq d = (
           (d # reflectX ||| d)
              ===
           (d ||| d # reflectX)
           )
           # centerXY

    box = (square 1 <> mconcat [ s # scale (1 - (0.1*s')) | s' <- [0..9] ]
            <> mconcat lines
          )
            # lc gold
            # bg blue
            # clipBy (square 1)
            # withEnvelope (square 1 :: Diagram B)
            # centerXY

    lines = [ centerLine # translate (r2 (r, r)) 
            | r <- map (\s -> ((1 - 0.1 * s) - 0.5) / 2.2) [ 0..9 ] 
            ]

    centerLine = ((0.5) ^& (-0.5)) ~~ ((-0.5) ^& 0.5)

    s = square 1 # rotateBy (1/8)
                 # scaleX 0.9
                 # rotateBy (-1/8)
                 # lc gold

