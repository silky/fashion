{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Data.Colour
import Data.Colour.RGBSpace
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine


main :: IO ()
main = mainWith (d # frame 0.2) >> putStrLn "Done!"

limit = pi
-- limit = 3*pi

locs = [ (x, y) 
          | x <- [0.1, 0.3 .. limit]
          , y <- [0.1, 0.3 .. limit]
       ]

vectorField (x, y) = r2 (cos (y), cos (x))

points = map p2 locs
arrows = map arrowAtPoint locs

arrowAtPoint (x, y) = obj
  where
    -- obj = arrowAt' opts (p2 (x,y)) (sL *^ vf) # alignTL # lc c
    obj = circle (m*0.1) # moveTo (p2 (x,y)) # fc c # lw none

    c :: Colour Double
    c = rgbUsingSpace sRGBSpace (x / limit) 0 (m**m)

    vf = vectorField (x, y)

    m  = norm vf
    hs = 0.02 * m
    sW = 0.005 * m
    sL = 0.05 + 0.1 * m
    opts = with & arrowHead  .~ spike
                & headLength .~ normalized hs
                & shaftStyle %~ lwN sW

field = position (zip points arrows)


d :: Diagram B
d = field # translateY 0.05
    <> square 3.5 # lw none # alignBL

