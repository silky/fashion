{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Tilings

main :: IO ()
main = mainWith d


t :: Tiling
t = mk3Tiling [6,12,4] 
-- t = semiregular [4,3,4,5] [3,2,1,0]

d :: Diagram B
d = drawTilingStyled s sf t 20 20
                                # centerXY 
                                # pad 1.1
  where
    s = mempty # lw 0

    col :: TilingPoly -> Colour Double
    col Triangle  = green
    col Square    = white
    col Hexagon   = white
    col Octagon   = white
    col Dodecagon = white

    sf p = mempty 
                # lw 2
                # lc black
                # fc ( col
                        . polyFromSides
                        . length
                        . polygonVertices
                        $ p )
