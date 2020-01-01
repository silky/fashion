{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Nvds.Colours.ColourSets


main :: IO ()
main = mainWith (d # frame 0.2) >> putStrLn "Done!"

d1 = t1 <> t2
  where
    t1 = e bn31 pisos # snugR 
    t2 = e summertime barbapapa # translateY (sqrt 2 / 2) # snugL

d = d1 # snugB <> d1 # reflectX # reflectY # snugT 

e c1 c2 = w1 <> w2
  where
    w1 = p1 c1 (reverse c1) # centerXY # reflectX
    w2 = p1 c2 (reverse c2) # reflectY # rotateBy (1/4) # centerXY 


p1 :: Colours -> Colours -> Diagram B
p1 c1 c2 = s1 ||| s2
  where
    n = 2
    s1 = (hcat (side n c1)) # rotateBy (1/8) # reflectY # centerXY
    s2 = (hcat (side n c2)) # rotateBy (1/8) # reflectX # centerXY


side n colours
 -- = zipWith (\r c -> rect 1 (r**(1/2)) # alignB # lw none # fc c) [1..n] (cycle colours)
 = zipWith (\r c -> rect 1 r # alignB # lw none # fc c) [1..n] (cycle colours)

