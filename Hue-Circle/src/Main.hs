{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Data.Colour.RGBSpace.HSV
import Data.Colour.RGBSpace
import Data.Colour.SRGB
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine


main :: IO ()
main = mainWith (d # frame 0.2) >> putStrLn "Done!"


srgb2colour :: (Floating a, Ord a) => RGB a -> Colour a
srgb2colour = uncurryRGB (rgbUsingSpace sRGBSpace)

hsv' a b c = srgb2colour (hsv a b c)

d :: Diagram B
d = m
  where
    c a l = (text l # fontSize 12)
              <> circle 0.2 # fc white # lw none
              <> circle 0.3 # fc (hsv' a 1 1)
                            # lw none
 
    m   = position $ zip pts cs
    cs  = zipWith c angles (map (\a -> show $ floor $ 65535 * (a/360)) angles)


    -- letters = ['a'..'z']
    -- n       = length letters
    n = 12
    dangle  = 360 / fromIntegral n
    angles  = [fromIntegral k * dangle | k <- [0..n-1]]
    mkPt a  = sin (a / (180/pi)) ^& cos (a / (180/pi))
    pts     = map mkPt angles
