{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Data.Colour.RGBSpace.HSV
import Data.Colour.RGBSpace
import Data.Colour.SRGB
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine


srgb2colour :: (Floating a, Ord a) => RGB a -> Colour a
srgb2colour = uncurryRGB (rgbUsingSpace sRGBSpace)


hsv' a b c = srgb2colour (hsv a b c)


main :: IO ()
main = mainWith (d # frame 0.2) >> putStrLn "Done!"


-- d = numberWheel
-- d = letterWheel
-- d = saturationWheel
d = brightnessWheel


brightnessWheel :: Diagram B
brightnessWheel = m
  where
    c a l b = (text l # fontSize 5)
                <> circle 0.2  # fc white # lw none
                <> circle 0.3  # fc (hsv' a 1 b)
                               # lw none
 
    bs = [ 0, 0.25..1 ]

    m        = mconcat $ map (\r -> position $ zip (map ((*.) (3+(-r*2))) pts) (cs (show (floor $ r * 65535)) r)) bs
    cs s sat = zipWith (\a l -> c a l sat) angles (repeat s)


    n      = 12
    dangle = 360 / fromIntegral n
    angles = [fromIntegral k * dangle | k <- [0..n-1]]

    pts    = map mkPt angles
    mkPt a = sin (a / (180/pi)) ^& cos (a / (180/pi))
    -- where
    --   m   = hcat $ map f bs
    --   f b = text (show $ floor (b * 65535)) # fontSize 12
    --                          # fc white
    --         <> square 1 # fc (hsv' 270 1 b)
    --                     # lc (hsv' 270 1 b)

    --   bs  = [ 0, 0.25..1 ]


saturationWheel :: Diagram B
saturationWheel = m
  where
    c a l s = (text l # fontSize 5)
                <> circle 0.2  # fc white # lw none
                <> circle 0.3  # fc (hsv' a s 1)
                               # lw none
 
    saturations = [ 0, 0.25..1 ]

    m        = mconcat $ map (\r -> position $ zip (map ((*.) (3+(-r*2))) pts) (cs (show (floor $ r * 65535)) r)) saturations
    cs s sat = zipWith (\a l -> c a l sat) angles (repeat s)


    n      = 12
    dangle = 360 / fromIntegral n
    angles = [fromIntegral k * dangle | k <- [0..n-1]]

    pts    = map mkPt angles
    mkPt a = sin (a / (180/pi)) ^& cos (a / (180/pi))


letterWheel :: Diagram B
letterWheel = m
  where
    c a l = (text l # fontSize 12)
              <> circle 0.05 # fc white # lw none
              <> circle 0.1  # fc (hsv' a 1 1)
                             # lw none
 
    m   = position $ zip pts cs
    cs  = zipWith c angles (map (: "") letters)

    letters = ['a'..'z']
    n       = length letters
    dangle  = 360 / fromIntegral n
    angles  = [fromIntegral k * dangle | k <- [0..n-1]]
    mkPt a  = sin (a / (180/pi)) ^& cos (a / (180/pi))
    pts     = map mkPt angles


numberWheel :: Diagram B
numberWheel = m
  where
    c a l = (text l # fontSize 12)
              <> circle 0.2 # fc white # lw none
              <> circle 0.3 # fc (hsv' a 1 1)
                            # lw none
 
    m   = position $ zip pts cs
    cs  = zipWith c angles (map (\a -> show $ floor $ 65535 * (a/360)) angles)

    n      = 12
    dangle = 360 / fromIntegral n
    angles = [fromIntegral k * dangle | k <- [0..n-1]]
    mkPt a = sin (a / (180/pi)) ^& cos (a / (180/pi))
    pts    = map mkPt angles
