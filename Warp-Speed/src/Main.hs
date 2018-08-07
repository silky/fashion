{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Control.Monad
import Data.Colour
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import GSL.Random.Quasi (halton, QRNGType, getListSample, newQRNG)

main :: IO ()
main = mainWith (frame 0.05 <$> d) >> putStrLn "Done!"


getPoints :: QRNGType -> Int -> IO [Point V2 Double]
getPoints qrngType n = do
    rng     <- newQRNG qrngType 2
    points  <- replicateM n (getListSample rng)

    return $ map (\[a,b] -> mkP2 a b) points


d :: IO (Diagram B)
d = do
  let n = 1000

  points <- getPoints halton n
  
  let c       = circle s # fc white # lw none
      circles = position (map f points)
      d'      = (circles # centerXY <> rect w h # bg black # centerXY)
                  # clipBy (rect w h)
                  # withEnvelope (rect w h :: Diagram B)
      --
      -- Black-hole-esque thing
      -- f pt    = (pt, c # scale (norm (pt - pc)))
      --
      -- Accidentally-close:
      f pt    = ( pt
                ,  pc ~~ pt # scale (norm (pt - pc) ** 0.6) 
                            # lc (col (norm (pc - pt) ** 0.2)) # lw 0.9
                )

  return d'

  where
    col :: Double -> Colour Double
    col n = blend n red blue

    s = 0.01
    w = 1
    h = 1
    cx = w / 2
    cy = h / 2
    pc = cx ^& cy

