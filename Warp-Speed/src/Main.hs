{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Control.Monad
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import GSL.Random.Quasi (halton, QRNGType, getListSample, newQRNG)

main :: IO ()
main = mainWith (frame 0.2 <$> d) >> putStrLn "Done!"


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
      d'      = circles # centerXY <> rect w h # bg black # centerXY
      -- Black-hole-esque thing
      -- f pt    = (pt, c # scale (norm (pt - pc)))
      --
      -- Accidentally-close:
      f pt    = (pt, pc ~~ pt # scale 0.2 # lc gold  # lw 0.4 # centerXY)

  return d'

  where
    s = 0.01
    w = 1.2
    h = 1.2
    cx = w / 2
    cy = h / 2
    pc = cx ^& cy

