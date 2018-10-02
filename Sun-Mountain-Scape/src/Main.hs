{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Main where

import Control.Monad
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
-- import System.Random
import Diagrams.Path
import Data.IORef
import Data.Random.Source.StdGen
import qualified Data.Random as R
import qualified Data.Random.Distribution.Gamma  as D
import qualified Data.Random.Distribution.Normal as D

main :: IO ()
main = mainWith (frame 0.2 <$> d) >> putStrLn "Done!"


-- Plan:
--
-- 1. Rectangles
-- 2. Sun
-- 3. Pick random values
--    - Smooth them out
--    - Make a path
--    - Convert to mountains


boundary :: Diagram B
boundary = foldl g mempty heights
    where
      w       = 20
      g d h   = d === rect w h
      heights = [ 1, 1, 1, 1, 0.7, 0.5, 0.3, 0.25, 0.2 ]


sun :: Diagram B
sun = circle 3 # fc orange # lw 1


d :: IO (Diagram B)
d = do
  let count     = 50
      smoothing = 20

  -- Heights of the mountain
  -- let ys :: [Double]
  --     l  = 0.0
  --     u  = 5.0
  --     ys = interpolate 10 . take count $ randomRs (l, u) (mkStdGen 1)
  
  -- ys <- interpolate smoothing <$> (replicateM count $ R.sample (D.gamma 5 0.4))

  -- TODO: Implement Gala's idea
  let dist = D.gamma 5 0.4
  r <- newIORef (mkStdGen 1)
  ys :: [Double] <- flip R.runRVar r $ interpolate smoothing <$> replicateM count dist

  let ys' = 0 : (ys ++ [0])

  -- x-coordinates
  let xs :: [Double]
      xs = [0, 1 / fromIntegral (length ys + 1) .. 1]
      mountains = trailFromVertices $ zipWith (^&) xs ys'

  let m = mountains # closeTrail # stroke # scaleX 20 # fc black # lc white

  let top = sun # moveTo ((-6) ^& (-4)) <> boundary

  let f = vsep 0.1 [ m # centerXY <> top # moveTo (0 ^& 7)
                   , reflectY boundary
                   ]

  return $ f


interpolate :: Int -> [Double] -> [Double]
interpolate steps xs = foldl g [] (zip xs (drop 1 xs))
  where
    g xs (n, m) = xs ++ interp n m
    interp n m  = [ n - (fromIntegral k*(n - m)) / fromIntegral steps
                  | k <- [0..steps] ]


