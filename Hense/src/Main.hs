{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Main where

import qualified Debug.Trace as Debug
import Control.Monad
import Data.IORef
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Nvds.Svg
import Data.Random.Source.StdGen
import qualified Data.Random as R
import qualified Data.Random.Distribution.Normal  as D
import qualified Data.Random.Distribution.Uniform as D


main :: IO ()
main = mainWith (frame 0.2 <$> d) >> putStrLn "Done!"


eps :: Double
eps = 0.001


d :: IO (Diagram B)
d = do
  -- Just (w, h, p) <- singlePathFromFile' "../refs/g.svg"
  Just (w, h, p1) <- singlePathFromFile' "../refs/g.svg"
  Just (w, h, p2) <- singlePathFromFile' "../refs/a.svg"

  
  let dist  = D.normal 0 1
      count = 200

  r <- newIORef (mkStdGen 4)
  xs :: [Double] <- flip R.runRVar r $ replicateM count dist

  r <- newIORef (mkStdGen 5)
  ys :: [Double] <- flip R.runRVar r $ replicateM count dist

  let pts1 = toNormalisedPoints (centerXY p1)
      pts2 = toNormalisedPoints (centerXY p2)
      c    = circle 1 :: Path V2 Double

  let c1 = c 
             -- # deform' eps (wibble pts1)
             # roughenRandomly (zip xs ys)
             -- # scaleY 2
             # stroke
             # lw none
             # fc darkorchid
             # scale 0.3
             # centerXY

  let t2 :: Path V2 Double
      t2 = polygon ( with & polyOrient .~ NoOrient
                          & polyType .~ PolySides
                    [ 0 @@ deg, 90 @@ deg, 135 @@ deg]
                    [ 1/sqrt 2, 1/ sqrt 2, 1]
                   )

      -- d2 = t2
      --       -- # deform' 0.005 (wibble pts2)
      --       # roughenRandomly (zip xs ys)
      --       # strokeP
      --       # reflectY
      --       # centerXY
      --       # lw none
      --       # fc lightseagreen

  let d = c1 -- <> d2

  return $ d


-- | TODO: Fix. This is wildly terrible.
wibble :: [Point V2 Double] -> Deformation V2 V2 Double
wibble pts = Deformation $ \p ->
  let x      = p ^. _x
      y      = p ^. _y
      len    = fromIntegral (length pts)
      infPts = cycle pts
      phi    = atan2 (p ^. _x) (p ^. _y)
      m      = 2 * pi
      i      = round $ len * ((phi + pi) / m)
      pp     = infPts !! i
      px     = sin (pp ^. _x)
      py     = cos (pp ^. _y)
      f      = 2
   in (x + (px * f)) ^& (y + (py * f))
   -- in x ^& y
   -- in px ^& py
  

-- | Here's a different idea.
--
-- What if we draw a reference line that is scaled to be
-- between 0-1. Then, if that line is straight, then:
--
-- f (x, y) = (x, y)
--
-- Otherwise, we do something like:
--
-- f (x, y) = (x + dx, y + dy)
--   where
--    dx = lineX
--    dy = lineY
--
-- And all the need to do is ensure that everything has
-- the right length.
--
roughenByExample :: Path V2 Double
                 -> Path V2 Double
                 -> Path V2 Double
roughenByExample ref p =
  undefined


-- | Not really random, because actually we just do it w.r.t. 
--   the given list.
--
--   Our game will be to compute the angle.
--
-- NOTE:
--
--  - For some reason, we get a lot of the same angle.
--  - We also get a lot of the same points?!
--  - Something seems really wrong here.
--
roughenRandomly :: [(Double, Double)]
                -> Path V2 Double
                -> Path V2 Double
roughenRandomly diffs path = newPath
  where
    factor = 0.01
    newPoint pt (dx, dy) = pt + ((dx ^& dy) * factor)

    newPath = path # deform' eps f
    f    = Deformation $ \pt -> newPoint pt (cycle (diffs) !! (angleD pt))
    --
    angleD pt = let a = angle pt
                 in Debug.trace (show a <> " " <> show pt) a

    angle pt = floor $ (180 + (atan2 (pt ^. _x) (pt ^. _y))
                       * (180/pi)
                       -- Note: Suspicious ...
                       * (1/(eps))
                       )


    eps  = 0.01


-- | This is some crazy scheme where I decided to chunk up the path
--   into various parts. But I don't think there's actually any point.
roughen :: Path V2 Double
        -> Path V2 Double
roughen d = mm
  where
    steps  = 20

    points    = circleRays steps
    endPoints = map snd points

    lines' :: [Path V2 Double]
    lines' = map (uncurry (~~)) points

    is     = map (intersectPoints d) lines'
    mm     = fromVertices endPoints
    -- verts  = pathVertices mm


-- | The rays of a unit circle.
circleRays :: Double -> [(P2 Double, P2 Double)]
circleRays n = map f [0..n]
    where
        f k = ( 0 ^& 0, cos (k * a * pi / 180) ^& sin (k * a * pi / 180) )
        a = 360 / n
