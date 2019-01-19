{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import           Data.List (sortBy)
import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude
import           Linear.V2
-- import qualified Numeric.LinearAlgebra.Static as SA
import qualified Numeric.LinearAlgebra        as LA


main :: IO ()
main = mainWith (d # frame 0.2) >> putStrLn "Done!"


testData :: [Double]
-- testData = [ 0, 1, 1, 0
--            , 1, 0, 0, 1
--            , 1, 0, 0, 1
--            , 0, 1, 1, 0
--            ]
testData = [ 0, 1, 0
           , 1, 0, 1
           , 0, 1, 0
           ]


dataWithXy :: [Double]
           -> Int
           -> Int
           -> [(Double, Double, Double)]
dataWithXy xs w h = imap (\i d -> (x i, y i, d)) xs
  where
    x i = ((fromIntegral w) / 2) - (fromIntegral $ i `div` w)
    y i = ((fromIntegral h) / 2) - (fromIntegral $ i `mod` h )


-- TODO:
--
--  Build up some matrix like:
--
--   0 1 1 1 1 0 
--   0 1 0 0 1 0
--   0 1 1 1 0 0
--
--  Then I can draw a line by connecting the dots with bezier curves.
--  We can get different numbers by kind of averaging across colours
--  and then just, per connected edge, or something, do something.

curves :: [(V2 Double, V2 Double, V2 Double)]
       -> Path V2 Double
curves pts = fromSegments path
  where
    path = map (\(a, b, c) -> bezier3 a b c) pts


d :: Diagram B
d = pd # strokeP
  <> circs

  where
    xs', xs :: [(Double, Double, Double)]
    xs' = filter (\(_, _, d) -> d == 1) $ dataWithXy testData 3 3

    xs = sortBy (\(a, b, _) (c, d, _) -> compare (atan2 a b) (atan2 c d)) xs'

    circs  = position $ zip (points) (repeat $ circle 0.3 # fc blue)
    points = map (\(a, b, _) -> p2 (a, b)) xs

    pts        = zip3 xs (drop 1 (cycle xs)) (drop 2 (cycle xs))
    -- bezierData = map (\((x0, y0, _), (x1, y1, _)) -> ( r2 (x0, y0) , r2 (x1, y1) , r2 (0, 0))) pts

    bezierData = map (\((x0, y0, _), (x1, y1, _), (x2, y2, _)) -> ( r2 (x0, y0) , r2 (x1, y1) , r2 (x2, y2))) pts
    pd         = curves bezierData
    -- pts        = zip xs (drop 1 (cycle xs))
    -- pd = mconcat $ map (\((x0, y0, _), (x1, y1, _)) -> p2 (x0,y0) ~~ p2 (x1,y1)) pts
