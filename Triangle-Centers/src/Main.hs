{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE DuplicateRecordFields      #-}


-- TODO:
--  - It isn't work for other triangle types yet.


module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine


data Trilinear = Trilinear
  { x :: Double
  , y :: Double
  , z :: Double
  } deriving (Show)


data Triangle  = T
  { area :: Double
  , a    :: Double
  , b    :: Double
  , c    :: Double

  -- Hmm
  , α    :: Angle Double
  , β    :: Angle Double
  , γ    :: Angle Double
  } deriving (Show)


k :: Trilinear -> Triangle -> Double
k (Trilinear { x, y, z }) (T { area, a, b, c }) =
  num / denom
    where
      num   = 2*area
      denom = a*x + b*y + c*z
  

data DiagramsTriangle = DT 
  { c :: Double
  , β :: Angle Double
  , a :: Double
  } deriving (Show)


computeT :: DiagramsTriangle -> Triangle
computeT (DT { c, β, a })
  = T { area = area
      , a    = a
      , b    = b
      , c    = c
      , α    = α @@ rad
      , β    = β
      , γ    = γ @@ rad
      }
  where
    -- https://en.wikipedia.org/wiki/Solution_of_triangles
    -- SAS case:
    --  Law of cosines
    b    = sqrt $ a**2 + c**2 - 2*a*c * cos (β ^. rad)
    --  Law of cosines
    γ    = acos $ (a**2 + b**2 - c**2) / (2 * a * b)
    --  Sum of angles
    α    = pi - β ^. rad - γ
    area = (1/2) * a * b * sin γ

-- This beta is the _exterior_ angle; we need it to be the interior one.
t (DT { c, β, a }) = polygon
      ( with
          & polyOrient .~ NoOrient
          & polyType   .~ PolySides 
            [ (180 - β ^. deg) @@ deg ] 
            [ c , a ]
      )

sec x = 1 / cos x

d = triangleWithCenter tri tc
  where
    tri = DT 1 (90 @@ deg) 1
    (T { area, a, b, c, α, β, γ }) = computeT tri

    -- tc              = incenter
    -- tc              = centerOfGravity
    -- tc              = circumcenter
    tc              = ninePointCenter

    incenter        = Trilinear 1 1 1

    centroid        = Trilinear (1/a) (1/b) (1/c)

    centerOfGravity = centroid

    circumcenter    = Trilinear (cos (α ^. rad)) (cos (β ^. rad)) (cos (γ ^. rad))

    orthocenter     = Trilinear (sec (α ^. rad)) (sec (β ^. rad)) (sec (γ ^. rad))

    ninePointCenter = Trilinear (cos (β ^. rad - γ ^. rad)) 
                                (cos (γ ^. rad - α ^. rad)) 
                                (cos (α ^. rad - β ^. rad)) 



triangleWithCenter :: DiagramsTriangle 
                   -> Trilinear 
                   -> Diagram B
triangleWithCenter tri@(DT { a, β, c }) tl@(Trilinear {x, y, z}) =
  (t1 # stroke # lw 1 <> circle r # fc black # moveTo pv)
  <> (mconcat $ zipWith (\pt c -> circle 0.04 # lw none # fc c # moveTo pt) [av, bv, cv] [red, blue, green])
  where
    nt = computeT tri
    b' = b nt
    t1 = t tri
    -- For fun: Make the radius equal to the equidistant
    --          length to the centerpoint.
    -- r = k tl nt
    --
    r = 0.02

    -- From: https://en.wikipedia.org/wiki/Trilinear_coordinates
    d  = a*x + b'*y + c*z

    k1 = a*x  / d
    k2 = b'*y / d
    k3 = c*z  / d

    -- Moderate crime against Haskell
    [[av, bv, cv]] = pathVertices t1
    pv             = (k1 *. av) + (k2 *. bv) + (k3 *. cv)



main :: IO ()
main = mainWith (d # frame 0.2) >> putStrLn "Done!"
