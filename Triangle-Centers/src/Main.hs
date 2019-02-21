{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE DuplicateRecordFields      #-}

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
  } deriving (Show)


-- k :: Trilinear -> Triangle -> Double
-- k (Trilinear { x, y, z }) (T { area, a, b, c }) =
--   num / denom
--     where
--       num   = 2*area
--       denom = a*x + b*y + c*z
  

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
      }
  where
    -- https://en.wikipedia.org/wiki/Solution_of_triangles
    -- SAS case:
    --  Law of cosines
    --  Law of cosines
    --  Sum of angles
    γ    = acos $ (a**2 + b**2 - c**2) / (2 * a * b)
    b    = sqrt $ a**2 + c**2 - 2*a*c * cos (β ^. rad)
    area = (1/2) * a * b * sin γ

t (DT { a, β, c}) = polygon
      ( with
          & polyOrient .~ NoOrient
          & polyType   .~ PolySides 
            [ β ] 
            [ a , c ]
      )


d :: Diagram B
d = 
  t1 # lw 1 <> circle 0.293 # fc black # moveTo pv
  where
    tri@(DT { a, β, c })     = DT 1 (90 @@ deg) 1
    -- tl@(Trilinear {x, y, z}) = Trilinear (1/a) (1/b') (1/c)
    tl@(Trilinear {x, y, z}) = Trilinear 1 1 1
    t1                       = t tri
    nt                       = computeT tri
    b'                       = b nt
    --
    -- Moderate crimes against Haskell
    --
    [[av, bv, cv]]     = pathVertices (t1)

    -- From: https://en.wikipedia.org/wiki/Trilinear_coordinates
    k1   = a*x  / (a*x + b'*y + c*z)
    k2   = b'*y / (a*x + b'*y + c*z)
    k3   = c*z  / (a*x + b'*y + c*z)
    pv   = k1 *. av + k2 *. bv + k3 *. cv



main :: IO ()
main = mainWith (d # frame 0.2) >> putStrLn "Done!"
