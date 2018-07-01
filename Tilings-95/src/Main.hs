{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Data.List.Split (chunksOf)

main :: IO ()
-- main = mainWith ( d # frame 0.1 # bg white ) >> putStrLn "Done"
-- main = mainWith ( circleTile # frame 0.1 # bg white ) >> putStrLn "Done"
main = mainWith ( dd # frame 0.1 # bg white ) >> putStrLn "Done"


dd :: Diagram B
dd = vcat (map hcat tss)
  where
    t   = (circleTile <> d 4 # centerXY)
            # withEnvelope (rect 5.7 4.8 :: D V2 Double)

    ts  = take n2 $ repeat t
    tss = chunksOf n ts
    n2  = n * n
    n   = 4


circleTile :: Diagram B
circleTile = circle 1 # lw 5 # lc white
             <> d 10 
                  # bg gold # scale 0.5 
                  # centerXY # clipBy (circle 1)


d :: Int -> Diagram B
d n = mkCols (map mkRow tss)
  where
    t :: Diagram B
    -- t = tile blue (tile orange (tile blue mempty # scale 0.4) # scale 0.5 # rotateBy (1/8))
    t = tile blue (tile magenta (square 1.5 # lc magenta) # scale 0.5 # rotateBy (1/8))
             -- Magic numbers that make things equal:
             -- # frame 0.09
             -- # intrudeEnvelope (0 ^& 0.22)
             # frame 0.3
             # intrudeEnvelope (0 ^& 0.22)

    mkCols = foldl (\d' t' -> d' # snugB <> t' # alignT) mempty
    mkRow  = foldl (\d' t' -> d' # snugR <> t' # alignL) mempty

    ts  = take n2 $ repeat t
    tss = chunksOf n ts
    n2  = n * n


tile :: Colour Double -> Diagram B -> Diagram B
tile colour d' = d' # centerXY <> ((((box # snugB # snugR <> t1 # snugB # snugR) 
      # snugB # snugL <> t2 # snugB # snugL) # snugT # snugL <> t3 # snugT # snugL)
      # snugT # snugR <> t4 # snugT # snugR) # centerXY
  where
    box   = sq -- # deform' 0.01 wibble 
               # strokeP
               # lw 0
               # lc gray

    t1    = tbase
    t2    = tbase # reflectX
    t3    = tbase # reflectXY
    t4    = tbase # reflectY
    tbase = rht # deform' 0.01 wibble
                # strokeP
                # scale 0.4
                # fc colour 
                # lw 0.1
                # lc colour


sq :: Path V2 Double
sq = square 1


rht :: Path V2 Double
rht = polygon ( with
        & polyType .~ PolySides 
        [ 135 @@ deg, 135 @@ deg  ]
        [ 1        , sqrt 2    , 1]
      )


wibble :: Deformation V2 V2 Double
wibble = Deformation $ \p ->
  ((p^._x) + f * cos ((p ^. _y) * tau)) ^& ((p ^. _y) + f * sin ((p ^. _x) * tau))
    where
      f = 0.01

