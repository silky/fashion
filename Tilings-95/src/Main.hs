{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Data.List.Split (chunksOf)

main :: IO ()
main = mainWith ( d # frame 0.1 ) >> putStrLn "Done"


d :: Diagram B
d = mkCols (map mkRow tss)
  where
    t :: Diagram B
    t = tile # frame 0.09
             # intrudeEnvelope (0 ^& 0.22)

    mkCols = foldl (\d' t' -> d' # snugB <> t' # alignT) mempty
    mkRow  = foldl (\d' t' -> d' # snugR <> t' # alignL) mempty

    ts  = take n2 $ repeat t
    tss = chunksOf n ts
    n   = 3
    n2  = n * n


tile :: Diagram B
tile = (((box # snugB # snugR <> t1 # snugB # snugR) 
      # snugB # snugL <> t2 # snugB # snugL) # snugT # snugL <> t3 # snugT # snugL)
      # snugT # snugR <> t4 # snugT # snugR
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
                # fc black 
                # lw 0.1
                # lc black


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

