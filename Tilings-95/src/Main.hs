{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Data.List.Split (chunksOf)

main :: IO ()
-- main = mainWith ( tile red (tile blue mempty # scale 0.4) # frame 0.1 # bg white ) >> putStrLn "Done"
-- main = mainWith ( d 3 # frame 0.1 # bg white ) >> putStrLn "Done"
-- main = mainWith ( circleTile # frame 0.1 # bg white ) >> putStrLn "Done"
main = mainWith ( dd # frame 0.1 # bg white ) >> putStrLn "Done"
-- main = mainWith ( rose # frame 0.1 # bg white ) >> putStrLn "Done"


dd :: Diagram B
dd = vcat $ map hcat tss
  where
    t   = (circleTile <> d gold 4 # centerXY)
            # withEnvelope (rect 4.9 4.24 :: D V2 Double)
            -- # withEnvelope (square 4.24 :: D V2 Double)
            -- # showEnvelope

    ts  = take n2 $ repeat t
    tss = chunksOf n ts
    n2  = n * n
    n   = 2


-- It's no good.
rose :: Diagram B
rose = foldl f mempty datas
  where
    datas = zip (take 4 reds) (reverse [0..4 - 1])

    f diag (c, s) = diag <> base 
                        # fc c 
                        # scale (1 - (s/4))
                        # rotateBy (s/15)

    base  = regPoly 5 0.8 # lw 0
    reds  = cycle [ darkred, red, pink, indianred ]



circleTile :: Diagram B
circleTile = shape # lw 5 # lc white
             <> d blue 10
                  # bg pink 
                  # rotateBy (1/8)
                  # scale 0.5
                  # centerXY 
                  # clipBy shape
  where
    -- shape = regPoly 6 1
    shape = regPoly 8 1


d :: Colour Double -> Int -> Diagram B
d c n = vcat (map hcat tss)
  where
    t :: Diagram B
    -- t = tile blue (tile orange (tile blue mempty # scale 0.4) # scale 0.5 # rotateBy (1/8))
    t = tile c (tile magenta (square 1.5 # lc magenta <> inner) # scale 0.5 # rotateBy (1/8))
             -- Magic numbers that make things equal:
             -- # frame 0.09
             -- # frame 0.3
             -- # intrudeEnvelope (0 ^& 0.22)

    -- mkCols = foldl (\d' t' -> d' # snugB <> t' # snugT) mempty
    -- mkRow  = foldl (\d' t' -> d' # snugR <> t' # snugL) mempty
    -- inner = rose # scale 0.5 
    inner = regPoly 5 0.4 # fc red # lc red

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
    tbase = rht # deform' 0.001 wibble
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
      f = 0.02

