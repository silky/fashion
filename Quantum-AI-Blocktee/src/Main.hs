{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Data.List
import Data.IORef (newIORef)
import System.Random
import Data.Random
import Nvds.Colours.ColourSets

main :: IO ()
main = mainWith (frame 0.2 <$> d) >> putStrLn "Done!"


rangle :: Diagram B -> Diagram B
rangle b = hsep 0.1 [ bar , b , r ] 
  where
    bar = (0 ^& (-0.5)) ~~ (0 ^& 0.5)
    r   = fromVertices [ (0 ^& (-0.5))
                       , (0.2 ^& 0)
                       , (0 ^& 0.5)
                       ]


tee :: [Colour Double] -> Int -> Int -> Diagram B
tee colours n m =
  ((circuit # center <> rect 30 7 # fc white # lw none ) # scale 0.7) # padY 2 # alignT
  <> surfaceCode colours n m # center # alignT


surfaceCode :: [Colour Double] -> Int -> Int -> Diagram B
surfaceCode colours n m = grid
  where
    cs1 = colours
    cs2 = drop 100 colours

    oc    = circle 0.15 # lw (local 0.05)

    -- TODO: Use the colours somehow!
    zblob c = blob "Z" c
    xblob c = blob "X" c ||| oc

    zrow k = hsep 0 $ intersperse oc (map (\i-> zblob (cs1 !! (i*k)) ) [1..m-1])
    
    -- xrow  = hsep 0 $ take m (phantom (xblob # scaleX 0.005) : repeat xblob)
    -- TODO: Hmm, a bit hacky because of the hardcoded strut; but it works,
    --       so ... meh. Should be possible with scaling the original, but
    --       doesn't work for some reason.
    xrow k = hsep 0 $ oc : take m (strutX 0.001 : map (\i -> xblob (cs2 !! (i*k)) ) [1..m-2])

    --
    -- grid  = vsep (-0.9) $ xrow : take n (cycle [zrow, xrow])
    grid  = vsep (-0.9) $ xrow 1 : take n (concat (map (\i -> [zrow i, xrow i]) [1..n-1]))
    -- cycle [zrow 1, xrow 1])


blob :: String -> Colour Double -> Diagram B
blob str colour = 
  mainBlob # pad 1.12
  where
    r = (local 0.05)
    mainBlob = 
      around # centerXY # scale 0.4 <> 
        (circle 0.15 # fc black # lw r <>
          seg # fc colour # lw none # centerXY)

    around = m === (m ||| phantom cc ||| m) # center === m
    cc :: Diagram B
    cc = square 1 # pad 3
    m = t str # fc white
          <> phantom (rect 1 1 :: Diagram B)
    -- Spline
    seg = cubicSpline True pts
    a = 0.2
    b = 0.3
    c = 1
    pts = map p2
           [ (-a, a)  , (-b, c)  , (b, c)
           , (a, a)   , (c, b)   , (c, -b)
           , (a, -a)  , (b, -c)  , (-b, -c)
           , (-a, -a) , (-c, -b) , (-c, b)
           ]


measurement :: Diagram B
measurement =
  square 1.4
    <> (arc xDir (180 @@ deg) # scale 0.5 # centerXY)
    <> (0 ^& (-0.3)) ~~ (0.5 ^& 0.5)


t :: String -> Diagram B
t m = text m # font "firacode"
             # fontSize (local 0.8)


quantum :: Diagram B
quantum = t "Quantum" <> rect 5 1.2


ai :: Diagram B
ai = t "AI" <> rect 2 1.2


blockchain :: Diagram B
blockchain = t "Blockchain" <> rect 7 1.2


-- TODO: Change the line widths; I think they all got bumped up somehow.
circuit :: Diagram B
circuit = c # lastThing
            -- Row 1
            # link "dr1" "quantum"  (0 @@ deg)   (180 @@ deg)
            # link "quantum" "d1-1" (0 @@ deg)   (180 @@ deg)
            # link "d1-1" "c2-1"    (270 @@ deg) (90 @@ deg)
            # link "d1-1" "d1-2"    (0 @@ deg)   (180 @@ deg)
            # link "d1-2" "d2-1"    (270 @@ deg) (90 @@ deg)
            -- Row 2
            # link "dr2" "c2-1"     (0 @@ deg)   (180 @@ deg)
            # link "c2-1" "ai"      (0 @@ deg)   (180 @@ deg)
            # link "ai" "d2-1"      (0 @@ deg)   (180 @@ deg)
            # link "d2-1" "c3-1"    (270 @@ deg) (90 @@ deg)
            -- Row 3
            # link "dr3" "blockchain"   (0 @@ deg)   (180 @@ deg)
            # link "blockchain" "c3-1"  (0 @@ deg)   (180 @@ deg)
            # link "c3-1" "measurement" (0 @@ deg)   (180 @@ deg)

  where
    link a b a1 a2
      = connectPerim' (with & arrowHead .~ noHead) a b a1 a2

    lastThing = withName "d2-1" $ \n1 ->
                withName "dr3"  $ \n2 ->
                  atop (cross # named "c3-1" # moveTo ((location n1 ^. _x) ^& (location n2 ^. _y) )
                       )

    c = row1 === strutY 1 === row2 === strutY 1 === row3
    row1   = hsep 3 [ dr # named "dr1"
                    , quantum # alignL # named "quantum"
                    , dot # named "d1-1"
                    , phantom ai
                    , dot # named "d1-2" ]
    row2   = hsep 3 [ dr # named "dr2"
                    , phantom quantum
                    , cross # named "c2-1"
                    , ai # named "ai"
                    , dot # named "d2-1"
                    ]
    row3   = hsep 3 [ dr # named "dr3"
                    , phantom (rect 2 1 :: Diagram B)
                    , blockchain # named "blockchain"
                    -- , cross # named "c3-1"
                    , strutX 0.3
                    , measurement # named "measurement"
                      ||| strutX 0.6
                      ||| (t "$$$..." <> rect 3 1 # lw none) # scale 0.7
                    ]

    dot    = circle 0.2 # fc black
    cross  = circle 0.2
              <> (-0.2 ^& 0) ~~ (0.2 ^& 0)
              <> (0 ^& (-0.2)) ~~ (0 ^& 0.2)
    dr     = rangle dollar
    dollar = t "$" <> rect 0.5 1 # lw none


d :: IO (Diagram B)
-- d = rangle dollar
-- d = quantum  ||| ai ||| blockchain
-- d = circuit
-- d = blob "X" orange 
-- d = qubitRow 10
-- d = qubitGrid 10 10
-- d = surfaceCode 10 10
d = do
  let n = 10
      m = 10

  -- let colours = cycle [green, green, blue, orange]

  -- let colourSet = goldfish -- Noon's Fav
  let colourSet = anythingBut
      repped    = concat (replicate 100 colourSet)

  let mcolours = shuffleNofM ((n+2)*(m+2)) (length repped) repped

  r <- newIORef (mkStdGen 2)
  colours <- runRVar mcolours r
  
  
  -- TODO: Compute a nxm number of list

  return $ tee colours n m

