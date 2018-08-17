{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Nvds.Svg


main :: IO ()
main = mainWith (frame 0.02 <$> d) >> putStrLn "Done!"


ifilter :: (Int -> Bool) -> [a] -> [a]
ifilter p xs = map snd $ filter (p . fst) (zip [1..] xs)


d :: IO (Diagram B)
d = do
  -- Supposing we "stack run ..." from the "./src" directory.
  Just (w, h, p) <- singlePathFromFile' "../svg-paths/j-small.svg"

  let pts = ifilter (\i -> (i `mod` 5) == 0) (toNormalisedPoints p)
              # scale 4
      ts  = mconcat (map (\pt -> gala'' (pt ^. _y) # scale (1/30) # moveTo pt) (reverse pts))

  return $ ts # centerXY
            <> rect w h
              # fc pink
              # lc pink
  where

    -- text' t = text t # font "Uroob" # bold -- Gala likes this one
    -- text' t = text t # font "Purisa" # bold
    -- text' t = text t # font "Fira Code" # bold
    start = 0.5
    step  = 0.3
    end   = 7
    ys  = reverse [start, start + step .. end]
    -- ys  = reverse [0.5, 0.6 .. 2.5]
  
    --
    -- Old way, with a function:
    -- ts  = mconcat (finalT : map pos ys)
    -- finalT = pos' (gala' white white white white) (head ys + step)
    -- finalT = pos' (cc) (head ys + step)

    pos y = gala # moveTo (p2 (y, f y))
    pos' d y = d # moveTo (p2 (y, f y))
    f x   = (x ** 2) / 4

text' t = (text t  <> 
            text t # fontSize (local 1.1) # bold # fc black
            # translateY ( 0.1)
            # translateX (-0.1)
          )
          # font "Fira Code" # bold
          # centerXY

sq n = square n # lw none

cc = circle 1 # fc red

gala' c1 c2 c3 c4 =
  hcat [ text' "G" # fc c1 <> sq 1  
       , text' "a" # fc c2 <> sq 0.6
       , text' "l" # fc c3 <> sq 0.3
       , text' "a" # fc c4 <> sq 0.5
       ]

gala'' v = gala'
            (blend (1 - v) deepskyblue mediumaquamarine)
            (blend (1 - v) mediumslateblue salmon)
            (blend (1 - v) mediumaquamarine deepskyblue)
            (blend (1 - v) salmon mediumslateblue)

gala = gala'
          deepskyblue
          salmon
          mediumaquamarine
          mediumslateblue

hello' c1 c2 c3 c4 c5 =
  hcat [ text' "H" # fc c1 <> sq 1  
       , text' "e" # fc c2 <> sq 0.6
       , text' "l" # fc c3 <> sq 0.3
       , text' "l" # fc c4 <> sq 0.3
       , text' "o" # fc c5 <> sq 0.5
       ]

hello = hello'
          cyan
          salmon
          yellow
          green
          orange

