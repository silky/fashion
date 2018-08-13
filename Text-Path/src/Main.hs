{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine


main :: IO ()
main = mainWith (d # frame 0.2) >> putStrLn "Done!"


d :: Diagram B
d = ts # centerXY
       <> rect 10 15
  where
    t   = text "Hello!"

    sq n = square n # lw none
    hello = hello'
              blue
              pink
              yellow
              green
              orange

    text' t = (text t <> 
               text t # fontSize (local 1.1) # bold # fc black
               # translateY ( 0.1)
               # translateX (-0.1)
              )
                # font "Fira Code" # bold
                # centerXY

    -- text' t = text t # font "Uroob" # bold -- Gala likes this one
    -- text' t = text t # font "Purisa" # bold
    -- text' t = text t # font "Fira Code" # bold
    hello' c1 c2 c3 c4 c5 =
      hcat [ text' "H" # fc c1 <> sq 1  
           , text' "e" # fc c2 <> sq 0.6
           , text' "l" # fc c3 <> sq 0.3
           , text' "l" # fc c4 <> sq 0.3
           , text' "o" # fc c5 <> sq 0.5
           ]

    ys  = [0.5, 0.6 .. 2.5]
    ts  = mconcat $ map pos ys

    pos y = hello # moveTo (p2 (y, f y))
    f x   = exp x

