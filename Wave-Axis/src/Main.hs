{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine


main :: IO ()
main = mainWith (d # frame 0.2) >> putStrLn "Done!"


pattern :: Diagram B -> Diagram B
pattern d = t 
  where
    sizes  = map exp [0.1, 0.2 .. 10]
    -- sizes  = [0.1, 0.2 .. 1]
    t      = mconcat (imap im sizes)
    im k s = if k `mod` 2 == 0 then d # scale s # lw none # fc white
                               else d # scale s # lw none # fc colour

half :: Diagram B
half = pattern (wedge 1 xDir (180 @@ deg))


colour :: Colour Double
-- colour = mediumpurple
colour = cornflowerblue


d :: Diagram B
d = f # bg colour
  where
    f      = lower <> upper' <> upper
    upper  = half # snugB
    lower  = half # reflectY # snugT
    upper' = upper # scaleY 0.3

