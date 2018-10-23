{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine


main :: IO ()
main = mainWith (d # frame 0.2) >> putStrLn "Done!"


half :: Diagram B
half = t 
  where
    t      = mconcat (imap im [0.1,0.2..1])
    a      = wedge 1 xDir (180 @@ deg)
    im k s = if k `mod` 2 == 0 then a # scale s # lw none # fc white
                               else a # scale s # lw none # fc gold


d :: Diagram B
d = f <> (square 2.3 # bg gold # lw none)
  where
    f = lower <> upper' <> upper
    upper = half # snugB 
    lower = half # reflectY # snugT
    upper' = upper # scaleY 0.3

