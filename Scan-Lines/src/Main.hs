{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine


main :: IO ()
main = mainWith (dd # frame 0.2) >> putStrLn "Done!"


dd = d # frame 0.2 # bg black


d :: Diagram B
d = sq    # lc green # centerXY
    <> sq # lc red   # centerXY # moveOriginBy (r2 (-0.25, -0.25))
    <> sq # lc blue  # centerXY # moveOriginBy (r2 (-0.5, 0.3))
  where
    r  = fromOffsets [unitY] 
    sq = hsep 0.03 [r | _ <- [1..35]]

