{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

main :: IO ()
main = mainWith (d # frame 0.2) >> putStrLn "Done!"


d :: Diagram B
d = hsep 0.002 (map r' [1..100])
      # fc red
      # lc blue
  where
    r :: Path V2 Double
    r = fromOffsets $ [unitY]

    r' k = r # deform' 0.001 (wibble k)
           # strokeP


wibble :: Double -> Deformation V2 V2 Double
wibble k = Deformation $ \p ->
  ((p^._x) + f * cos ((p ^. _y) * tau + m * k)) ^& ((p ^. _y) + f * sin ((p ^. _x) * tau + m * k))
    where
      f = 0.01
      m = 3/tau

