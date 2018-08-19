{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

main :: IO ()
main = mainWith (type2 # frame 0.2) >> putStrLn "Done!"


type2 :: Diagram B
type2 = d
  where
    d :: Diagram B
    d = hsep 0.05 (map r' [1..20])
          -- # fc red
          -- # lc blue
          # lw 10
      where
        r :: Path V2 Double
        r = fromOffsets $ [unitY] # scaleY 2

        r' k = r # deform' 0.001 (wibble k)
                 # strokeP
                 # lineTexture (mkLinearGradient (mkStops [(blue, 0, 0.5), (magenta, 0.5, 1)])
                                          ((-1) ^& (-1))
                                          (3 ^& 3)
                                          GradPad
                               )


    wibble :: Double -> Deformation V2 V2 Double
    wibble k = Deformation $ \p -> (x p ^& y p)
        where
          xy p = abs $ (p ^. _x) - (p ^. _y)
          x p  = (p ^. _x) + f * cos ((xy p) * tau + m * k)
          y p  = (p ^. _y) + f * sin ((xy p) * tau + m * k)
          f = 0.05
          m = 2/tau


-- | The original one
type1 :: Diagram B
type1 = d
  where
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

