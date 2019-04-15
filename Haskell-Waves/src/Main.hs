{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Main where

import Diagrams.Prelude hiding (union)
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Path.Boolean (union)
import Diagrams.TwoD.Offset (expandPath)


s = sRGB24read

main :: IO ()
main = mainWith (d) >> putStrLn "Done!"


-- TODO: Make this less hacky
logoPoints :: [[(Double, Double)]]
logoPoints = [ -- >
               [ (0, 3), (1.2, 1.5), (0,0) ]
               -- \
             , [ (0.8, 3), (0.8 + (2 * 1.2), 0) ]
               -- /
             , [ (0.8, 0), (0.8 + 1.2, 1.5) ]
               -- =
             , [ (2.2, 1.85), (4, 1.85) ]
             , [ (2.7, 1.32), (4, 1.32) ]
             ]



-- logo :: Diagram B
logo = Path trails # expandPath 0.2
                   # union Winding
    where
        verts  = (map . map) p2 logoPoints
        trails = map fromVertices verts 



-- | The original one
type1 :: Diagram B
type1 = d # rotateBy (1/4)
  where
    d :: Diagram B
    d = hsep 0.002 (map r' [1..80])
          # fc red
          # lc (s "4169e1")
      where
        r :: Path V2 Double
        r = fromOffsets $ [unitY * 3]

        r' k = r # deform' 0.001 (wibble k)
                 # strokeP


wibble :: Double -> Deformation V2 V2 Double
wibble k = Deformation $ \p ->
  ((p^._x) + f * cos ((p ^. _y) * tau + m * k)) 
             ^& ((p ^. _y) + f * sin ((p ^. _x) * tau + m * k))
    where
      f = 0.02
      m = 3/tau



logoStack =
  vsep 0.2 (take 5 $ repeat slogo)
    where
      slogo = logo 
                   -- # deform' 0.01 (wibble 2)
                   # strokeP
                   # centerXY 
                   # scale 0.1
                   # fc white
                   # lc (s "4169e1")
                   # lwL 0.009


logos =
  hsep 0.2 [logoStack, logoStack]

d :: Diagram B
d = logos # centerXY <> 
      type1 # centerXY 

