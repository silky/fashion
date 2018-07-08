{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Main where

import Data.Typeable
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Tilings
import Data.List.Split (chunksOf)
import qualified Diagrams.TwoD.Path.Boolean as B

main :: IO ()
-- main = mainWith ( moon # frame 0.1 ) >> putStrLn "Done"
-- main = mainWith ( moonBg # frame 0.1 ) >> putStrLn "Done"
main = mainWith ( tiledMoon # frame 0.1 ) >> putStrLn "Done"


tiledMoon :: Diagram B
tiledMoon = dd (d')
  where
    t = t33434
    w = 10
    h = 10

    d' = generateTiling t (r2 (0,0)) (r2 (1,0)) inRect f g
    f  = liftA2 (,) (drawEdge (mempty # lw 0)) mempty
    g  = liftA2 (,) mempty (drawPoly')


    dd (es, ps) = viewRect (es <> ps)
    viewRect = withEnvelope (rect w h :: Diagram B)
    inRect ((unr2 . toV2) -> (x,y)) = -w/2 <= x && x <= w/2 && -h/2 <= y && y <= h/2


-- drawPoly' :: (RealFloat n, Renderable (Path V2 n) b, Typeable n) 
--            => Polygon -> QDiagram b V2 n Any
drawPoly' p = d
  where
    d = case polyFromSides . length . polygonVertices $ p of
          Triangle -> poly (mempty # lw 1) 
          Square   -> innerSq  <> (poly (mempty # lw 0 # fc blue) # scale 0.8 # centerXY) # moveTo cp
          _        -> error "Unsupported Polygon"

    poly s = drawPoly s p
    poly'  = poly mempty
    cp     = centerPoint poly'

    innerSq  = moon # scale 0.2 
                    # rotateBy ( (cp ^. _x) ^ 2 + (cp ^. _y) ^2 )
                    # moveTo (centerPoint poly')
    innerTri = moon # fc orange # scale 0.1 # rotateBy (1/2)  # moveTo cp
  



moonBg :: Diagram B
moonBg =
  moon # centerXY <> square 3 # bg blue


moon' = d 
  where
    c1 = circle 1
    c2 = circle 0.7 # moveTo (p2 (0.5, 0))
    d  = B.difference Winding c1 c2



moon :: Diagram B
moon = d # strokeP # recommendFillColor white # lw 0
  where
    c1 = circle 1
    c2 = circle 0.7 # moveTo (p2 (0.5, 0))
    d  = B.difference Winding c1 c2

