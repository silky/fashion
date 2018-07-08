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
-- main = mainWith ( tiledMoon1 # frame 2 ) >> putStrLn "Done"
-- main = mainWith ( diamond # frame 0.1 ) >> putStrLn "Done"
-- main = mainWith ( hexDiamond # frame 0.1 ) >> putStrLn "Done"
main = mainWith ( tiledMoon2 # frame 2 ) >> putStrLn "Done"


tiledMoon2 :: Diagram B
tiledMoon2 = drawEmbeddedTiling drawPolyForT6 t w h
                # rotateBy (1/12)
  where
    t = t6
    w = 10
    h = 10



hexDiamond :: Diagram B
hexDiamond = ((diamond # snugR # snugT <> diamond # reflectY # snugL # snugT)
              # snugB)
            <> diamond # rotateBy (1/3) # snugT


diamond :: Diagram B
diamond = moon # scale 0.15 # centerXY 
          <> d # scale 0.8 # fc blue
          <> d # fc white
  where
    d = polygon ( with
            & polyOrient .~ NoOrient
            & polyType   .~ PolySides 
            [ 120 @@ deg , 60 @@ deg , 120 @@ deg]
            [ 1          , 1         , 1         ]
          )
          # lw 1
          # lc black
          # centerXY

drawPolyForT6 p = d
  where
    d = case polyFromSides . length . polygonVertices $ p of
          Hexagon -> poly (mempty # lw 0) <> hd # moveTo cp -- (innerSq  <> poly (mempty # lw 1 # fc blue)) 
          _        -> error "Unsupported Polygon"

    poly s = drawPoly s p
    poly'  = poly mempty
    cp     = centerPoint poly'
    hd     = hexDiamond # centerXY # rotateBy (1/12)

    innerSq  = moon # scale 0.2 
                    # rotateBy ( (cp ^. _x) ^ 2 + (cp ^. _y) ^2 )
                    # moveTo (centerPoint poly')


-- drawEmbeddedTiling :: 
drawEmbeddedTiling drawPoly' t w h = wrapDiagram d
  where
    d = generateTiling t (r2 (0,0)) (r2 (1,0)) inRect f g
    f = liftA2 (,) (drawEdge (mempty # lw 0)) mempty
    g = liftA2 (,) mempty (drawPoly')
    --
    wrapDiagram (es, ps) = viewRect (es <> ps)
    viewRect = withEnvelope (rect w h :: Diagram B)
    inRect ((unr2 . toV2) -> (x,y)) = -w/2 <= x && x <= w/2 && -h/2 <= y && y <= h/2



-- | Not quite right, but indicative of the ideas.
tiledMoon1 :: Diagram B
tiledMoon1 = drawEmbeddedTiling drawPolyForT33434 t w h
  where
    t = t33434
    w = 10
    h = 10


drawPolyForT33434 p = d
  where
    d = case polyFromSides . length . polygonVertices $ p of
          Triangle -> (poly (mempty # lw 0 # fc orange) # scale 0.8 # centerXY) # moveTo cp 
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

