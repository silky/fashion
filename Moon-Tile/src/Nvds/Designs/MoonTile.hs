{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Nvds.Designs.MoonTile where

import Data.Typeable
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Tilings
import Data.List.Split (chunksOf)
import qualified Diagrams.TwoD.Path.Boolean as B


-- | Let's build this little wavey thing via arcs?
waveyThing :: Diagram B
waveyThing = d # rotateBy (1/8) # scaleY 1.7 
               # lw 0.2
  where
    d = (top <> leftSide) # centerXY <> 
        (top <> leftSide) # reflectXY # centerXY
          # moveOriginBy (r2 (-0.3, 0.34))

    leftSide = top # reflectX # rotateBy (1/4)
    top      = hsep 0 [corner, hsep (-0.5) (wws), corner # reflectX ]

    corner = circle 1
    -- corner = arc xDir (190 @@ deg)
    --           # scaleY 1.6
    --           # rotateBy ((1/8) - (1 / 36))
    --           # moveOriginBy (r2 (0, 0.2))

    wws = take 10 $ repeat ww
    ww = w <> w'
    w' = reflectY w # centerXY
    w = arc xDir (130 @@ deg) 
              # scaleY 1.4
              # rotateBy (1/9)
              # centerXY
              # moveOriginBy (r2 (-0.5, -0.9))


-- Some attempt to compute a nice little edge, but it doesn't look so good.
sinFunc :: Diagram B
sinFunc = 
  (hrule 10 :: Path V2 Double) # deform' 0.001 (g f1) 
      # strokeP
      # lw 50
      # rotateBy (1/12)
  where
    f1 x = sin ((x / 4) * tau)
    f2 x = cos ((x / 4) * tau)
    g f  = Deformation $ \p ->
            ( (p ^. _x) ^& f ( p ^. _x ) )



tiledMoon :: Diagram B
tiledMoon = drawEmbeddedTiling drawPolyForT6 t w h
                # rotateBy (1/12)
  where
    t = t6
    w = 2
    h = w


-- | As-is, it goes the colourful one, if you swap `diamond' c` for `diamond'
--   then it does the original one.
hexDiamond :: Diagram B
hexDiamond = 
  ( ((diamond # snugR # snugT <> diamond # reflectY # snugL # snugT)
              # snugB)
            <> diamond # rotateBy (1/3) # snugT
  ) # scale 0.99


-- One with colours
diamond' c = d
  where
    d = polygon ( with
            & polyOrient .~ NoOrient
            & polyType   .~ PolySides 
            [ 120 @@ deg , 60 @@ deg , 120 @@ deg]
            [ 1          , 1         , 1         ]
          )
          # centerXY
          # fc c


-- | The original
diamond :: Diagram B
diamond = moon # rotateBy (1/6) # scale 0.15 # centerXY 
          <> d # scale 0.8 # fc blue # lw 0
          -- <> (d :: Path V2 Double) # scale 0.87 # strokeP # dashingL [0.1, 0.1] 0 # lw 2
          --       -- # deform' 0.0001 g # strokeP # lw 2 # lc gray 
          <> waveyThing # scale 0.029 # rotateBy (-1/12)
          -- <> (d :: Path V2 Double) # scale 0.93 # strokeP # dashingL [0.01, 0.01] 0 
          --       # lw 10
          --       # lc orange
                -- # deform' 0.0001 g # strokeP # lw 2 # lc gray 
          <> d # fc white # lw 0
  where
    f x = cos ((x / 4) * tau)
    g = Deformation $ \p ->
      ( ((p ^. _x) + 0.02 * cos ((p ^. _y) * 10 * tau)) ^& 
        ((p ^. _y) + 0.02 * sin ((p ^. _y) * 30 * tau)) )

    d = polygon ( with
            & polyOrient .~ NoOrient
            & polyType   .~ PolySides 
            [ 120 @@ deg , 60 @@ deg , 120 @@ deg]
            [ 1          , 1         , 1         ]
          )
          # centerXY


drawPolyForT6 p = d
  where
    d = case polyFromSides . length . polygonVertices $ p of
          Hexagon -> poly (mempty # lw 0) <> hd # moveTo cp -- (innerSq  <> poly (mempty # lw 1 # fc blue)) 
          _       -> error "Unsupported Polygon"

    poly s = drawPoly s p
    poly'  = poly mempty
    cp     = centerPoint poly'
    hd     = hexDiamond # centerXY # rotateBy (1/12)

    innerSq  = moon # scale 0.2 
                    # rotateBy ( (cp ^. _x) ^ 2 + (cp ^. _y) ^2 )
                    # moveTo (centerPoint poly')


drawEmbeddedTiling drawPoly' t w h = wrapDiagram d
  where
    d = generateTiling t (r2 (0,0)) (r2 (1,0)) inRect f g
    f = liftA2 (,) (drawEdge (mempty # lw 0)) mempty
    g = liftA2 (,) mempty (drawPoly')
    --
    wrapDiagram (es, ps) = viewRect (es <> ps)
    viewRect = withEnvelope (rect w h :: Diagram B)
    inRect ((unr2 . toV2) -> (x,y)) = -w/2 <= x && x <= w/2 && -h/2 <= y && y <= h/2


moonBg :: Diagram B
moonBg =
  moon # centerXY <> square 3 # bg blue


moon :: Diagram B
moon = d # strokeP # recommendFillColor white # lw 0
  where
    c1 = circle 1
    c2 = circle 0.7 # moveTo (p2 (0.5, 0))
    d  = B.difference Winding c1 c2

