{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ViewPatterns               #-}

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
    f  = liftA2 (,) (drawEdge mempty) mempty
    g  = liftA2 (,) mempty (drawPoly'' mempty)


    dd (es, ps) = viewRect (es <> ps)
    viewRect    = withEnvelope (rect w h :: Diagram B)

    inRect ((unr2 . toV2) -> (x,y)) = -w/2 <= x && x <= w/2 && -h/2 <= y && y <= h/2


drawPoly'' :: (RealFloat n, Renderable (Path V2 n) b, Typeable n) 
           => (Polygon -> Style V2 n) -> Polygon -> QDiagram b V2 n Any
drawPoly'' s p = poly <> square 0.5 # moveTo (centerPoint poly)
  where
    poly = applyStyle (s p) . fromVertices . map toP2 . polygonVertices $ p
  


-- drawTilingStyled :: (Renderable (Path R2) b, Backend b R2)
--                  => Style R2 -> (Polygon -> Style R2)
--                  -> Tiling -> Double -> Double -> Diagram b Any
-- drawTilingStyled eStyle pStyle t w h =
--   mkDia $ generateTiling t (0,0) (1,0) inRect

--             -- draw the edges and polygons into separate
--             -- diagrams, so we can make sure all the edges are
--             -- overlaid on top of all the polygons at the end
--             (liftA2 (,) (drawEdge eStyle) mempty)
--             (liftA2 (,) mempty (drawPoly pStyle))
--   where
--     inRect ((unr2 . toR2) -> (x,y)) = -w/2 <= x && x <= w/2 && -h/2 <= y && y <= h/2
--     mkDia (es, ps) = viewRect (es <> ps)
--     viewRect = withEnvelope (rect w h :: D R2)











moonBg :: Diagram B
moonBg =
  moon # centerXY <> square 3 # bg blue


moon :: Diagram B
moon = d # strokeP # fc white # lw 0
  where
    c1 = circle 1
    c2 = circle 0.7 # moveTo (p2 (0.5, 0))
    d  = B.difference Winding c1 c2

