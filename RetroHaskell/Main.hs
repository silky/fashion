{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import ColourSets
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Data.List (intersperse)
import Data.List hiding (union)
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude hiding (union)
import Diagrams.TwoD.Offset (expandPath)
import Diagrams.TwoD.Path.Boolean (union)
import GSL.Random.Quasi
import System.Random
import Data.List.Split (chunksOf)

-- TODO: Add stars! http://www.imagemagick.org/Usage/advanced/#stars
-- TODO:
--  - Some colour themes    

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



logo :: Diagram B
logo = Path trails # expandPath 0.2
                   # union Winding
                   # strokeP
                   # lw 0.1
                   # lc white
                   # centerXY
    where
        verts  = (map . map) p2 logoPoints
        trails = map fromVertices verts 


dropShadow :: Diagram B -> Diagram B
dropShadow d = d <> d # translateX 0.4
                      # translateY (-0.4)
                      # fc black


-- TODO: Use this.
wibble :: Deformation V2 V2 Double
wibble = Deformation $ \p ->
  ((p^._x) + 0.3 * cos ((p ^. _y) * tau)) ^& (p ^. _y)


bars :: Diagram B
bars = vcat' (with & sep .~ 0.5) (replicate 3 $ rect 10 0.3 # lw none)


getPoints :: QRNGType -> Int -> IO [Point V2 Double]
getPoints qrngType n = do
    rng     <- newQRNG qrngType 2
    points  <- replicateM n (getListSample rng)

    return $ map (\[a,b] -> mkP2 a b) points


transforms :: Colours -> Diagram B -> IO (Diagram B)
transforms colours d = do
    s <- (*1.5) <$> randomRIO (0.3, 1.2)
    r <- randomRIO (0,   360)
    c <- randomRIO (0, len)

    let colour = colours !! c

    return $ d # scale s
               # rotateBy (r/360)
               # fc colour
        where
            len     = length colours - 1



retroHaskell :: Colours -> IO (Diagram B)
retroHaskell colours = do
    -- scale
    let s = 0.01 / 1.8
        a = 55 * 2 + 10
        b = 15 * 2 + 10
        c = 1  * 2
        start = 101
        bgColour = white

    points    <- getPoints halton (start + 2*a + 2*b + c)

    lambdas   <- mapM (mkPoint (logo # dropShadow # scale s))
                      (take a $ drop start points)

    circles   <- mapM (mkPoint (circle 1 # lw none # dropShadow # scale s))
                      (take a $ drop (start + a) points)

    rects1    <- mapM (mkPoint (rect 8 1 # scale 0.7 # lw none # dropShadow # scale s))
                      (take b $ drop (start + 2*a) points)

    triangles <- mapM (mkPoint (triangle 3 # lw none # dropShadow # scale s))
                      (take b $ drop (start + b + 2*a) points)

    rects2    <- mapM (mkPoint (bars # dropShadow # scale s))
                      (take c $ drop (start + 2*b + 2*a) points)

    let diags = map position [ lambdas
                             , circles
                             , rects1
                             , triangles
                             , rects2
                             ]

    return $ mconcat diags # bg bgColour
  where
      mkPoint d p = do
          d' <- transforms colours d
          return (p, d')


-- stack run -- -- -w 500 -h 500 -o ./output/a.png
-- convert -delay 50 *.png a.gif
gif :: IO (Animation B V2 Double)
gif = do
    let n = 100

    frames <- replicateM n (retroHaskell colourPalette)
    -- 31 is something to do with how long it takes to run each step, or
    -- something.

    let anim = stretch (n / 31) (discrete frames)

    return anim


colourSample :: IO (Diagram B)
colourSample = do
    imgs <- mapM img allColours
    return $ vcat (map hcat (chunksOf 2 imgs))
  where
      img (cs, name) = do 
          d <- retroHaskell cs 

          let diag = (text name # scale 0.02 <> rect 1 0.1 # bg white # lw 0 <> d # centerXY) # frame 0.05

          return (diag)


colourPalette :: Colours
colourPalette = takoTank


-- main = mainWith gif
-- main = mainWith (retroHaskell colourPalette)
main = mainWith (colourSample)


