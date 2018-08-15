{-# LANGUAGE OverloadedStrings #-}

module Nvds.Svg (singlePathFromFile)
  where

import qualified Graphics.Svg as S
import           Graphics.Svg.Types ()
import           Linear.V2
import           Diagrams.Prelude


toNormalisedPoints :: Path V2 Double
                   -> [Point V2 Double]
toNormalisedPoints path = normalised
    where
      rawPoints  = concat (pathVertices path)

      -- TODO: Let's normalise here. The plan will be to suppose
      -- that all the points should be exactly d-spaced from each other.
      -- The problem is that that is "d" has measured along the curve?!?!
      --
      -- Actually it's very easy; because _if_ there is a big curve we need to
      -- follow, _then_ we can just pick a point that is on the curve (so it's
      -- just kind of close). When there isn't a point on the curve, _THEN_
      -- that means it's a straight-line anyway, so we can just partition it
      -- up exactly!
      --
      -- Brilliant!
      differences = map norm $ zipWith (-) rawPoints (drop 1 rawPoints)
      averageDiff = sum differences / (fromIntegral (length differences))
      --
      normalised  = rawPoints


-- | Just naively expect there to be exactly one path in the file, defined by
-- the typical cubic bezier thing. This is useful because it's exactly what we
-- get from the way I personally like to use InkScape.
singlePathFromFile :: FilePath 
                   -> IO (Maybe (Path V2 Double))
singlePathFromFile file = do
  mdoc <- S.loadSvgFile file
  return $ go mdoc
    where
      go Nothing  = Nothing
      go (Just doc) = 
        let (w, h) = dimensionInPixels doc
            convert = toSimplePath . oneSvgPath
         in Just $ convert doc
                    # scaleX (1/w)
                    # scaleY (1/h)
        

toSimplePath :: S.Path 
             -> Path V2 Double
toSimplePath svgPath = 
    path
  where
    percent  = 0.95
    beziers  = map (\(a, b, c) -> bezier3 a b c) (toCubicBezierParams svgPath)
    half     = length beziers `div` 2
    amount   = floor (fromIntegral half * percent)
    points   = take amount beziers
    path     = fromSegments points 


getPaths :: S.Document -> [S.Path]
getPaths doc = concat $ map (S.foldTree g s0) (S._elements doc)
  where
    s0 :: [S.Path]
    s0 = []

    g ps (S.GroupTree tree) = ps ++ concatMap extractPaths (S._groupChildren tree)
    g ps _                = ps

    extractPaths (S.PathTree path) = [path]
    extractPaths _ = []


oneSvgPath :: S.Document -> S.Path
oneSvgPath = head . getPaths


toCubicBezierParams :: S.Path 
                    -> [(V2 Double, V2 Double, V2 Double)]
toCubicBezierParams path = curves
  where
    cmds   = S._pathDefinition path
    curves = foldl g [] cmds

    g xs (S.CurveTo _ pts) = pts ++ xs
    g xs _ = xs


dimensionInPixels :: S.Document -> (Double, Double)
dimensionInPixels =  dimensionInPixels' 72


dimensionInPixels' :: S.Dpi -> S.Document -> (Double, Double)
dimensionInPixels' dpi doc = (w, h)
    where
      (w', h') = S.documentSize dpi doc
      (w,  h)  = (fromIntegral w', fromIntegral h')

