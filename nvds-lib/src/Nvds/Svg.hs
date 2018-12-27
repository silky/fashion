{-# LANGUAGE OverloadedStrings #-}

module Nvds.Svg 
  ( singlePathFromFile
  , singlePathFromFile'
  , toNormalisedPoints
  )
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

      -- averageDiff = sum differences / (fromIntegral (length differences))
      -- stddev xs   = sqrt $ sum (map (\x -> (x - averageDiff) ** 2) xs) / (fromIntegral (length xs) - 1)

      differences = map norm $ zipWith (-) rawPoints (drop 1 rawPoints)
      smallest    = minimum differences
      normalised  = foldl f [] (zip rawPoints (drop 1 rawPoints))
      f xs (pt1, pt2) =
        let diff      = norm $ pt1 - pt2
            newPoints = if diff <= smallest then [pt1] else stepped
            steps     = map (/diff) [ 0, smallest .. (diff - smallest) ]
            stepped   = foldl (\xs s -> ((1-s) *. pt1 + s *. pt2) : xs) [] steps
         in newPoints ++ xs


singlePathFromFile' :: FilePath 
                    -> IO (Maybe (Double, Double, Path V2 Double))
singlePathFromFile' file = do
  mdoc <- S.loadSvgFile file
  return $ go' <$> mdoc
    where
      go' doc = 
        let (w, h)  = dimensionInPixels doc
            convert = toSimplePath . oneSvgPath
            mdim    = max w h
            w'      = w / mdim
            h'      = h / mdim
         in (w', h', convert doc # reflectY
                                 # scale (1/mdim)
                                 )


-- | Just naively expect there to be exactly one path in the file, defined by
-- the typical cubic bezier thing. This is useful because it's exactly what we
-- get from the way I personally like to use InkScape.
singlePathFromFile :: FilePath 
                   -> IO (Maybe (Path V2 Double))
singlePathFromFile file = do
  m <- singlePathFromFile' file
  let mpath = (\(_, _, path) -> path) <$> m
  return mpath
        

toSimplePath :: S.Path 
             -> Path V2 Double
toSimplePath svgPath = 
    path
  where
    -- TODO: Make this a parameter.
    percent  = 0.98 -- Was: 95
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
    g ps _                  = ps

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

