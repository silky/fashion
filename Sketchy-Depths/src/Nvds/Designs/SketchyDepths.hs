{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Nvds.Designs.SketchyDepths where

import           Data.Aeson
import           Data.List.Split (chunksOf)
import           Data.Maybe
import           Data.String.Conv (toS)
import           Data.Vector ((!))
import           Data.Word
import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude
import           GHC.Generics
import qualified Data.ByteString  as B
import qualified Data.Vector      as V


data RawSketch = RawSketch
  { recognized  :: !Bool
  , drawing     :: ![[[Double]]]
  } deriving (Show, Generic)

instance FromJSON RawSketch
instance ToJSON   RawSketch



path = "/home/noon/ml-data/quickdraw/"


d :: String 
  -> IO (Diagram B)
d f = do
  -- tornado
  -- cake
  -- not map
  -- not computer
  -- fan
  -- flower
  -- cloud
  -- laptop coloured in
  -- airplane coloured in
  -- let f = "camera"

  -- m  <- filter (not . recognized) <$> readJson (path <> f <> ".ndjson")

  m <- V.filter (recognized) <$> readJson (path <> f <> ".ndjson")

  let chunks   = 10
      sketches = map (\k -> drawSketch (m ! k)) [0..chunks*chunks - 1]
      ds       = vcat (map hcat (chunksOf chunks sketches))

  return $ ds


drawSketch :: RawSketch -> Diagram B
drawSketch sketch = rendered
  where
    strokes = drawing sketch
    -- img     = concat (map f strokes)
    img     = map f strokes
                -- # concat # fromVertices
                # mconcat
                -- # closeTrail
                -- # strokeTrail
                # lineJoin LineJoinRound
                # lineCap  LineCapRound
                # lw 1
                # fc gold
                -- # bg gold

    f [xs, ys] = fromVertices $ (zipWith (curry p2) xs ys)
    rendered   = square 300 # lw none <> img # centerXY
                            # reflectY
 
readJson :: (FromJSON a) => FilePath -> IO (V.Vector a)
readJson f = do
  file <- B.readFile f

  let lines' = lines (toS file)
      items' = map (decode' . toS) lines'

  return $ V.fromList $ take 1000 (catMaybes items')
