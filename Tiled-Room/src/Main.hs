module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import qualified Nvds.Designs.CubicLimitSeries  as CLS
import qualified Nvds.Designs.Tilings95         as T95
import qualified Nvds.Designs.MoonTile          as MT

main :: IO ()
main = do
  mainWith (bg white . frame 0.2 <$> d) >> putStrLn "Done!"


-- room :: Diagram B
-- room = roomLeft <> roomMiddle <> roomRight <> roomBottom
--   where
--     roomLeft   = mempty
--     roomRight  = mempty
--     roomMiddle = squareParts # map fromVertices # mconcat
--     roomBottom = mempty


d :: IO (Diagram B)
d = do
  d1 <- CLS.design

  let d2 = T95.dd
  let d3 = MT.tiledMoon 20 # scale 1

  d4 <- CLS.design

  let sq  = square 1 # lw 2 # lc blue
  let d1' = (d1 # scale 0.066 # centerXY <> sq)
                -- # clipTo (square 1)
                # clipBy (square 1)
                # withEnvelope (square 1 :: Diagram B)

  let d2' = (d2 # centerXY # scale 0.1 # bg white <> sq )
                -- # clipTo (square 1)
                # clipBy (square 1)
                # withEnvelope (square 1 :: Diagram B)
                # snugR # snugT

  let d3' = (d3 # scale 0.066 # centerXY <> sq)
                # clipTo (square 1)
                -- # withEnvelope (square 1 :: Diagram B)

  let roomMiddle = d1' # centerXY
                       # snugL # snugT


      roomLeft   = d2'-- # centerXY
                       # rotateBy (1/8)
                       # scaleY 2
                       # rotateBy ( 1 - ( atan (1/2) / (2 * pi) ) )
                       # scale (1/sqrt 2.5)

      roomRight   = d3' # rotateBy (1/8)
                        # scaleY 2
                        # rotateBy ( 1 - ( atan (1/2) / (2 * pi) ) )
                        # scale (1/sqrt 2.5)
                        -- # showEnvelope
                        # reflectX
                        # snugL # snugT
                        -- # showEnvelope
                        -- # showOrigin
                        -- # showOrigin
                        -- # showEnvelope
          

  let room = (roomLeft <> roomMiddle) # snugR # snugT
              <> roomRight 


  -- return roomLeft
  -- return $ vsep 0 [room, room # reflectY]
  return $ room
  -- return d1'

