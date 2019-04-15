{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE TemplateHaskell            #-}

module Main where

import Control.Monad (replicateM)
import Data.List.Split (chunksOf)
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude hiding (union)
import Diagrams.TwoD.Offset (expandPath)
import Diagrams.TwoD.Path.Boolean (union)
import GSL.Random.Quasi
import Nvds.Colours.ColourSets
import System.Random
import Data.List (intersperse)
import Development.GitRev          (gitCommitDate, gitCommitCount, gitHash)


-- TODO:
--
--  - Add self-replicating code path.

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
            len = length colours - 1

getPoints :: QRNGType -> Int -> IO [Point V2 Double]
getPoints qrngType n = do
    rng     <- newQRNG qrngType 2
    points  <- replicateM n (getListSample rng)

    return $ map (\[a,b] -> mkP2 a b) points



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
    d = hsep 0.002 (map r' [1..71])
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


bgColour = black
fgColour = white


logoStack =
  vsep 0.2 (take 5 $ repeat slogo)
    where
      slogo = logo 
                   # deform' 0.01 (wibble 2)
                   # strokeP
                   # centerXY 
                   # scale 0.1
                   # fc peachpuff
                   -- # lc (s "4169e1")
                   -- # fc (s "4169e1")
                   # lc bgColour
                   # lwL 0.001

llogo :: Diagram B
llogo = logo
          # deform' 0.01 (wibble 2)
          # strokeP
          # centerXY 
          # scale 0.1
          -- Scheme 1:
          -- # fc peachpuff
          -- # lwL 0.001
          --
          -- Scheme 2:
          # lc (s "4169e1")
          # fc bgColour
          # lwL 0.01
          --
          -- Scheme 3:
          -- # lc white
          -- # lwL 0.01

logos =
  hsep 0.2 [ logoStack
           , logoStack
           , logoStack
           , logoStack
           , logoStack
           ]


commands = [ "git clone https://github.com/silky/fashion.git silky-fashion"
           , "cd silky-fashion/Haskell-Waves/"
           , "git checkout " ++ $(gitHash)
           , "stack run -- -o design.png -w 2000"
           ]

line = unwords (intersperse "&&" commands)

replicateText 
  = text line # font "Fira Code"
              # fontSize (local 1.1)
              # bold
              <> rect 168 3
                  # fc white

-- d = randomLayout
d = zigzagLayout # bg bgColour
-- d :: IO (Diagram B)
-- d = do
--   putStrLn $ line
  -- return $ replicateText


gridLogos = 
  vcat (map hcat (chunksOf n ds))
    where
      n      = 9
      -- Square
      -- ds     = replicate (n*n) llogo'
      -- Diagonal
      ds     = concat $ replicate (n*n `div` 2) [llogo', phantom llogo']
      llogo' = llogo <> (phantom (square 0.5 :: Diagram B))



zigzagLayout :: Diagram B
zigzagLayout =
    replicateText # centerXY # scale 0.02
      # translateY (0.8)
    <>
    gridLogos # centerXY
    <>
    type1 # centerXY # scale 1.5


randomLayout :: IO (Diagram B)
randomLayout = do
      -- scale
    let s = 0.4 / 1.8
        a = 20 * 1 + 10
        start = 101

    points    <- getPoints halton (start + a)

    lambdas   <- mapM (mkPoint (llogo # scale s))
                      (take a $ drop start points)

    let diags = mconcat $ map position [ lambdas ]

    return $ diags # centerXY 
              <> type1 # centerXY 
                  # scale 0.4
  where
    colours = [ peachpuff ]
    mkPoint d p = do
      d' <- transforms colours d
      return (p, d')

-- git clone ... && stack run -- -o base_design.png -w 12000 && convert +crop ...  final-design.png
