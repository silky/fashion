{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Prelude hiding (traverse)
import           Data.Word
import           Data.Array.Repa hiding ((++))
import           Data.Array.Repa.Stencil
import           Data.Array.Repa.Stencil.Dim2
import           Codec.Picture
import qualified Codec.Picture.Repa as R
import           Codec.Picture.Types
import           Control.Monad
import           Control.Monad.Primitive

-- http://hackage.haskell.org/package/JuicyPixels-repa-0.7.1.0/docs/Codec-Picture-Repa.html

main :: IO ()
main = do
  let w     = 100
      h     = w
      shape = Z :. w :. h
      cx    = w `div` 2
      cy    = h `div` 2
      array :: Array D DIM2 Int
      array = fromFunction shape (const 0)
  
  let stencil = makeStencil2 3 3 g
      g ix    = case ix of
                  Z :. -1 :.  0 -> Just 1
                  Z :.  0 :. -1 -> Just 1
                  Z :.  0 :.  1 -> Just 1
                  Z :.  1 :.  0 -> Just 1
                  _             -> Nothing

      array' :: Array PC5 DIM2 Int
      array' = mapStencil2 (BoundConst 0) stencil array
                           

  -- let array' :: Array D DIM3 Int
  --     array' = traverse array id f
  --     f g (Z :. 50 :. 50 :. 1) = g (Z :. cx :. cy :. 1) + 1
  --     f g (Z :. i  :.  j :. 1) = i
  
  -- Steps:
  --
  --  1. "Drop" a piece of sand at (cx, cy) (add one)
  --  2. If

  putStrLn $ "Done!"


computeSandpile :: Array PC5 DIM2 Word8
                -> Array PC5 DIM3 Word8
computeSandpile im = im


-- img :: FilePath
-- img = "/home/noon/tmp/ab-grey.png"

--main :: IO ()
--main = do
--  -- Juicy-Pixels Madness.
--  Right (ImageY8 im) <- readImage img

--  let p :: R.Img R.R
--      p@(R.Img imgData) = R.convertImage im
--  --
--  -- newImage <- computeSandpile im
--  --
--  let newImageData = computeSandpile imgData
--      newImage     = R.imgToImage (R.Img newImageData)

--  putStrLn $ "Wiring new image!"

--  (Left b) <- writeDynamicPng "../a.png" newImage

--  putStrLn $ "Result: " ++ show b
--  putStrLn $ "Done!"


-- TODO: This is insanely slow and crashes my computer.
--       But it works!
-- computeSandpile' :: Image Pixel8 -> IO (Image PixelRGB8)
-- computeSandpile' im = do
--   -- m  <- thawImage im
--   let h = 100
--       w = h

--   m <- createMutableImage h w (0 :: Pixel8)
--   o <- createMutableImage h w (PixelRGB8 0 0 0)

--   let my = mutableImageHeight m
--       mx = mutableImageWidth  m
--       cy = my `div` 2 - 1
--       cx = mx `div` 2 - 1

--   let ap i j f = do v <- readPixel m i j
--                     writePixel m i j (f v)

--   let steps = 1000
--   -- let steps = 30 * 1000

--   forM [0..steps - 1] $ \i -> do

--     -- At each step, drop something on to the center
--     ap cx cy (+1)

--     forM [0..mx - 1] $ \x -> do
--       forM [0..my - 1] $ \y -> do
--         p <- readPixel m x y

--         when (p >= 4) $ do
--           ap x y (\a -> a - 4)

--           when (x < mx - 1) $ ap (x+1) y (+1)
--           when (x > 0)      $ ap (x-1) y (+1)
--           when (y < my - 1) $ ap x (y+1) (+1)
--           when (y > 0)      $ ap x (y-1) (+1)


--   forM [0..mx - 1] $ \x -> do
--     forM [0..my - 1] $ \y -> do
--       p <- readPixel m x y
--       let p' = case p of
--                   0 -> PixelRGB8 255 255 255
--                   1 -> PixelRGB8 255 0   0
--                   2 -> PixelRGB8 0   255 0
--                   3 -> PixelRGB8 0   0   255
--                   _ -> PixelRGB8 0   0   0

--       writePixel o x y p'

--   freezeImage o



  -- m' <- last <$> replicateM 500 (sandpileStep m)

