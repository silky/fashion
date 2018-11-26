{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

import           Codec.Picture.Bitmap
import           Codec.Picture.Types
import           Control.Monad.Random
import           Data.Serialize
import           Data.Word (Word8)
import           GHC.TypeLits
import           Grenade
import           Options.Applicative
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as V
import qualified Numeric.LinearAlgebra.Static as SA


type ZDim      = 5
type TotalDim  = 2+5
type OutDim    = 3
type Hidden    = 100

-- type BatchSize = 500 * 500

type CppnNet = Network '[ FullyConnected TotalDim Hidden , Tanh
                        , FullyConnected Hidden   Hidden , Softmax
                        , FullyConnected Hidden   Hidden , Tanh
                        , FullyConnected Hidden   OutDim , Logit
                        ]
                       '[ 'D1 TotalDim
                        , 'D1 Hidden   , 'D1 Hidden
                        , 'D1 Hidden   , 'D1 Hidden 
                        , 'D1 Hidden   , 'D1 Hidden
                        , 'D1 OutDim   , 'D1 OutDim
                        ]


randomNet :: MonadRandom m => m CppnNet
randomNet = randomNetwork


buildInputs :: Int 
            -> Int 
            -> S ('D1 ZDim)
            -> [ S ('D1 TotalDim) ]
buildInputs h w (S1D z') = vects
  where
    z      = V.toList (SA.unwrap z')
    start  = -1
    end    = 1
    range  = end - start
    -- TODO: Presently only works for a square.
    step   = range / (fromIntegral w - 1)
    pts    = [ start, start + step .. end ]
    points = [ [x, y] ++ z | x <- pts, y <- pts ]

    vects  = map (S1D . SA.vector) points


netForward :: CppnNet 
           -> Int 
           -> Int 
           -> IO (CppnNet, Image PixelRGB8)
netForward net width height = do

  z <- do
    seed <- getRandom
    return $ S1D (SA.randomVector seed SA.Uniform :: SA.R ZDim)

  let inputs       = buildInputs width height z
      outputs      = map (runNet net) inputs
      extractPixel = toPixel . extract

      pixels :: V.Vector (PixelBaseComponent PixelRGB8)
      pixels = V.fromList (concatMap extractPixel outputs)

      img :: Image PixelRGB8
      img = Image width height pixels

  return $ (net, img)



toPixel :: (Double, Double, Double) 
        -> [Word8]
toPixel (r,g,b) = [ fromIntegral $ round (r * 255)
                  , fromIntegral $ round (g * 255)
                  , fromIntegral $ round (b * 255) ]


extract :: S ('D1 OutDim) -> (Double, Double, Double)
extract (S1D a) = let [r,g,b] = V.toList (SA.unwrap a)
                    in (r,g,b)


netLoad :: FilePath -> IO CppnNet
netLoad modelPath = do
  modelData <- B.readFile modelPath
  either fail return $ runGet (get :: Get CppnNet) modelData


data CppnOpts = CppnOpts Int Int (Maybe FilePath) (Maybe FilePath)


opts :: Parser CppnOpts
opts =
  CppnOpts <$> option auto (long "width"  <> short 'w' <> value 500)
           <*> option auto (long "height" <> short 'h' <> value 500)
           <*> optional (strOption (long "load"))
           <*> optional (strOption (long "save"))


main :: IO ()
main = do
  CppnOpts width height load save <- execParser (info (opts <**> helper) idm)

  net0 <- case load of
    Just loadFile -> netLoad loadFile
    Nothing -> randomNet

  (net, image) <- netForward net0 width height


  writeBitmap "test.png" image

  case save of
    Just saveFile -> B.writeFile saveFile $ runPut (put net)
    Nothing -> return ()

  putStrLn $ "Done!"
