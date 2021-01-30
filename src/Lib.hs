module Lib(
  chooseOutputDimension,
  getPixel
) where

import           Data.List       (find, minimumBy)
import           Data.Function   (on)
import qualified Data.ByteString as BS
import           Codec.Picture

data Dimension = Dimension { width :: Int, height :: Int, extra :: Int } deriving (Show)

chooseOutputDimension :: Int -> (Int, Int, Int)
chooseOutputDimension nBytes = (w, h, e)
  where nPixels = ceiling $ (fromIntegral nBytes :: Float) / 3.0
        sqrtMax = ceiling $ sqrt (fromIntegral nPixels)
        dimensions = filter (isBigEnough nPixels) $ map (dimensionInfo nPixels nBytes) [sqrtMax, sqrtMax - 1 .. 1]
        bestDimension = minimumBy (compare `on` extra) dimensions
        (w, h, e) =  (width bestDimension, height bestDimension, extra bestDimension)

dimensionInfo :: Int -> Int -> Int -> Dimension
dimensionInfo nPixels nBytes w
  | nPixels `mod` w == 0 = Dimension w h $ extraBytes w h nBytes
  | otherwise = Dimension w (h + 1) $ extraBytes w (h + 1) nBytes
  where h = nPixels `div` w

extraBytes :: Int -> Int -> Int -> Int
extraBytes w h nBytes = w * h * 3 - nBytes

isBigEnough :: Int -> Dimension -> Bool
isBigEnough nPixels d = width d * height d >= nPixels

getPixel :: BS.ByteString -> Int -> Int -> Int -> PixelRGB8
getPixel bytes w x y 
  | idx >= nBytes = PixelRGB8 0 0 0 
  | idx == nBytes - 1 = PixelRGB8 (bytes `BS.index` idx) 0 0
  | idx == nBytes - 2 = PixelRGB8 (bytes `BS.index` idx) (bytes `BS.index` (idx + 1)) 0
  | otherwise = PixelRGB8 (bytes `BS.index` idx) (bytes `BS.index` (idx + 1)) (bytes `BS.index` (idx + 2))
  where idx = (w * y + x) * 3
        nBytes = BS.length bytes
