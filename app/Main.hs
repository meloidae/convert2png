module Main where

import           Lib                (chooseOutputDimension, getPixel)
import qualified Data.ByteString    as BS
import qualified Data.ByteString.Lazy as BSL
import           System.Environment (getArgs)
import           Codec.Picture      as CP
import           Codec.Picture.Png  as CPP

main :: IO ()
main = do
  [inPath, outPath] <- getArgs
  fileBS <- BS.readFile inPath
  let (width, height, extraBytes) = chooseOutputDimension $ BS.length fileBS
      image = CP.generateImage (getPixel fileBS width) width height
      png = CPP.encodePng image
  BSL.writeFile outPath png
  putStrLn $ "width=" ++ show width ++ ", height=" ++ show height ++ ", extra=" ++ show extraBytes
  return ()
