module Main where


import Codec.BMP
import qualified Data.ByteString as BS
import System.Environment
import System.Exit
import System.Random
import System.IO

import Set3.C18 ( aesCTRModeGo )


main = do

  -- -- Abandoned because it turns out most files look fairly random...
  -- args <- getArgs
  -- fp <- case args of
  --         [fp] -> return fp
  --         _    -> do
  --           hPutStrLn stderr "usage: <prog> <filepath>"
  --           exitFailure
  -- bs <- BS.readFile fp

  -- gradually increasing grayscale shading
  let bs = BS.concat (map (\i -> BS.replicate 1000 i) [0..255])

  let nearestSquareLen = ceiling ((sqrt (fromIntegral (BS.length bs))) / 2)
      padLen = (4 * (nearestSquareLen ^ 2)) - BS.length bs
      padding = BS.replicate padLen 0
      ptBMP = packRGBA32ToBMP nearestSquareLen nearestSquareLen
                              (bs `BS.append` padding)

  putStrLn $ unlines [ "padLen: " ++ show padLen
                     , "bs length: " ++ show (BS.length bs)
                     , "nearestSquareLen: " ++ show nearestSquareLen
                     ]

  g <- getStdGen
  let rands = randoms g
      nonce = take 8 rands
      key = take 16 (drop 8 rands)
      encBS = aesCTRModeGo nonce key (BS.unpack bs)
      ctBMP = packRGBA32ToBMP nearestSquareLen nearestSquareLen
                              ((BS.pack encBS) `BS.append` padding)

  writeBMP "plaintext.bmp"  ptBMP
  writeBMP "ciphertext.bmp" ctBMP
