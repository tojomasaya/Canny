{-# OPTIONS -Wall #-}
module Main where
import System.Environment
import Data.Array.Repa as Repa
import Data.Array.Repa.IO.BMP as BMP
import Data.List as L
import Lib
import Data.Time
type F = Double

main :: IO ()
main = do
  args <- getArgs
  case args of
    [ src, dst, minval, maxval] ->
      case (reads minval, reads maxval) of
        ([(minv, "")], [(maxv, "")]) ->
          go src dst minv maxv
        _ -> putStrLn "Can't parse float point value."
    _ -> putStrLn "Usage: src dst minval maxval"

time :: String -> IO a -> IO a
time msg m = do
  start <- getCurrentTime
  x <- m
  end <- getCurrentTime
  putStrLn (msg L.++ ": "
             L.++ show (diffUTCTime end start))
  return x

go :: FilePath -> FilePath -> F -> F -> IO ()
go src dst minval maxval = do
  result <- BMP.readImageFromBMP src
  case result of
    Left s -> print s
    Right input -> do
      gray <- Repa.computeP
        $ Repa.map (toIntensity Rec601Luma
                    . mapRGB floatOfWord8) input

      tmp <- time "threshold" . threshold
        =<< time "supress" . supress minval maxval
        =<< time "magnitudeAndDirection" . magnitudeAndDirection
        =<< time "blur" (blur gray)
      output <- Repa.computeP
                $ Repa.map
                (toRGB . word8OfFloat
                 . (floatOfBool :: Bool -> Float)) tmp
      BMP.writeImageToBMP dst output                
      return ()


