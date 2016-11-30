{-# OPTIONS -W -Wno-unused-top-binds #-}
{-# LANGUAGE QuasiQuotes, BangPatterns #-}
module Lib (canny, toRGB, toIntensity
           ,IntensityConversion(..)
           ,floatOfBool
           ,magnitudeAndDirection
           ,blur
           ,supress
           ,threshold
           ,word8OfFloat, floatOfWord8, mapRGB ) where
import Data.Array.Repa
import Control.Monad
import qualified Control.Monad.ST as ST
import qualified Data.Array.Repa as Repa
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2
import Data.Word
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM


type F = Double
type Image a = Array U DIM2 a
type Direction' = Word8
data Direction = Vertical
               | Horizontal
               | PositiveDiagonal
               | NegativeDiagonal
type Magnitude = F
type RGB a = (a, a, a)
data IntensityConversion = Rec601Luma
                         | Rec709Luma
                         | Brightness
                         | Lightness
                         | Average
                         | MS
                         | RMS

toIntensity :: RealFloat a =>
               IntensityConversion ->
               RGB a -> a
toIntensity Rec709Luma (!r, !g, !b) =
  0.2126*r + 0.7152*g + 0.0722*b
toIntensity Rec601Luma (!r, !g, !b) =
  0.299*r + 0.587*g + 0.114*b
toIntensity Brightness (!r, !g, !b) =
  max (max r g) b
toIntensity Lightness (!r, !g, !b) =
  (min (min r g) b + max (max r g) b) / 2.0
toIntensity Average (!r, !g, !b) = (r + g + b) / 3.0
toIntensity MS (!r, !g, !b) =
  (r*r + g*g + b*b) / 3.0
toIntensity RMS (!r, !g, !b) =
  sqrt ((r*r + g*g + b*b) / 3.0)
{-# INLINE toIntensity #-}

slopeToDirection :: F -> Direction
slopeToDirection !s
  | tan(h - theta) <= s && s < tan(h + theta) = Horizontal
  | tan(p - theta) <= s && s < tan(p + theta) = PositiveDiagonal
  | tan(n - theta) <= s && s < tan(n + theta) = NegativeDiagonal
  | otherwise = Vertical
  where (h, p, n) = (0, pi / 4, - pi / 4)
        theta = pi / 8
{-# INLINE slopeToDirection #-}

direction :: Direction' -> Direction
direction 63 = Vertical
direction 255 = Horizontal
direction 127 = PositiveDiagonal
direction 191 = NegativeDiagonal
direction _ = error "direction: invalid argument"
direction' :: Direction -> Direction'
direction' Vertical = 63
direction' Horizontal = 255
direction' PositiveDiagonal = 127
direction' NegativeDiagonal = 191
{-# INLINE direction' #-}

data Strength = Strong | Weak | None
type Strength' = Word8

strength :: Strength' -> Strength
strength 0 = None
strength 127 = Weak
strength 255 = Strong
strength _ = error "strength: invalid argument"
{-# INLINE strength #-}
strength' :: Strength -> Strength'
strength' None = 0
strength' Weak = 127
strength' Strong  = 255
{-# INLINE strength' #-}
none' :: Strength'
weak' :: Strength'
strong' :: Strength'
none' = strength' None
weak' = strength' Weak
strong' = strength' Strong

magnitudeToStrength' :: F -> F -> Magnitude -> Strength'
magnitudeToStrength' !minval !maxval !x
  | x < minval = none'
  | x < maxval = weak'
  | otherwise  = strong'
{-# INLINE magnitudeToStrength' #-}

canny :: Monad m => F -> F -> Image F -> m (Image Bool)
canny minval maxval image =
  threshold
  =<< supress minval maxval
  =<< magnitudeAndDirection
  =<< blur image

magnitudeAndDirection :: Monad m =>
                         Image F ->
                         m (Image (Magnitude, Direction'))
magnitudeAndDirection image =
  do
    x <- sobelY image
    y <- sobelX image
    computeP $
      Repa.zipWith (,)
      (Repa.map sqrt (x*^x +^ y*^y))
      (Repa.map (direction' . slopeToDirection) (y /^ x))
        
sobelX :: Monad m => Image F
       -> m (Image F)
sobelX = computeP
  . mapStencil2 BoundClamp [stencil2| -1  0  1
                                      -2  0  2
                                      -1  0  1 |]

sobelY :: Monad m => Image F
       -> m (Image F)
sobelY = computeP
  . mapStencil2 BoundClamp [stencil2| 1  2  1
                                      0  0  0
                                     -1 -2 -1 |]
supress :: Monad m =>
           F -> F ->
           Image (Magnitude, Direction')
        -> m (Image Strength')
supress minval maxval image =
  computeP $ Repa.traverse image id f
  where
    f lookup' ix@(Z:.i:.j)
      | i == 0 || j == 0 ||
        i == rows - 1 || j == cols - 1 = none'
      | m1 <= m && m2 <= m =
        magnitudeToStrength' minval maxval m
      | otherwise = none'
      where
        (m, d') = lookup' ix
        mag = fst . lookup'
        (m1, m2) =
          case direction d' of
            Horizontal ->
              (mag (Z:.i-1:.j), mag (Z:.i+1:.j))
            Vertical ->
              (mag (Z:.i:.j-1), mag (Z:.i:.j+1))
            PositiveDiagonal ->
              (mag (Z:.i-1:.j+1), mag (Z:.i+1:.j-1))
            NegativeDiagonal ->
              (mag (Z:.i+1:.j+1), mag (Z:.i-1:.j-1))
        (Z:.rows:.cols) = extent image

threshold :: Monad m => Image Strength' -> m (Image Bool)
threshold image = do
  _ <- result `seq` return result
  return result
  where
    result = fromUnboxed sh $ ST.runST $ do
      tmp <- VM.replicate (rows*cols) False
      loop tmp
      V.unsafeFreeze tmp
    ixs = [Z:.i:.j | i <- [1..rows-2], j <- [1..cols-2]]
    sh@(Z:.rows:.cols) = extent image
    fromIx (Z:.(!i):.(!j)) = i * cols + j
    {-# INLINE fromIx #-}
    go :: DIM2 -> V.MVector s Bool -> ST.ST s ()
    go ix@(Z:.i:.j) tmp = do
      let k = fromIx ix
      b <- VM.read tmp k
      when (not b
           && i /= 0 && j /= 0
           && i /= rows - 1 && j /= cols - 1
           && (s' == weak' || s' == strong'))
        (do VM.write tmp k True
            go (Z:.i+1:.j) tmp
            go (Z:.i-1:.j) tmp
            go (Z:.i:.j+1) tmp
            go (Z:.i:.j-1) tmp
            go (Z:.i+1:.j+1) tmp
            go (Z:.i-1:.j-1) tmp
            go (Z:.i+1:.j-1) tmp
            go (Z:.i-1:.j+1) tmp)
        where
          s' = image Repa.! ix
    loop :: V.MVector s Bool -> ST.ST s ()
    loop tmp =
      forM_ ixs 
      (\ix -> 
         when (image Repa.! ix == strong')
         (go ix tmp))

      

blur :: Monad m => Image F -> m (Image F)
blur image = computeP . Repa.map (/ 256) =<< gaussian image
  where
    gaussian :: Monad m => Image F -> m (Image F)
    gaussian image =
      computeP
      $ mapStencil2 BoundClamp [stencil2| 1  4  6  4 1
                                          4 16 24 16 4
                                          6 24 36 24 6
                                          4 16 24 16 4
                                          1  4  6  4 1 |] image

floatOfBool :: RealFloat a => Bool -> a
floatOfBool True = 1
floatOfBool False = 0
{-# INLINE floatOfBool #-}

toRGB :: a -> RGB a
toRGB !y = (y, y, y)
{-# INLINE toRGB #-}

mapRGB :: (a -> b) -> RGB a -> RGB b
mapRGB !f (!r,!g,!b)= (f r, f g, f b)
{-# INLINE mapRGB #-}

word8OfFloat :: RealFloat a => a -> Word8
word8OfFloat !x | x < 0 = 0
                | x < 1.0 = floor (x * 256)
                | otherwise = 255
{-# INLINE word8OfFloat #-}

floatOfWord8 :: RealFloat a => Word8 -> a
floatOfWord8 !x = fromIntegral x / 256
{-# INLINE floatOfWord8 #-}
