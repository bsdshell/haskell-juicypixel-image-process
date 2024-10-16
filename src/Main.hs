-- {{{ begin_fold
-- script
-- #!/usr/bin/env runhaskell -i/Users/cat/myfile/bitbucket/haskelllib
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-} 
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
-- import Turtle
-- echo "turtle"

-- import Data.Set   -- collide with Data.List 
import Data.Char
import Data.Typeable (typeOf) -- runtime type checker, typeOf "k"
import qualified Data.List as L
import Data.List.Split
import Data.Time
import Data.Time.Clock.POSIX
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.IO
import System.Posix.Files
import System.Posix.Unistd
import System.Process
import Text.Read
import Text.Regex
import Text.Regex.Base
import Text.Regex.Base.RegexLike
import Text.Regex.Posix
import Data.IORef 
import Control.Monad (unless, when, forM_)
import Control.Concurrent 

import qualified Text.Regex.TDFA as TD

import AronModule 

import           Codec.Picture
import           Codec.Picture.Extra hiding (rotateLeft90, rotateRight90)
import           Codec.Picture.Drawing
import           Codec.Picture.Types
import           Control.Monad.Primitive
import           Data.Word8
import Data.Foldable (foldlM)
import Control.Monad (void)



geneImg::IORef Int -> Int -> Int -> IO()
geneImg ref w h = do
    n <- readIORef ref
    let pngName = "00" ++ (show n) ++ ".png"
    img <- withMutableImageX w h (PixelRGB8 150 0 0) $ \m -> do
      -- A green diagonal line
      drawLine m 0 0 (w - 1) (h - 1) (PixelRGB8 0 255 0)

      -- A blue square at a 45-degree angle
      drawPolygon m [(50, 50), (75, 75), (100, 50), (75, 25), (50, 50)] (PixelRGB8 0 0 255)

      -- An orange bounding rectangle
      drawRectangle m 0 0 (w - 1) (h - 1) (PixelRGB8 255 150 0)

      -- A mangenta filled rectangle
      fillRectangle m (200 + n) 30 (250 + n) 130 (PixelRGB8 255 0 255)
  
      modifyIORef ref (+1)
      -- A dark green filled triangle
      fillTriangle m 50 200 250 300 70 350 (PixelRGB8 0 150 50)
      ls <- randIntList 100 (1, 100)
      histogramx m (10, 20) (20, 30) 100 ls (PixelRGB8 0 255 0)

      -- A blue pentagon
      drawPolygon m
          [ (340, 80)
          , (245, 149)
          , (281, 261)
          , (399, 261)
          , (435, 149)
          , (340, 80)
          ]
          (PixelRGB8 0 0 255)
    writePng pngName img


histogramx::(Pixel px, PrimMonad m) => MutableImage(PrimState m) px
    ->(Int, Int) -> (Int, Int) -> Int ->[Int] -> px -> m()
histogramx m (x1, y1) (x2, y2) h cs px = do
  let leftMargin = 4
  let width = 5 
  let distBar = 5
  let n = len cs
  let topMargin = 10
  drawRectangle m 0 0 20 30 px
  mapM_ (\(k, h) -> do
            let totalWidth = width + distBar
            fillRectangle m (0 + k*totalWidth) (1000 - (topMargin + h))  (width + k*totalWidth) (1000 - topMargin) px
        ) $ zip [0..(len cs - 1)] cs

negative :: Image PixelRGBA8 -> Image PixelRGBA8
negative = pixelMap $ \(PixelRGBA8 r g b a) -> PixelRGBA8 (255 - r) (255 - g) (255 - b) a
  
redToGreen :: Image PixelRGBA8 -> Image PixelRGBA8
-- redToGreen = pixelMap $ \(PixelRGBA8 r g b a) -> PixelRGBA8 r g b (41 < r && r < 80 && 45 < g && g < 80 && 52 < b  && b < 90 ? 0 $ a) 
redToGreen = pixelMap $ \(PixelRGBA8 r g b a) -> PixelRGBA8 r g b (r < 15 && g < 15 && b < 20 ? 0 $ a) 


-- blend :: Image PixelRGBA8 -> Image PixelRGBA8 -> Image PixelRGBA8 
-- blend =  pixelMap $ \(PixelRGBA8 r g b a) -> pixelMap $ \(PixelRGBA8 r' g' b' a') -> PixelRGBA8 (r + r') (g + g') (b + b') (a + a') 

removeBackground :: Image PixelRGBA8 -> Image PixelRGBA8
-- removeBackground = pixelMap $ \(PixelRGBA8 r g b a) -> PixelRGBA8 r g b (41 < r && r < 80 && 45 < g && g < 80 && 52 < b  && b < 90 ? 0 $ a) 
-- removeBackground = pixelMap $ \(PixelRGBA8 r g b a) -> PixelRGBA8 r g b (r < 15 && g < 15 && b < 15 ? 0 $ a) 
removeBackground = pixelMap $ \(PixelRGBA8 r g b a) -> 
                   -- PixelRGBA8 r g b ( 20 < r && r < 25 && 20 < g && g < 30 && 12 < b && b < 20 ? 0 $ a) 
                   PixelRGBA8 r g b (r < 15 && g < 15 && b < 15 ? 0 $ a)

transparent = PixelRGBA8 0 0 0 0
  
changeColor :: (Int, Int, Int) -> Image PixelRGBA8 -> Image PixelRGBA8
changeColor (x, y, z) = pixelMap $ \(PixelRGBA8 r g b a) -> PixelRGBA8 (r + fi x) (g + fi y) (b + fi z) a 

  
  
bx :: Int -> Int -> (Int, Int) -> (Int, Int) -> Bool
bx x y (w1, h1) (w2, h2) = b1 && b2
  where
    b1 = x < min w1 w2
    b2 = y < min h1 h2

mergeSameImage :: Float -> FilePath -> FilePath -> FilePath -> IO()
mergeSameImage alpha fp1 fp2 fp3 = do
  img1 <- readImage fp1
  img2 <- readImage fp2

  let rgbaImg1 = convertRGBA8 (either error id img1)
  let rgbaImg2 = convertRGBA8 (either error id img2)
  let (w1, h1) = (imageWidth rgbaImg1, imageHeight rgbaImg1)
  fw "(width1, height1)"
  print (w1, h1)
  
  fw "(width2, height2)"
  let (w2, h2) = (imageWidth rgbaImg2, imageHeight rgbaImg2)
  print (w2, h2)

  if (w1, h1) == (w2, h2) then do
    let newImg = generateImage
                  (\x y -> blend (pixelAt rgbaImg1 x y)
                                 (pixelAt rgbaImg2 x y)
                  )
                  w1
                  h1
    writePng fp3 newImg
  else
    print "(w1, h1) /= (w2, h2)"
 where
  beta = 0.2
  blend (PixelRGBA8 r0 g0 b0 a0) (PixelRGBA8 r1 g1 b1 a1) = 
    PixelRGBA8 (a0 /= 0 && a1 /= 0 ? div (r0 + r1) 2 $ a0 /= 0 ? r0 $ r1) 
               (a0 /= 0 && a1 /= 0 ? div (g0 + g1) 2 $ a0 /= 0 ? g0 $ g1) 
               (a0 /= 0 && a1 /= 0 ? div (b0 + b1) 2 $ a0 /= 0 ? b0 $ b1) 
               ( 
                  case a0 /= 0 && a1 /= 0 of
                        v | v -> round $ rf a0 * rf beta + rf a1 * (rf 1 - beta)
                          | otherwise -> a0 /= 0 ? a0 $ a1
               )

mergeSameImageX :: Float -> FilePath -> FilePath -> FilePath -> IO()
mergeSameImageX alpha fp1 fp2 fp3 = do
  img1 <- readImage fp1
  img2 <- readImage fp2

  let rgbaImg1 = imagePadRight 1 $ convertRGBA8 (either error id img1)
  let rgbaImg2 = imagePadDown 1 $ convertRGBA8 (either error id img2)
  let (w1, h1) = (imageWidth rgbaImg1, imageHeight rgbaImg1)
  fw "(width1, height1)"
  print (w1, h1)
  
  fw "(width2, height2)"
  let (w2, h2) = (imageWidth rgbaImg2, imageHeight rgbaImg2)
  print (w2, h2)

  if (w1, h1) == (w2, h2) then do
    let newImg = generateImage
                  (\x y -> blend (pixelAt rgbaImg1 x y)
                                 (pixelAt rgbaImg2 x y)
                  )
                  w1
                  h1
    writePng fp3 newImg
  else
    print "(w1, h1) /= (w2, h2)"
 where
  beta = 0.2
  -- blend (PixelRGBA8 r0 g0 b0 a0) (PixelRGBA8 r1 g1 b1 a1) = PixelRGBA8 (round $ sqrt $ rf (r0 * r0 + r1 * r1)) 0 0 (a0 + a1)
  blend (PixelRGBA8 r0 g0 b0 a0) (PixelRGBA8 r1 g1 b1 a1) = PixelRGBA8 (r0 * r0 + r1 * r1) 0 0 (a0 + a1)
  -- blend (PixelRGBA8 r0 g0 b0 a0) (PixelRGBA8 r1 g1 b1 a1) = PixelRGBA8 (r0 + r1) (g0 + g3) (b0 + b1) (a0 + a1)

composeImage :: Image PixelRGBA8 -> Image PixelRGBA8 -> Image PixelRGBA8
composeImage img0 img1 = newImg
  where
    (w0, h0) = (imageWidth img0, imageHeight img0)
    (w1, h1) = (imageWidth img1, imageHeight img1)
    newImg = generateImage (\x y -> blend (pixelAt img0 x y)
                                          (pixelAt img1 x y)
                           ) w0 h0
    blend (PixelRGBA8 r0 g0 b0 a0) (PixelRGBA8 r1 g1 b1 a1) = PixelRGBA8 (255 `min` (r0 + r1)) (255 `min` (g0 + g1)) (255 `min` (b0 + b1)) a0
  

{-|

  KEY: transpose image, transpose png, rotate image

  @
  saveImage tranImage "asteroid_s_5.png" "xx90.png"
  @
-}
tranImage :: Pixel a => Image a -> Image a 
tranImage img@Image {..} = generateImage (pixelAt img) imageHeight imageWidth

{-|

  KEY: rotate image 

  @
  saveImage rotateLeft90 "asteroid_s_5.png" "xx90.png"
  @
-}
saveImage :: (Image PixelRGBA8 -> Image PixelRGBA8) -> FilePath -> FilePath -> IO ()
saveImage f fp1 fp2 = do
  img1 <- readImage fp1
  let rgbaImg1 = convertRGBA8 (either error id img1)
  let img = f rgbaImg1
  savePngImage fp2 (ImageRGBA8 img)
  -- writePng fp2 img
  
  
{-|

  KEY: rotate left, rotate image 90 to the left, rotate 90 to the left

  @
  saveImage rotateLeft90 "asteroid_s_5.png" "xx90.png"
  @
-}
rotateLeft90 :: Pixel a => Image a -> Image a
rotateLeft90 img@Image {..} = generateImage gen imageHeight imageWidth
  where
    gen x y = pixelAt img (imageWidth - 1 - y) x

rotateLeft180 :: Pixel a => Image a -> Image a
rotateLeft180 img@Image{..} = generateImage gen imageWidth imageHeight
  where
    gen x y = let ix = imageWidth - 1; iy = imageHeight - 1 in pixelAt img (ix - x) (iy - y)
  
rotateRight180 :: Pixel a => Image a -> Image a
rotateRight180 img@Image{..} = generateImage gen imageWidth imageHeight
  where
    gen x y = let ix = imageWidth - 1; iy = imageHeight - 1 in flipXY (iy - y) (ix - x)
    flipXY = flip $ pixelAt img

red =   PixelRGBA8 255 0 0 255
green = PixelRGBA8 0 255 0 255
blue =  PixelRGBA8 0 0 255 255
black =  PixelRGBA8 0 0 0 255
  
circle :: (Int, Int) -> PixelRGBA8 -> Int -> Int -> Image PixelRGBA8
circle (x0, y0) pixel = generateImage gen
  where
    gen x y  = let x' = rf $ x - x0; y' = rf $ y - y0; r = sqrt $ x'^2 + y'^2 in abs (r - 100.0) < 1.0 ? pixel $ black

line :: (Int, Int) -> (Int, Int) -> Image PixelRGBA8 -> Int -> Int -> Image PixelRGBA8
line  (x0, y0) (x1, y1) img@Image{..} x y = generateImage gen imageWidth imageHeight
  where
    x0' = fi x0
    y0' = fi y0
    x1' = fi x1
    y1' = fi y1
    gen x y = let d = (sqrt . fi) $ (x1' - x0')^2 + (y1' - y0')^2
                  d1 = (sqrt . fi) $ (x - x0')^2 + (y - y0')^2
                  d2 = (sqrt . fi) $ (x - x1')^2 + (y - y1')^2
              in abs (d - d1 - d2) < 0.001 ? red $ pixelAt img x y
  
triangle :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Image PixelRGBA8 -> Image PixelRGBA8
triangle p0 p1 p2 img@Image{..} = img2
  where
    img0 = line p0 p1 img imageWidth imageHeight
    img1 = line p1 p2 img0 imageWidth imageHeight
    img2 = line p2 p0 img1 imageWidth imageHeight


{-
-- | Compute the intersection point of two lines, if any.
intersectLine :: Segment -> Segment -> Maybe Point
intersectLine l1@(Seg p0 p1) l2@(Seg q0 q1)
  | p # q == 0 = Nothing  -- parallel
  | otherwise  = Just $ Vec x y
  where
    p = p1 - p0
    q = q1 - q0
    u # v = dot u (rot90 v)

    x = (getX q * (p # p0) - getX p * (q # q0)) / (p # q)
    y = (getY q * (p # p0) - getY p * (q # q0)) / (p # q)
-}

  
{-|

  KEY: rotate right, rotate image 90 to the right, rotate 90 to the right 

  @
  saveImage rotateRight90 "asteroid_s_5.png" "xx90.png"
  @
-}
rotateRight90 :: Pixel a => Image a -> Image a
rotateRight90 img@Image {..} = generateImage gen imageHeight imageWidth
  where
    gen x y = pixelAt img y (imageHeight - 1 - x)

{-|

  KEY: image grayscale, gray scale

  @
  saveImage toGrayScale "bigboss.png" "bigboss_grayscale1.png"
  @
-}
toGrayScale :: Image PixelRGBA8 -> Image PixelRGBA8
toGrayScale img@Image {..} = generateImage (\x y -> gray $ pixelAt img x y) imageWidth imageHeight
  where
    gray :: PixelRGBA8 -> PixelRGBA8
    gray (PixelRGBA8 r g b a) = PixelRGBA8 gc gc gc a
      where
        gc = round $ rI * r' + gI * g' + bI * b'
        r' = fi r
        g' = fi g
        b' = fi b
        rI = 0.299 :: Float
        gI = 0.587 :: Float
        bI = 1.0 - rI - gI

{-|

  KEY: scale image, bilinear interpolation

  <http://localhost/pdf/bilinear_interpo_upsampling_cs_toronto_edu.pdf PDF>
  <https://hackage.haskell.org/package/JuicyPixels-extra-0.6.0/docs/src/Codec.Picture.Extra.html#scaleBilinear scaleBilinear>

  * The code is based on 'scaleBilinear'

-}
scaleImage :: Int -> Int -> Image PixelRGBA8 -> Image PixelRGBA8 
scaleImage width height img@Image {..} = generateImage (fun img) width height
  where
    -- fun img x y = p00
    fun :: Image PixelRGBA8 -> Int -> Int -> PixelRGBA8
    fun imgx x y = p00
                where
                  w = imageWidth
                  h = imageHeight
                  rx = fi w/ fi width :: Float
                  ry = fi h/fi height :: Float
                  xx = fi x * rx
                  yy = fi y * ry

                  -- dx = rx - (fi . floor) rx
                  -- dy = ry - (fi . floor) ry
                  (x', y') = (floor $ rf xx, floor $ rf yy)
                  dx = xx - fi x'
                  dy = yy - fi y'  
                  -- (x', y') = (floor $ fi x * rx, floor $ fi y * ry)
                  qxy  = sumPixel (mulPixel (1 - rf dx) pxy) (mulPixel (rf dx) px1y)
                  qxy1 = sumPixel (mulPixel (1 - rf dx) pxy1) (mulPixel (rf dx) px1y1)
                  p00  = sumPixel (mulPixel (1 - rf dy) qxy) (mulPixel (rf dy) qxy1)
                  pxy   = pixelAt imgx x' y'
                  px1y  = let xInx = (w - 1)  `min` (x' + 1) in pixelAt imgx xInx y'
                  pxy1  = let yInx = (y' + 1) `min` (h - 1)  in pixelAt imgx x' yInx
                  px1y1 = let xInx = (x' + 1) `min` (w - 1)
                              yInx = (y' + 1) `min` (h - 1)
                          in pixelAt imgx xInx yInx

    sumPixel (PixelRGBA8 r0 g0 b0 a0) (PixelRGBA8 r1 g1 b1 a1) = PixelRGBA8
                                                                      (m `min` (r0 + r1))
                                                                      (m `min` (g0 + g1))
                                                                      (m `min` (b0 + b1))
                                                                      (m `min` (a0 + a1))
                                                                 where
                                                                   m = maxBound :: (PixelBaseComponent PixelRGBA8)

    mulPixel :: Float -> PixelRGBA8 -> PixelRGBA8
    mulPixel x (PixelRGBA8 r0 g0 b0 a0) = PixelRGBA8 (floor $ rf r0 * x) (floor $ rf g0 * x) (floor $ rf b0 * x) (floor $ rf a0 * x)

{-|
  KEY: flip image vertically

  @
  saveImage flipImageV "asteroid_s_5.png" "xx90.png"
  @
-}
flipImageV :: Pixel a => Image a -> Image a
flipImageV img@Image {..} = generateImage gen imageWidth imageHeight
  where
    gen x y = pixelAt img x (imageHeight - 1 - y)

{-|
  KEY: flip image horizontally 

  @
  saveImage flipImageH "asteroid_s_5.png" "xx90.png"
  @
-}
flipImageH :: Pixel a => Image a -> Image a
flipImageH img@Image {..} = generateImage gen imageWidth imageHeight
  where
    gen x y = pixelAt img (imageWidth - 1 - x) y 

imagePadRight :: Int -> Image PixelRGBA8  -> Image PixelRGBA8
imagePadRight n img@Image {..} = generateImage gen (imageWidth + n) imageHeight
  where
    gen x y = x < imageWidth ? pixelAt img x y $ PixelRGBA8 0 0 0 0
  
imagePadDown :: Int -> Image PixelRGBA8  -> Image PixelRGBA8
imagePadDown n img@Image {..} = generateImage gen imageWidth (imageHeight + n)
  where
    gen x y = y < imageHeight ? pixelAt img x y $ PixelRGBA8 0 0 0 0
  
-- Codec.Picture generateImage :: forall px . Pixel px => (Int -> Int -> px) -> Int -> Int -> Image px
-- Codec.Picture.Types generateImage :: forall px . Pixel px => (Int -> Int -> px) -> Int -> Int -> Image px
-- Graphics.Rasterific.MeshPatch generateImageMesh :: Int -> Int -> Point -> Image px -> MeshPatch (ImageMesh px)

{-|

  KEY: blend images, compose images 

  @
  let alpha = 0.5
  blendImage alpha "asteroid_s_5.png"   "asteroid_s_6.png" "ex4_blend.png"
  @
-}
blendImage :: Float -> FilePath -> FilePath -> FilePath -> IO()
blendImage alpha fp1 fp2 fp3 = do
  img1 <- readImage fp1
  img2 <- readImage fp2

  let rgbaImg1 = convertRGBA8 (either error id img1)
  let rgbaImg2 = convertRGBA8 (either error id img2)
  -- Get image dimension
  let (w1, h1) = (imageWidth rgbaImg1, imageHeight rgbaImg1)
  fw "(width1, height1)"
  print (w1, h1)
  
  fw "(width2, height2)"
  let (w2, h2) = (imageWidth rgbaImg2, imageHeight rgbaImg2)
  print (w2, h2)
  -- Genrate new image by blending pixel-by-pixel
  let newImg1 = generateImage
                 (\x y -> blendPixel alpha ( let ix = x - w2 in ix >= 0  && y < min h1 h2 ? pixelAt rgbaImg1 ix y $ transparent)
                 -- (\x y -> blendPixel alpha (transparent)
                                           -- ( y < min h1 h2 ? pixelAt rgbaImg1 x y $ transparent))
                                           transparent)
                 (w1 + w2)
                 (max h1 h2)
  let newImg2 = generateImage
                 (\x y -> blendPixel alpha ( x < w2 && y < h2 ? pixelAt rgbaImg2 x y $ transparent)
                 -- (\x y -> blendPixel alpha (transparent)
                                           -- ( y < min h1 h2 ? pixelAt rgbaImg1 x y $ transparent))
                                           transparent)
                 (w1 + w2)
                 (max h1 h2)
  -- Write the resulting image to output path
  writePng fp3 newImg1
  writePng "ex4_blend2.png" newImg2
  
{-|

  KEY: blend images, compose images 

  @
  0 <= shiftLeft  <= width of image1
  0 <= shiftRight <= width of image2

  let alpha = 0.5
  let shiftLeft = 100  // img1.png
  let shiftRight = 200 // img2.png
  blendImageH alpha (shiftLeft, shiftRight) "img1.png"   "img2.png" "output.png"
  @
-}
blendImageH :: Float -> (Int, Int) -> FilePath -> FilePath -> FilePath -> IO ()
blendImageH alpha (shiftLeft, shiftRight) fp1 fp2 blendfp = do
  img1 <- readImage fp1
  img2 <- readImage fp2

  let rgbaImg1 = convertRGBA8 (either error id img1)
  let rgbaImg2 = convertRGBA8 (either error id img2)
  -- Get image dimension
  let (w1, h1) = (imageWidth rgbaImg1, imageHeight rgbaImg1)
  fw "(width1, height1)"
  print (w1, h1)
  
  fw "(width2, height2)"
  let (w2, h2) = (imageWidth rgbaImg2, imageHeight rgbaImg2)
  print (w2, h2)
  -- Genrate new image by blending pixel-by-pixel

  -- let shiftLeft = (w1 - 20)
  let newImg1 = generateImage 
                  -- (\x y -> blendPixel alpha (y < h1 && x < w1 ? pixelAt rgbaImg1 x y $ transparent) transparent)
                  (\x y -> blendPixel alpha (let ix = x - shiftRight in ix >= 0 && ix < w1 && y < h1 ? pixelAt rgbaImg1 ix y $ transparent) transparent)
                  (w1 + w2)
                  (max h1 h2)

  let newImg2 = generateImage
                  (\x y -> blendPixel alpha (let ix = x - (w1 - shiftLeft) in ix >= 0 && x < w1 + w2 - shiftLeft && y < h2 ? pixelAt rgbaImg2 ix y $ transparent) transparent)
                  (w1 + w2)
                  (max h1 h2)

  -- Write the resulting image to output path
  let outfp1 = dropExt fp1 ++ "_" ++ ".png"
  let outfp2 = dropExt fp2 ++ "_" ++ ".png"
  writePng outfp1 newImg1
  writePng outfp2 newImg2
  mergeSameImage alpha outfp1 outfp2 blendfp
  
{-|

  KEY: blend images, compose images 

  @
  0 <= shiftLeft  <= width of image1
  0 <= shiftRight <= width of image2

  let alpha = 0.5
  blendImageV alpha "asteroid_s_5.png"   "asteroid_s_6.png" "ex4_blend.png"
  @
-}
blendImageV :: Float -> FilePath -> FilePath -> FilePath -> IO ()
blendImageV alpha fp1 fp2 blendfp = do
  img1 <- readImage fp1
  img2 <- readImage fp2

  let rgbaImg1 = convertRGBA8 (either error id img1)
  let rgbaImg2 = convertRGBA8 (either error id img2)
  -- Get image dimension
  let (w1, h1) = (imageWidth rgbaImg1, imageHeight rgbaImg1)
  fw "(width1, height1)"
  print (w1, h1)
  
  fw "(width2, height2)"
  let (w2, h2) = (imageWidth rgbaImg2, imageHeight rgbaImg2)
  print (w2, h2)
  -- Genrate new image by blending pixel-by-pixel

  let newImg1 = generateImage 
                  (\x y -> blendPixel alpha (x < w1 && y < h1 ? pixelAt rgbaImg1 x y $ transparent) transparent)
                  (max w1 w2)
                  (h1 + h2)

  let newImg2 = generateImage
                  (\x y -> blendPixel alpha (let iy = y - h1 in iy >= 0 && x < w2 ? pixelAt rgbaImg2 x iy $ transparent) transparent)
                  (max w1 w2)
                  (h1 + h2)

  -- Write the resulting image to output path
  let outfp1 = dropExt fp1 ++ "_" ++ ".png"
  let outfp2 = dropExt fp2 ++ "_" ++ ".png"
  writePng outfp1 newImg1
  writePng outfp2 newImg2
  mergeSameImage alpha outfp1 outfp2 blendfp 

dxdy :: Image PixelRGBA8 -> Int -> Int -> (Float, Float)
dxdy img@Image{..} x y = (dx, dy)
  where
    w = imageWidth
    h = imageHeight
    xl = pixelIntent $ pixelAt img ((x - 1) `max` 0) y
    xr = pixelIntent $ pixelAt img ((x + 1) `min` (w - 1)) y
    dx = (xr - xl) * 0.5
    yl = pixelIntent $ pixelAt img x ((y - 1) `max` 0)
    yr = pixelIntent $ pixelAt img x ((y + 1) `min` (h - 1))
    dy = (yr - yl) * 0.5
  

-- 5x5 Gaussian kernel for blurring
gaussianKernel :: [[Float]]
gaussianKernel =
  [ [2/159, 4/159,  5/159,  4/159, 2/159]
  , [4/159, 9/159, 12/159,  9/159, 4/159]
  , [5/159, 12/159, 15/159, 12/159, 5/159]
  , [4/159, 9/159, 12/159,  9/159, 4/159]
  , [2/159, 4/159,  5/159,  4/159, 2/159]
  ]
  
clamX x dx w = 0 `max` (inx `min` (x + dx)) where inx = w - 1
clamY y dy h = 0 `max` (inx `min` (y + dy)) where inx = h - 1

fstColor :: PixelRGBA8 -> Word8
fstColor = inxColor 0
  
inxColor :: Int -> PixelRGBA8 -> Word8
inxColor n (PixelRGBA8 r g b a) | n == 0 = r
                                | n == 1 = g
                                | n == 2 = b
                                | otherwise = a

pixZero   = PixelRGBA8 0 0 0 255
                                
doubleThreshold :: (Word8, Word8) -> Image PixelRGBA8 -> Image PixelRGBA8
doubleThreshold (low, hight) img@Image{..} = pixelMap classify img
  where
    classify px@(PixelRGBA8 r g b a) | fstColor px >= hight = PixelRGBA8 255 255 255 a
                                     | fstColor px >= low   = PixelRGBA8 128 128 128 a
                                     | otherwise = PixelRGBA8 0 0 0 a
                                
edgeTrack :: Image PixelRGBA8 -> Image PixelRGBA8
edgeTrack img@Image{..} = generateImage track w h
  where
    w = imageWidth
    h = imageHeight
    neighbour x y = [pixelAt img (clamX x dx w) (clamY y dy h) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0)]
    track x y
      | let r = fstColor $ pixelAt img x y in r == 128 && elem 255 (map fstColor $ neighbour x y) = PixelRGBA8 255 255 255 (inxColor 3 $ pixelAt img x y)
      | let r = fstColor $ pixelAt img x y in r == 128 = PixelRGBA8 0 0 0 255
      | otherwise = pixelAt img x y
  
{-|
   KEY: canny edge detection, image edge, detect edge
   <http://localhost/pdf/bilinear_interpo_upsampling_cs_toronto_edu.pdf Bilinear_Interpolation>
-}
cannyEdge:: FilePath -> FilePath -> IO ()
cannyEdge fp1 fp2 = do
  img1 <- readImage fp1
  let rgbaImg1 = convertRGBA8 (either error id img1)
  -- Get image dimension
  let (w1, h1) = (imageWidth rgbaImg1, imageHeight rgbaImg1)
  fw "(w1, h1)"
  print (w1, h1)
  -- Genrate new image by blending pixel-by-pixel
  let mb = maxBound :: (PixelBaseComponent PixelRGBA8)
  let pz = PixelRGBA8 0 0 0 255
  
  let newImg1 = generateImage 
                  (\x y ->
                           let (dx, dy) = dxdy rgbaImg1 x y
                               graMag = sqrt $ dx^2 + dy^2
                                          -- (x - 1, x + 1)
                               pix0 = let (dx0, dy0) = dxdy rgbaImg1 (clamX x (-1) w1) y
                                          (dx1, dy1) = dxdy rgbaImg1 (clamX x 1    w1) y
                               
                                          graMag0 = sqrt $ dx0^2 + dy0^2
                                          graMag1 = sqrt $ dx1^2 + dy1^2
                                      in  graMag > max graMag0 graMag1 ? pixelAt rgbaImg1 x y $ pz
                               
                               pix1 = let (dx0, dy0) = dxdy rgbaImg1 x $ clamY y (-1) h1
                                          (dx1, dy1) = dxdy rgbaImg1 x $ clamY y 1    h1
                                          graMag0 = sqrt $ dx0^2 + dy0^2
                                          graMag1 = sqrt $ dx1^2 + dy1^2
                                      in  graMag > max graMag0 graMag1 ? pixelAt rgbaImg1 x y $ pz
                  
                               pix2 = let (dx0, dy0) = dxdy rgbaImg1 (clamX x (-1) w1) (clamY y 1    h1)
                                          (dx1, dy1) = dxdy rgbaImg1 (clamX x 1    w1) (clamY y (-1) h1)
                                          graMag0 = sqrt $ dx0^2 + dy0^2
                                          graMag1 = sqrt $ dx1^2 + dy1^2
                                      in  graMag > max graMag0 graMag1 ? pixelAt rgbaImg1 x y $ pz
                               pix3 = let (dx0, dy0) = dxdy rgbaImg1 (clamX x 1    w1) (clamY y 1    h1)
                                          (dx1, dy1) = dxdy rgbaImg1 (clamX x (-1) w1) (clamY y (-1) h1)
                                          graMag0 = sqrt $ dx0^2 + dy0^2
                                          graMag1 = sqrt $ dx1^2 + dy1^2
                                      in  graMag > max graMag0 graMag1 ? pixelAt rgbaImg1 x y $ pz
                                    
                               -- beta = atan (rf dy/rf dx)
                               beta = atan2 (rf dy) (rf dx)
                           in  case abs beta of
                                     e | e <= pi/8 || e >= 7/8 * pi                -> pix0
                                       | e >= 3/8 * pi && e < 5/8 * pi             -> pix1
                                       | e >= pi/8 && e < 3/8 * pi                 -> pix2
                                       | e >= 5/8 * pi && e < 7/8 * pi             -> pix3
                                       | otherwise                                 -> pz
                  )
                  w1
                  h1
   
  -- let newImg2 = edgeTrack $ doubleThreshold (50, 150)  newImg1
  -- let newImg2 = doubleThreshold (50, 200)  newImg1  
  {-
  let newImg1 = generateImage 
                  (\x y -> let p0 = pixelAt rgbaImg1 x y; p1 = pixelAt rgbaImg1 (x + 1) y  in gradientMag p0 p1) 
                  (w1 - 1) 
                  h1
  -}
  {-
  let newImg1 = generateImage 
                  -- (\x y -> intensityPixel $ pixelAt rgbaImg1 x y) 
                  (\x y -> let p0 = pixelAt rgbaImg1_rot x y; p1 = pixelAt rgbaImg1_rot (x + 1) y  in gradientMag p0 p1) 
                  (w1' - 1)
                  h1'
  let rgbaImg1_rot_left = rotateLeft90 newImg1
  -}
  
  -- Write the resulting image to output path
  -- writePng fp2 newImg1
  writePng fp2 newImg1
  -- writePng fp2 rgbaImg1_rot_left
  
{-|
blendPixel :: Float -> PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8
blendPixel alpha (PixelRGBA8 r0 g0 b0 a0) (PixelRGBA8 r1 g1 b1 a1) =
  let blend c0 c1 = round $ fi r0 * alpha + fi r1 * (1 - alpha)
  in PixelRGBA8 (blend r0 r1) (blend g0 g1) (blend b0 b1) (a0 + a1)
  -- in PixelRGBA8 (blend r0 r1) (blend g0 g1) (blend b0 b1) (a0 == 0 || a1 == 0 ? 0 $ blend a0 a1)
-}
blendPixel :: Float -> PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8
blendPixel alpha (PixelRGBA8 r0 g0 b0 a0) (PixelRGBA8 r1 g1 b1 a1) = PixelRGBA8 r0 g0 b0 a0


{-|
  KEY: pixel intensity

  @
    rI = 0.299
    gI = 0.587
    bI = 1 - rI - gI

    intensityPixel :: PixelRGBA8 -> PixelRGBA8
    intensityPixel (PixelRGBA8 r0 g0 b0 a0) = PixelRGBA8 (round $ rI * r0' + gI * g0' + bI * b0') 0 0 a0 
  @
-}
intensityPixel :: PixelRGBA8 -> PixelRGBA8
intensityPixel (PixelRGBA8 r0 g0 b0 a0) = PixelRGBA8 (round $ rI * r0' + gI * g0' + bI * b0') 0 0 a0 
  where
    r0' = rf r0
    g0' = rf g0
    b0' = rf b0
    rI = 0.299
    gI = 0.587
    bI = 1 - rI - gI

{-
{-|
  KEY: pixel intensity, color intensity
-}
pixelIntent :: PixelRGBA8 -> Float
pixelIntent (PixelRGBA8 r0 g0 b0 a0) = rI * r0' + gI * g0' + bI * b0'
  where
    r0' = rf r0
    g0' = rf g0
    b0' = rf b0
    rI = 0.299
    gI = 0.587
    bI = 1 - rI - gI  
-}
{-|
  KEY: gradient magnitude
-}
gradientMag :: PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8 
gradientMag (PixelRGBA8 r0 g0 b0 a0) (PixelRGBA8 r1 g1 b1 a1) = PixelRGBA8 diff 0 0 a0 
  where
    r0' = rf r0
    g0' = rf g0
    b0' = rf b0
    r1' = rf r1
    g1' = rf g1
    b1' = rf b1
    rI = 0.299
    gI = 0.587
    bI = 1 - rI - gI 
    inten0 = sqrt $ rI * r0' + gI * g0' + bI * b0'
    inten1 = sqrt $ rI * r1' + gI * g1' + bI * b1'
    m = maxBound :: (PixelBaseComponent PixelRGBA8)
    diff = round $ inten1 - inten0


  
gradient :: PixelRGBA8 -> PixelRGBA8 -> Float
gradient (PixelRGBA8 r0 g0 b0 a0) (PixelRGBA8 r1 g1 b1 a1) = diff
  where
    r0' = rf r0
    g0' = rf g0
    b0' = rf b0
    r1' = rf r1
    g1' = rf g1
    b1' = rf b1
    rI = 0.299
    gI = 0.587
    bI = 1 - rI - gI 
    inten0 = sqrt $ rI * r0' + gI * g0' + bI * b0'
    inten1 = sqrt $ rI * r1' + gI * g1' + bI * b1'
    m = maxBound :: (PixelBaseComponent PixelRGBA8)
    diff = inten1 - inten0



addp ::
  forall a.
  ( Pixel a,
    Bounded (PixelBaseComponent a),
    Integral (PixelBaseComponent a)
  ) =>
  a ->
  a ->
  a
addp = mixWith (const f)
  where
    f x y =
      fromIntegral $
        (maxBound :: PixelBaseComponent a) `min` (fromIntegral x + fromIntegral y)

-- Main function to read the image, convert, and save it
mainX0 :: IO ()
mainX0 = do
    -- Load the input image
    eitherImage <- readImage "bigboss.png"
    
    case eitherImage of
      Left err -> putStrLn ("Error loading image: " ++ err)
      Right dynamicImage -> do
        -- Convert the image to RGBA8 format
        let rgbaImage = convertRGBA8 dynamicImage
        -- Convert to grayscale
        let grayscaleImage = toGrayScale rgbaImage
        -- Save the output image
        savePngImage "output_image.png" (ImageRGBA8 grayscaleImage)
        putStrLn "Image converted to grayscale successfully!"

-- Sobel kernels for x and y gradient
sobelX :: [[Float]]
sobelX = [[-1, 0, 1], [-2, 0, 2], [-1, 0, 1]]

sobelY :: [[Float]]
sobelY = [[ 1, 2, 1], [ 0, 0, 0], [-1, -2, -1]]

-- Calculate the gradient magnitude and direction
applySobel :: Image Pixel8 -> (Image Float, Image Float)
applySobel img = (gradMag, gradDir)
  where
    gradMag = generateImage (\x y -> sqrt ((conv sobelX x y) ** 2 + (conv sobelY x y) ** 2)) w h
    gradDir = generateImage (\x y -> atan2 (conv sobelY x y) (conv sobelX x y)) w h
    w = imageWidth img
    h = imageHeight img
    conv kernel x y = convolve img kernel x y
  
-- Apply convolution with a kernel (e.g., Gaussian, Sobel)
convolve :: Image Pixel8 -> [[Float]] -> Int -> Int -> Float
convolve img kernel x y = sum $ zipWith (*) (concat kernel) pixelValues
  where
    pixelValues = [fromIntegral (pixelAt img (x + dx - 2) (y + dy - 2)) | dx <- [0..4], dy <- [0..4]]

convolveX :: Image PixelRGBA8 -> [[Float]] -> Int -> Int -> Float
convolveX img@Image{..} kernel x y = sum $ zipWith (*) (pixelValue x y) (concat kernel)
  where
    w = imageWidth
    h = imageHeight
    clamX x dx = 0 `max` ((w - 1) `min` (x + dx))
    clamY y dy = 0 `max` ((h - 1) `min` (y + dy))
    pixelValue x y = [let PixelRGBA8 r g b a = pixelAt img (clamX x dx) (clamY y dy) in rf r | dx <- [-2..2], dy <- [-2..2]] :: [Float]
  
-- Apply Gaussian blur
applyGaussianBlur :: Image Pixel8 -> Image Pixel8
applyGaussianBlur img = generateImage blur (imageWidth img) (imageHeight img)
  where
    blur x y = floor $ convolve img gaussianKernel x y

blur :: Image PixelRGBA8 -> Image PixelRGBA8
blur img@Image{..} = generateImage (\x y -> let (r, a) = convolveX img gaussianKernel x y in PixelRGBA8 r r r a) w h
  where
    w = imageWidth
    h = imageHeight
    -- 5x5 Gaussian kernel for blurring
    gaussianKernel :: [[Float]]
    gaussianKernel =
      [ [2/159, 4/159,  5/159,  4/159, 2/159]
      , [4/159, 9/159, 12/159,  9/159, 4/159]
      , [5/159, 12/159, 15/159, 12/159, 5/159]
      , [4/159, 9/159, 12/159,  9/159, 4/159]
      , [2/159, 4/159,  5/159,  4/159, 2/159]
      ]
  
    convolveX :: Image PixelRGBA8 -> [[Float]] -> Int -> Int -> (Word8, Word8)
    convolveX img@Image{..} kernel x y = let ls = zipWith (\(r, a) b -> (r * b, a)) (pixelValue x y) (concat kernel) in (fi $ floor $ sum $ map fst ls, snd $ head ls) ::(Word8, Word8)
      where
        w = imageWidth
        h = imageHeight
        clamX x dx = 0 `max` ((w - 1) `min` (x + dx))
        clamY y dy = 0 `max` ((h - 1) `min` (y + dy))
        pixelValue x y = [let PixelRGBA8 r g b a = pixelAt img (clamX x dx) (clamY y dy) in (rf r, a) | dx <- [-2..2], dy <- [-2..2]] :: [(Float, Word8)]      
  

mainX6 :: IO ()
mainX6 = do
    argList <- getArgs
    let fname = head argList
    nls <- readFileList fname >>= \cx -> return $ map (\x -> read x ::Int) cx
    let w = 2000
        h = 1000

    img <- withMutableImageX w h (PixelRGB8 10 10 10) $ \m -> do
        -- A green diagonal line
        drawLine m 0 0 (w - 1) (h - 1) (PixelRGB8 0 255 0)

        -- A blue square at a 45-degree angle
        -- drawPolygon m [(50, 50), (75, 75), (100, 50), (75, 25), (50, 50)] (PixelRGB8 0 0 255)

        -- An orange bounding rectangle
        -- drawRectangle m 0 0 (w - 1) (h - 1) (PixelRGB8 255 150 0)

        -- A mangenta filled rectangle
        -- mapM_ (\x -> do
        --           fillRectangle m (100 + x) 30 (110 + x) 130 (PixelRGB8 255 0 255)
        --      ) [10, 30..100]
        ls <- randIntList 100 (1, 100)
        histogramx m (10, 20) (20, 30) 100 ls (PixelRGB8 0 255 0)
        -- A dark green filled triangle
        -- fillTriangle m 50 200 250 300 70 350 (PixelRGB8 0 150 50)

        -- A blue pentagon
        drawPolygon m
            [ (340, 80)
            , (245, 149)
            , (281, 261)
            , (399, 261)
            , (435, 149)
            , (340, 80)
            ]
            (PixelRGB8 0 0 255)

    writePng "example.png" img
  
    ref <- newIORef 0
    mapM_ (\_ -> do
              geneImg ref w h
          ) [1..10]
  
    -- dynamicImage <- readImage "boss_tran_0.png" 
    -- dynamicImage <- readImage "boss_tran_1.png" 
    -- dynamicImage <- readImage "boss_tran_2.png" 
    -- dynamicImage <- readImage "asteroid0_x.png"
    dynamicImage <- readImage "earth0.png" 
    -- dynamicImage <- readImage "ex4.png" 
    let image = convertRGBA8 <$> dynamicImage
    -- let modified = negative <$> image
    mapM_ (\x -> do
      ls <- randomIntList 10 (1, 200) 
      let t = (fi $ head ls, fi $ ls !! 1, fi $ ls !! 2)
      let modified = changeColor t <$> image
      let imgName = "earth_x" ++ show x ++ ".png"
      case modified of 
              Left err -> print err
              -- Right image -> saveJpgImage 100 "ex4.png" $ ImageRGBA8 image
              Right image -> savePngImage imgName $ ImageRGBA8 image
          ) [0..10]
    print "ok"

  
mainX3 :: IO ()
mainX3 = do
    -- dynamicImage <- readImage "boss_tran_0.png" 
    -- dynamicImage <- readImage "boss_tran_1.png" 
    -- dynamicImage <- readImage "boss_tran_2.png" 
    -- dynamicImage <- readImage "asteroid0_x.png"
    dynamicImage <- readImage "xx90.png" 
    -- dynamicImage <- readImage "ex4.png" 
    let image = convertRGBA8 <$> dynamicImage
    -- let modified = negative <$> image
    mapM_ (\x -> do
      ls <- randomIntList 10 (1, 200) 
      let t = (fi $ head ls, fi $ ls !! 1, fi $ ls !! 2)
      let modified = changeColor t <$> image
      let imgName = "xx90_x" ++ show x ++ ".png"
      case modified of 
              Left err -> print err
              -- Right image -> saveJpgImage 100 "ex4.png" $ ImageRGBA8 image
              Right image -> savePngImage imgName $ ImageRGBA8 image
          ) [0..10]
    print "ok"    

mainX5 :: IO ()
mainX5 = do
    -- dynamicImage <- readImage "boss_tran_0.png" 
    -- dynamicImage <- readImage "boss_tran_1.png" 
    -- dynamicImage <- readImage "boss_tran_2.png" 
    -- dynamicImage <- readImage "asteroid0_x.png"
    dynamicImage <- readImage "earth0.png" 
    -- dynamicImage <- readImage "ex4.png" 
    let image = convertRGBA8 <$> dynamicImage
    -- let modified = negative <$> image
    mapM_ (\x -> do
      ls <- randomIntList 10 (100, 255) 
      let t = (fi $ head ls, fi $ ls !! 1, fi $ ls !! 2)
      let modified = changeColor t <$> image
      let imgName = "earth_x" ++ show x ++ ".png"
      case modified of 
              Left err -> print err
              -- Right image -> saveJpgImage 100 "ex4.png" $ ImageRGBA8 image
              Right image -> savePngImage imgName $ ImageRGBA8 image
          ) [0..10]
    print "ok"
  
mainX8 :: IO ()
mainX8 = do
  let alpha = 0.3
  -- blendImage alpha "asteroid_s_5.png"   "asteroid_s_6.png" "ex4_blend.png"
  -- mergeSameImage 0.1 "ex4_blend2.png"   "ex4_blend3.png" "ex4_blend4.png"
  -- blendImageV    0.1 "asteroid_s_5.png" "asteroid_s_6.png" "ex4_s_5v.png" "ex4_s_6v.png"
  -- blendImageV    0.1 "ex4_x0.png" "ex4_x1.png" "ex4_vv.png"
  -- blendImageH    0.1 "asteroid_s_5.png" "asteroid_s_6.png" "ex4_s_5h.png" "ex4_s_6h.png"
  -- blendImageH    0.1 (200, 0) "ex4_x0.png" "ex4_x1.png" "ex4_hh.png"
  blendImageH 0.1 (200, 0) "asteroid_s_5.png" "asteroid_s_6.png" "ex4_.png"
  img <- readImage "asteroid_s_5.png" 
  let imgx = convertRGBA8 (either error id img)
  print $ imageData imgx
  let rotImg = tranImage imgx 
  let leftImg = rotateLeft90 imgx 
  let rightImg = rotateRight90 imgx 
  let imgV = flipImageV imgx 
  let imgH = flipImageH imgx 
  writePng "asteroid_s_5_rot.png" rotImg 
  writePng "asteroid_s_5_left.png" leftImg 
  writePng "asteroid_s_5_right.png" rightImg 
  writePng "asteroid_s_5_v.png" imgV 
  writePng "asteroid_s_5_h.png" imgH 
  saveImage tranImage "asteroid_s_5.png" "xx.png" 
  saveImage rotateRight90 "asteroid_s_5.png" "xx90.png"
  mergeSameImageX 0.1 "intensity.png" "intensity_y.png" "int.png"
  saveImage (imagePadRight 100) "ex4_x.png" "ex4_xx.png"
  saveImage (imagePadDown 100)  "ex4_x.png"  "ex4_xxx.png"
  (w1, h1) <- imageSize "ex4.png"
  -- saveImage (scaleBilinear (div w1 10) (div h1 10)) "ex4.png" "ex4_scale1.png"
  -- saveImage (scaleBilinear (w1 * 3) (h1 * 3)) "ex4.png" "ex4_scale1.png"
  let px = addp (PixelRGBA8 1 2 3 4) (PixelRGBA8 10 20 30 40) 
  print px
  -- saveImage (scaleImage (div w1 4) (div h1 4)) "ex4.png" "ex4_bilinear.png"
  -- saveImage (scaleImage (div w1 10) (div h1 10)) "ex4.png" "ex4_bilinear.png"

  print "cannyEdge"
  -- cannyEdge "small.png" "small_canny.png"
  -- cannyEdge "ex4.png" "ex4_canny.png"
  cannyEdge "output_image.png" "output_image_canny.png"
  -- cannyEdge "bigboss_grayscale.png" "bigboss_canny_y.png"
  saveImage toGrayScale "earth_0.png" "earth_0_x.png"
  saveImage blur "bigboss_grayscale1.png" "bigboss_grayscale1_blur.png"
  -- cannyEdge "bigboss_grayscale1_blur.png" "canny_blur_x.png"
  -- cannyEdge "xx90_x8.png" "xx90_x8_x.png"
  -- saveImage (scaleImage (round $ rf $ fi w1 * rf 0.2) (round $ rf $ fi h1 * rf 0.2)) "/tmp/boss_tran_0.png" "/tmp/boss_tran_x.png"
{-


withImage -> Int -> Int -> (Int -> Int -> m pixel) -> m pixel


-}
  

darkenImage :: Image PixelRGBA8 -> IO (Image PixelRGBA8)
darkenImage img@Image{..} = do
  let w = imageWidth
  let h = imageHeight
  muImg <- newMutableImage w h
  forM_ [0..w - 1] $ \x ->
    forM_ [0..h - 1] $ \y -> do
      let PixelRGBA8 r g b a = pixelAt img x y
      writePixel muImg x y $ PixelRGBA8 (div r 2) (div g 2) (div b 2) a
  unsafeFreezeImage muImg

withMutableImageX :: (Pixel px, PrimMonad m) => Int -> Int -> px -> (MutableImage (PrimState m) px -> m ()) -> m (Image px)
withMutableImageX w h px f = do
  img <- createMutableImage w h px
  f img
  unsafeFreezeImage img

{-|

   SEE: 'drawLine' from 'Codec.Picture.Drawing'
   <https://hackage.haskell.org/package/juicy-draw-0.2.0.0/docs/src/Codec.Picture.Drawing.html#withMutableImage  drawLine>
-}
drawLineX :: (Pixel px, PrimMonad m) => (Int, Int)  -> (Int, Int) -> px -> MutableImage (PrimState m) px -> m ()
drawLineX p0@(x0, y0) p1@(x1, y1) color img = do
                          if abs dx > abs dy then do
                            forM_ [min x0 x1 .. max x0 x1] $ \x ->
                               writePixel img x (truncate $ rf (x - x0) * m + rf y0) color
                          else do
                            forM_ [min y0 y1 .. max y0 y1] $ \y ->
                               writePixel img (truncate $ rf (y - y0) * (dx / dy) + rf x0) y color
                 where
                   dx = rf $ x1 - x0
                   dy = rf $ y1 - y0
                   m = rf dy / rf dx
                                        
drawLineX2 :: (Pixel px, PrimMonad m) => (Int, Int)  -> (Int, Int) -> px -> MutableImage (PrimState m) px -> m ()
drawLineX2 p0@(x0, y0) p1@(x1, y1) color img = do
                            forM_ ls $ \t -> do
                              let (x, y) = tr $ (1.0 - t) *: (rf x0, rf y0) +: (t *: (rf x1, rf y1))
                              writePixel img x y color
                              {--  
                            forM_ lt $ \t -> do
                              let (x, y) = tr $ (1.0 - t) *: (rf x0, rf y0) +: (t *: (rf x1, rf y1))
                              writePixel img x y color
                              --}  where
    dx = abs $ x1 - x0
    dy = abs $ y1 - y0
    dt = max dx dy
    dt' = div dt 3
    delta = 1.0 / rf dt
    delta' = 1.0 / rf dt'
    ls = map (\x -> rf x * delta) [0..dt]
    lt = map (\x -> rf x * delta') [0..dt']
    (*:) x (a, b) = (x*a, x*b)
    (+:) (a, b) (c, d) = (a + c, b + d)
    tr (a, b) = (truncate a, truncate b)


type MImage m px = MutableImage (PrimState m) px

-- | Create an image given a function to apply to an empty mutable image
withMutableImage
    :: (Pixel px, PrimMonad m)
    => Int                      -- ^ image width
    -> Int                      -- ^ image height
    -> px                       -- ^ background colour
    -> (MImage m px -> m ())    -- ^ function to apply to mutable image
    -> m (Image px)             -- ^ action
withMutableImage w h px f = createMutableImage w h px >>= \m -> f m >> unsafeFreezeImage m

-- | Plot a pixel at the given point in the given colour
plot
    :: (Pixel px, PrimMonad m)
    => MImage m px  -- ^ mutable image
    -> Int          -- ^ x-coordinate of point
    -> Int          -- ^ y-coordinate of point
    -> px           -- ^ colour
    -> m ()         -- ^ action
plot = writePixel

-- | Draw an antialiased line from first point to second point in given colour
drawAntialiasedLine
    :: forall px m . (Pixel px, PrimMonad m)
    => MImage m px      -- ^ mutable image
    -> Int              -- ^ x-coordinate of first point
    -> Int              -- ^ y-coordinate of first point
    -> Int              -- ^ x-coordinate of second point
    -> Int              -- ^ y-coordinate of second point
    -> (Double -> px)   -- ^ colour generator function
    -> m ()             -- ^ action
drawAntialiasedLine m p1x p1y p2x p2y colour = do
    let steep = abs (p2y - p1y) > abs (p2x - p1x)
        ((p3x, p4x), (p3y, p4y)) = swapIf steep ((p1x, p2x), (p1y, p2y))
        ((ax, ay), (bx, by)) = swapIf (p3x > p4x) ((p3x, p3y), (p4x, p4y))
        dx = bx - ax
        dy = by - ay
        gradient = if dx == 0 then 1.0 else fromIntegral dy / fromIntegral dx

    -- handle first endpoint
    let xpxl1 = ax -- round (fromIntegral ax)
        yend1 = fromIntegral ay + gradient * fromIntegral (xpxl1 - ax)
        xgap1 = rfpart (fromIntegral ax + 0.5)
    endpoint steep xpxl1 yend1 xgap1

    -- handle second endpoint
    let xpxl2 = bx -- round (fromIntegral bx)
        yend2 = fromIntegral by + gradient * fromIntegral (xpxl2 - bx)
        xgap2 = fpart (fromIntegral bx + 0.5)
    endpoint steep xpxl2 yend2 xgap2

    -- main loop
    let intery = yend1 + gradient
    void $ if steep
        then foldlM (\i x -> do
            plot m (ipart i) x (colour (rfpart i))
            plot m (ipart i + 1) x (colour (fpart i))
            pure $ i + gradient) intery [xpxl1 + 1..xpxl2 - 1]
        else foldlM (\i x -> do
            plot m x (ipart i) (colour (rfpart i))
            plot m x (ipart i + 1) (colour (fpart i))
            pure $ i + gradient) intery [xpxl1 + 1..xpxl2 - 1]

    where
        endpoint :: Bool -> Int -> Double -> Double -> m ()
        endpoint True xpxl yend xgap = do
            plot m ypxl xpxl (colour (rfpart yend * xgap))
            plot m (ypxl + 1) xpxl (colour (fpart yend * xgap))
            where ypxl = ipart yend
        endpoint False xpxl yend xgap = do
            plot m xpxl ypxl (colour (rfpart yend * xgap))
            plot m xpxl (ypxl + 1) (colour (fpart yend * xgap))
            where ypxl = ipart yend

swapIf :: Bool -> (a, a) -> (a, a)
swapIf False p = p
swapIf True (x, y) = (y, x)

ipart :: Double -> Int
ipart = truncate

fpart :: Double -> Double
fpart x
    | x > 0 = x - temp
    | otherwise = x - (temp + 1)
    where temp = fromIntegral (ipart x)

rfpart :: Double -> Double
rfpart x = 1 - fpart x

main :: IO ()
main = do
  let inName = "back11.png"
  let outName = "back11_r180.png"
  saveImage rotateLeft180 inName outName

  let img0 = circle (1000, 1000) green 2000 2000
  writePng "circle.png" img0
  let img1 = line (10, 10) (400, 500) img0 2000 2000

  s <- randomIntList 20 (1, 1000)
  let ls = partList 2 s
  let lv = map (\(x:y:_) -> (x, y)) ls
  prex lv
  let lt = init $ partList 3 lv
  prex lt
  mapM_ (\_ -> do
               let img2 = triangle (40, 40) (300, 400) (1000, 800) img1
               print "ok"
           ) [0..10]
  let img2 = triangle (40, 40) (300, 400) (1000, 800) img1
  saveImage (triangle (10, 10) (100, 400) (300, 500)) "circle.png" "circle_xx.png"

  img <- readImage "back11.png" 
  let imgx = convertRGBA8 (either error id img)
  darkenImg <- darkenImage imgx 
  writePng "back11_darken.png" darkenImg
  muImg <- withMutableImageX 1000 1000 black $ \mu -> do
    {--    
    drawLineX (10, 80) (100, 800) red mu
    drawLineX (300, 500) (120, 400) green mu
    drawLineX (120, 400) (120, 800) green mu
    drawLineX (120, 800) (300, 500) green mu
    drawLineX (3, 2 + 6) (640 - 2, 480 - 3 + 6) blue mu
    --}  
    drawAntialiasedLine mu 3 2 (640 - 2) (480 - 3) (\x -> let level = round (x * 255) in PixelRGBA8 level level level 255)
    -- drawLineX (120, 310) (800, 300) blue mu
    drawLineX2 (800, 300) (120, 310) red mu
  print "ok"
  writePng "muImg.png" muImg
-- We start and end the line with sufficient clearance from the edge of the
  -- image to be able to see the endpoints
  img <- withMutableImageX 640 480 (PixelRGB8 0 0 0) $ \m@(MutableImage w h _) ->
          drawAntialiasedLine m 3 2 (w - 2) (h - 3)
          (\brightness -> let level = round (brightness * 255) in PixelRGB8 255 0 0)

  -- Write it out to a file on disc
  writePng "xiaolin-wu-algorithm.png" img
