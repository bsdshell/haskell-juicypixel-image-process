-- {{{ begin_fold
-- script
-- #!/usr/bin/env runhaskell -i/Users/cat/myfile/bitbucket/haskelllib
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-} 
-- import Turtle
-- echo "turtle"

-- import Data.Set   -- collide with Data.List 
import Control.Monad
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
import Control.Monad (unless, when)
import Control.Concurrent 

import qualified Text.Regex.TDFA as TD

import AronModule 

import           Codec.Picture
import           Codec.Picture.Drawing
import           Codec.Picture.Types
import           Control.Monad.Primitive


geneImg::IORef Int -> Int -> Int -> IO()
geneImg ref w h = do
    n <- readIORef ref
    let pngName = "00" ++ (show n) ++ ".png"
    img <- withMutableImage w h (PixelRGB8 150 0 0) $ \m -> do
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
  return ()

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
  -- blend (PixelRGBA8 r0 g0 b0 a0) (PixelRGBA8 r1 g1 b1 a1) = PixelRGBA8 (r0 + r1) (g0 + g1) (b0 + b1) (a0 + a1)



{-|

  KEY: transpose image, transpose png, rotate image

  @
  saveImage tranImage "asteroid_s_5.png" "xx90.png"
  @
-}
tranImage :: Pixel a => Image a -> Image a 
tranImage img@Image {..} = generateImage (\x y -> pixelAt img y x) imageHeight imageWidth

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
  writePng fp2 img 

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

intensity:: FilePath -> FilePath -> IO ()
intensity fp1 fp2 = do
  img1 <- readImage fp1
  let rgbaImg1 = convertRGBA8 (either error id img1)
  let rgbaImg1_rot = rotateRight90 rgbaImg1
  -- Get image dimension
  let (w1, h1) = (imageWidth rgbaImg1, imageHeight rgbaImg1)
  let (w1', h1') = (imageWidth rgbaImg1_rot, imageHeight rgbaImg1_rot)
  fw "(width1, height1)"
  print (w1, h1)

  {-
  -- Genrate new image by blending pixel-by-pixel
  let newImg1 = generateImage 
                  -- (\x y -> intensityPixel $ pixelAt rgbaImg1 x y) 
                  (\x y -> let p0 = pixelAt rgbaImg1 x y; p1 = pixelAt rgbaImg1 (x + 1) y  in gradient p0 p1) 
                  (w1 - 1) 
                  h1
  -}
  
  let newImg1 = generateImage 
                  -- (\x y -> intensityPixel $ pixelAt rgbaImg1 x y) 
                  (\x y -> let p0 = pixelAt rgbaImg1_rot x y; p1 = pixelAt rgbaImg1_rot (x + 1) y  in gradient p0 p1) 
                  (w1' - 1)
                  h1'
  let rgbaImg1_rot_left = rotateLeft90 newImg1
  -- Write the resulting image to output path
  -- writePng fp2 newImg1
  writePng fp2 rgbaImg1_rot_left
  
{-|
blendPixel :: Float -> PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8
blendPixel alpha (PixelRGBA8 r0 g0 b0 a0) (PixelRGBA8 r1 g1 b1 a1) =
  let blend c0 c1 = round $ fi r0 * alpha + fi r1 * (1 - alpha)
  in PixelRGBA8 (blend r0 r1) (blend g0 g1) (blend b0 b1) (a0 + a1)
  -- in PixelRGBA8 (blend r0 r1) (blend g0 g1) (blend b0 b1) (a0 == 0 || a1 == 0 ? 0 $ blend a0 a1)
-}
blendPixel :: Float -> PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8
blendPixel alpha (PixelRGBA8 r0 g0 b0 a0) (PixelRGBA8 r1 g1 b1 a1) = PixelRGBA8 r0 g0 b0 a0

intensityPixel :: PixelRGBA8 -> PixelRGBA8
intensityPixel (PixelRGBA8 r0 g0 b0 a0) = PixelRGBA8 (round $ rf rI * rf r0 + rf gI * rf g0 + rf bI * rf b0) 0 0 a0 
  where
    rI = 0.299
    gI = 0.587
    bI = (1 - rI - gI) 

gradient :: PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8 
gradient (PixelRGBA8 r0 g0 b0 a0) (PixelRGBA8 r1 g1 b1 a1) = PixelRGBA8 intdiff 0 0 a0 
  where
    rI = 0.299
    gI = 0.587
    bI = (1 - rI - gI) 
    p0 = intensityPixel 
    inten0 = round $ rf rI * rf r0 + rf gI * rf g0 + rf bI * rf b0
    inten1 = round $ rf rI * rf r1 + rf gI * rf g1 + rf bI * rf b1
    intdiff = inten1 - inten0

mainX :: IO ()
mainX = do
    argList <- getArgs
    let fname = head argList
    nls <- readFileList fname >>= \cx -> return $ map (\x -> read x ::Int) cx
    let w = 2000
        h = 1000

    img <- withMutableImage w h (PixelRGB8 10 10 10) $ \m -> do
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
    dynamicImage <- readImage "ex4.png" 
    let image = convertRGBA8 <$> dynamicImage
    -- let modified = negative <$> image
    mapM_ (\x -> do
      ls <- randomIntList 10 (1, 200) 
      let t = (fi $ head ls, fi $ ls !! 1, fi $ ls !! 2)
      let modified = changeColor t <$> image
      let imgName = "ex4_xx" ++ show x ++ ".png"
      case modified of 
              Left err -> print err
              -- Right image -> saveJpgImage 100 "ex4.png" $ ImageRGBA8 image
              Right image -> savePngImage imgName $ ImageRGBA8 image
          ) [0..10]

    print "ok"
  
main :: IO ()
main = do
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
  intensity "bigboss.png" "intensity_y.png"
  mergeSameImageX 0.1 "intensity.png" "intensity_y.png" "int.png"
  saveImage (imagePadRight 100) "ex4_x.png" "ex4_xx.png"
  saveImage (imagePadDown 100)  "ex4_x.png"  "ex4_xxx.png"
