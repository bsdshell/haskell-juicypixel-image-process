-- {{{ begin_fold
-- script
-- #!/usr/bin/env runhaskell -i/Users/cat/myfile/bitbucket/haskelllib
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE DuplicateRecordFields #-} 
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


changeColor :: (Int, Int, Int) -> Image PixelRGBA8 -> Image PixelRGBA8
changeColor (x, y, z) = pixelMap $ \(PixelRGBA8 r g b a) -> PixelRGBA8 (r + fi x) (g + fi y) (b + fi z) a 

main :: IO ()
main = do
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
      let t = (fi $ ls !! 0, fi $ ls !! 1, fi $ ls !! 2)
      let modified = changeColor t <$> image
      let imgName = "ex4_xx" ++ (show x) ++ ".png"
      case modified of 
              Left err -> print err
              -- Right image -> saveJpgImage 100 "ex4.png" $ ImageRGBA8 image
              Right image -> savePngImage imgName $ ImageRGBA8 image
          ) [0..10]

    print "ok"
