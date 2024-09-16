{-# LANGUAGE CPP #-}  -- forall
{-# LANGUAGE RankNTypes #-}  -- forall
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module AronDevLib where

import Control.Monad
import Control.Applicative
import Data.Typeable (typeOf) -- runtime type checker, typeOf "k"
import Data.Typeable
import Data.List 
import qualified Text.Pretty.Simple as TPS 
import qualified Control.Monad.IO.Class as CM
       
-- import AronModule
-- import AronGraphic
-- import Graphics.Rendering.OpenGL            as               GL

#if 0
class MyType a b c where
  rmInx :: a -> b -> c

instance MyType Int [a] [a] where
  rmInx n cx = removeIndex n cx

instance MyType Integer [a] [a] where
  rmInx n cx = removeIndex (fi n) cx
#endif

countNum x = map (\x -> (head x, length x)) $ groupBy (\a b -> a == b)  $ sort x

-- prexx::(CM.MonadIO m, Show a) => a -> m ()
-- prexx s = TPS.pPrintOpt TPS.CheckColorTty TPS.defaultOutputOptionsDarkBg {TPS.outputOptionsCompact = True} s

{-|
    KEY: Integer to Num
    Better name
-}   
integerToNum::(Integral a, Num b) => a -> b
integerToNum = fromIntegral

  
(÷)::forall a b c.(Typeable a, Num a, Typeable b, Num b, Fractional c) => a -> b -> c
(÷) x y = x' / y'
  where
    x' = case cast x of
           Just (x::Int) -> realToFrac x
           _             -> case cast x of
                               Just (x::Integer) -> realToFrac x
                               _                 -> case cast x of
                                                       Just (x::Float) -> realToFrac x
                                                       _               -> case cast x of
                                                                             Just (x::Double) -> realToFrac x
                                                                             _                -> error "Unknown type"
    y' = case cast y of
           Just (y::Int) -> realToFrac y
           _             -> case cast y of
                                Just (y::Integer) -> realToFrac y
                                _                 -> case cast y of
                                                        Just (y::Float) -> realToFrac y
                                                        _               -> case cast y of
                                                                              Just (y::Double) -> realToFrac y
                                                                              _                -> error "Unknown type"

(⨸)::forall a b c.(Typeable a, Num a, Typeable b, Num b) => a -> b -> Float
(⨸) x y = x' / y'
  where
    x' = case cast x of
           Just (x::Int) -> realToFrac x
           _             -> case cast x of
                               Just (x::Integer) -> realToFrac x
                               _                 -> case cast x of
                                                       Just (x::Float) -> realToFrac x
                                                       _               -> case cast x of
                                                                             Just (x::Double) -> realToFrac x
                                                                             _                -> error "Unknown type"
    y' = case cast y of
           Just (y::Int) -> realToFrac y
           _             -> case cast y of
                                Just (y::Integer) -> realToFrac y
                                _                 -> case cast y of
                                                        Just (y::Float) -> realToFrac y
                                                        _               -> case cast y of
                                                                              Just (y::Double) -> realToFrac y
                                                                              _                -> error "Unknown type"





(×)::forall a b c. (Typeable a, Num a, Typeable b, Num b) => a -> b -> Float
(×) x y = x' * y'
   where
     x' = case cast x of
            Just (x::Float) -> realToFrac x
            _               -> case cast x of
                                 Just (x::Int) -> realToFrac x
                                 _             -> case cast x of
                                                   Just (x::Double) -> realToFrac x
                                                   _                -> error "Unknown type"

     y' = case cast y of
            Just (y::Float) -> realToFrac y
            _               -> case cast y of
                                 Just (y::Int) -> realToFrac y
                                 _             -> case cast y of
                                                   Just (y::Double) -> realToFrac y
                                                   _                -> error "Unknown type"


(↑)::Int -> [a] -> [a]
(↑) n cx = take n cx
(↓)::Int -> [a] -> [a]
(↓) n cx = drop n cx


{-|
   @
   (GLdouble -> GLdouble, (GLdouble, GLdouble))
   
   let f = \x -> x^2
   let iv = (-1, 1)
   let g = \x -> x
   drawCurveD f iv green
   moveFromTo2 (f, iv) speed count $ drawRect2d (0.1, 0.1) >> drawRect2d (0.2, 0.2)
   @
-}
#if 0
moveFromTo2::(GLdouble -> GLdouble, (GLdouble, GLdouble)) -> Integer -> Integer -> IO() -> IO()
moveFromTo2 (f, (x0, x1)) speed count draw = do
  let n = 100.0::GLdouble
  let delta = (x1 - x0)/n
  print $ "count=" ++ (show count)
  let lx = map (\x -> rf x) $ map (\x -> x0 + x * delta) [0..n]
  let ls = map (\x -> Vector3 x (f x) 0.0::Vector3 GLdouble) lx
  let inx = speed * count - 1 in (inx < (len ls) - 1) ? (translate $ ls ! fi inx) $ (translate $ last ls)
  draw
  where
    (!) = (!!)           
#endif

fun44::Int -> Int
fun44 x = x + 2


#if 0

{-|
   Select the image =>
   - => decreasing the size
   + => increasing the size
   _____ http://localhost/image/opengl_cylinder.png
-}
drawCylinder::IO()
drawCylinder = do
    let n = 20::Int
        δ = (2*pi)/(rf(n-1)) :: Float
        r = 0.2
        br = 0.2
        σ = 1/rf(n-1)

        fx::Int -> Int -> GLfloat
        fx i j = let i' = rf i
                     j' = rf j
                     α  = δ*i'
                     β  = δ*j'
                 in r*cos(α)
        fy::Int -> Int -> GLfloat
        fy i j = let i' = rf i
                     j' = rf j
                     α  = δ*i'
                     β  = δ*j'
                     n = 3
                 in r*sin(α)
        
        fz::Int -> Int -> GLfloat
        fz i j = let i' = rf i
                     j' = rf j
                     α  = δ*i'
                     β  = δ*j'
                 in β*0.05
        -- in drawParamSurfaceN fx fy fz n
        in drawParamSurfaceN fx fy fz n
#endif

xIntToFloat::Int -> Float
xIntToFloat = fromIntegral

xIntegerToFloat :: Integer -> Float
xIntegerToFloat = fromIntegral
