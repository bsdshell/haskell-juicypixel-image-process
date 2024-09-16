{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-} -- support raw string [r|<p>dog</p> |]
{-# LANGUAGE CPP #-}

module AronModule where
import Control.Monad
import qualified Control.Monad.IO.Class as CM
import Control.Concurrent
import Control.Applicative hiding (some, many)
  
import qualified Data.Array.IO as DAO
import Data.Array.IO
    ( getElems,
      readArray,
      writeArray,
      MArray(newArray, getBounds),
      IOArray )
import Data.Char
import Data.Char
import qualified Data.Char8 as DC8
import qualified Data.List as L
import Data.List.Split
import Data.IORef
import Data.Time
import Data.Ratio
import Data.Maybe (fromJust, isJust, fromMaybe)
import Data.Time.Clock.POSIX
import Data.Foldable (foldrM)
import Data.Typeable (typeOf) -- runtime type checker, typeOf "k"
import Data.Typeable
import System.Directory
import System.Directory.Internal
import System.Environment
import System.Exit
import System.FilePath.Posix hiding (combine)
import System.IO
import System.IO.Silently
import System.Posix.Terminal (queryTerminal)
import System.Posix.IO (stdOutput, stdInput)
import Data.Ix

-- import qualified Data.ByteString.Char8 as BSC 

-- import qualified System.IO.Strict    as SIS

--- Friday, 12 November 2021 14:20 PST
--- BUG: Haddoc does not compile because ambiguous packages name: strict and strict-io
-- https://hackage.haskell.org/package/strict-io-0.2.2/docs/System-IO-Strict.html
-- import System.IO.Strict (SIO)
-- import qualified System.IO.Strict as SIO  -- strict-io , see .cabal file

import System.Posix.Files
import System.Posix.Unistd
import System.Posix.Types
import System.Process
import System.Random
import Text.Read (Read, readMaybe)
import Text.Regex
import Text.Regex.Base
import Text.Regex.Base.RegexLike

-- NOTE: Text.Regex.Posix should not be used, it DOES NOT support unicode char
-- import Text.Regex.Posix
import Text.Printf
import Numeric
import Debug.Trace (trace)

-- Thu 23 Feb 14:39:33 2023 
-- NOTE: Use resolver: lts-17.2
-- There is bug on TPS.outputOptionsCompact is not found with older lts
-- FIXED: the bug by changed the resolve to lts-17.2 in haskellwebapp2/stack.yaml
-- SEE: haskellwebapp2/stack.yaml 
-- Text.Pretty.Simple => prettyprinter-1.7.1
-- SEE: dependencies: => stack ls dependencies
import qualified Text.Pretty.Simple as TPS 

import qualified Network.HTTP.Conduit as CO
import qualified GHC.Generics         as GEN

-- import Data.Array  -- TODO: change to DR
import qualified Data.Array as DR
import qualified Data.Vector         as V
import qualified Data.HashMap.Strict as M
import qualified Text.Regex.TDFA     as TD
import qualified Text.Regex          as TR
import qualified Data.Set            as DS
import qualified Data.Word           as DW

import qualified Data.ByteString.Lazy      as BL  -- Lazy ByteString
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Lazy.Internal as IN (ByteString)
import qualified Data.ByteString.UTF8      as BSU
import qualified Data.ByteString           as BS  -- Strict ByteString
import qualified Data.Text.Lazy            as TL
import qualified Data.Text                 as TS  -- Strict Text
import qualified Data.Text.IO              as TSO
import qualified Data.Text.Encoding        as TSE
import qualified Data.Text.Lazy.Encoding   as TLE
import qualified Data.ByteString.Char8     as S8 (putStrLn, putStr, lines, isInfixOf)   -- strict ?
import qualified Data.ByteString.Internal  as BSI (c2w)
import Control.Lens hiding (pre, Empty)

-- {-# LANGUAGE QuasiQuotes #-} -- support raw string [r|<p>dog</p> |]
import Text.RawString.QQ (r)         -- Need QuasiQuotes too 


-- import qualified Data.HashSet as HS conflict with Data.HashMap.Strict
import qualified Data.HashSet as HSET
import qualified Data.Hashable as DH

import qualified Foreign.C.Types as FCT
import qualified Data.Aeson      as DA
-- import Rainbow
-- import System.Console.Pretty (Color (..), Style (..), bgColor, color, style, supportsPretty)

import qualified System.Console.ANSI as AN


import           Data.Int (Int64)
import           Database.SQLite.Simple hiding (bind)
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToRow
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok

import qualified Database.MySQL.Base as MSQL
import qualified System.IO.Streams as Streams

import           Codec.Picture
import           Codec.Picture.Drawing
import           Codec.Picture.Types
import           Control.Monad.Primitive

-- BEG_ffi
--import Foreign
--import Foreign.Ptr
--import Foreign.C.Types
--import Foreign.C.String
--import System.IO.Unsafe
-- END_ffi

-- BEG
-- KEY: ansi color, console color
import Rainbow
import System.Console.Pretty (Color (..), Style (..), bgColor, color, style, supportsPretty)
-- https://hackage.haskell.org/package/ansi-terminal-0.10.3/docs/System-Console-ANSI.html
-- import System.IO (hFlush, stdout)
import qualified System.Console.ANSI as AN
-- END


import qualified Turtle as TUR (empty, shellStrictWithErr, ExitCode)
-- import Turtle (empty, shellStrictWithErr, ExitCode)

import qualified Database.Redis as RED
    (
        Connection,
        connect,
        defaultConnectInfo,
        connectMaxConnections,
        runRedis,
        disconnect,
        get,
        set
    )

-- import Turtle (empty, shellStrictWithErr, ExitCode)

--------------------------------------------------------------------------------
-- | How to build AronModule.hs
-- | ghc --make AronModule.hs
--------------------------------------------------------------------------------
--
{-|

<https://hackage.haskell.org/package/bytestring-0.10.4.0/docs/Data-ByteString-Char8.html Strict ByteString Char8>
<http://hackage.haskell.org/package/utf8-string-1.0.1.1/docs/Data-ByteString-UTF8.html Strict ByteString UTF8>
<https://hackage.haskell.org/package/bytestring-0.10.4.0/docs/Data-ByteString-Lazy-Char8.html Lazy ByteString Char8>
<http://hackage.haskell.org/package/utf8-string-1.0.1.1/docs/Data-ByteString-Lazy-UTF8.html Lazy ByteString UTF8
-}


{-|
 -
  KEY: haskell ffi, c function
  SEE: /Users/aaa/myfile/bitbucket/haskellffi/AronCLibFFI.c
 -}
foreign import ccall "print_ascii" 
    print_ascii_c :: IO() 

{-|
 -
  KEY: haskell ffi, c function
  SEE: /Users/aaa/myfile/bitbucket/haskellffi/AronCLibFFI.c
 -}
print_ascii_f::IO()
print_ascii_f = print_ascii_c


{-|
  === any dimension list
  * 2*[1, 2] = [2, 4]
  * [1, 2]+[3, 4] = [4, 6]
  * 2*[[1, 2]] = [[2, 4]]
-}
instance Num a => Num [a] where
    (+) = zipWith (+)
    (-) = zipWith (-)
    (*) = zipWith (*)
    abs = map abs
    signum = map signum
    fromInteger = repeat . fromInteger

------------------------------------------------------------------
-- | Overloading the sin, cos to avoid Haskell crazy strict type
-- | TODO: add test cases
class Cnum a where
    _sqrt,_sin,_cos::a -> Float
instance Cnum Int where
    _cos n = cos(realToFrac n)
    _sin n = sin(realToFrac n)
    _sqrt n = sqrt(realToFrac n)
instance Cnum Float where
    _cos n = cos n
    _sin n = sin n
    _sqrt n = sqrt n
instance Cnum Double where
    _cos n = cos $ realToFrac n
    _sin n = sin $ realToFrac n
    _sqrt n = sqrt $ realToFrac n
instance Cnum Integer where
    _cos n = cos (realToFrac n)
    _sin n = sin (realToFrac n)
    _sqrt n = sqrt (realToFrac n)

{-|
    === No much to say
    * Division is painful in Haskell
    * Try to overload _div for many types
    * TODO: add more combination?
    * TODO: add test cases
-}
class Dnum a b where
    type DivType a b
    _div::a->b-> DivType a b

instance Dnum Integer Float where
    type DivType Integer Float = Integer
    _div a b = div a (round b)

instance Dnum Float Integer where
    type DivType Float Integer = Integer
    _div a b = div (round a) b

instance Dnum Int Float where
    type DivType Int Float = Integer
    _div a b = div (toInteger a) (round b)

instance Dnum Float Int where
    type DivType Float Int = Integer
    _div a b = div (round a) (toInteger b)

instance Dnum Double Int where
    type DivType Double Int = Integer
    _div a b = div (round a) (toInteger b)

instance Dnum Double Integer where
    type DivType Double Integer = Integer
    _div a b = div (round a) b

instance Dnum Integer Double where
    type DivType Integer Double  = Integer
    _div a b = div a (round b)

instance Dnum Integer Integer where
    type DivType Integer Integer = Float
    _div a b = (realToFrac a) / (realToFrac b)

class Add a b where
    type SumTy a b
    addd :: a -> b -> SumTy a b

instance Add Integer Double where
    type SumTy Integer Double = Double
    addd x y = fromIntegral x + y

instance Add Double Integer where
    type SumTy Double Integer = Double
    addd x y = x + fromIntegral y

instance Add Double Int where
    type SumTy Double Int = Double
    addd x y = x + fromIntegral y

instance Add Int Double where
    type SumTy Int Double = Double
    addd x y = (fromIntegral x) + y

instance (Num a) => Add a a where
    type SumTy a a = a
    addd x y = x + y


-- | define record for all the code blocks
--  can not define [TS.Text] => sqlite3 does not support [TS.Text]
data CodeBlock =
    CodeBlock
    { codeBlockId :: Int64
    , header      :: TS.Text
    , codeblock   :: TS.Text
    , addedtime   :: Int64
    , score       :: Int64
    } deriving (Eq, Read, Show)

instance FromRow CodeBlock where
  fromRow = CodeBlock <$> field <*> field <*> field <*> field <*> field

-- What is 'Only'
-- https://hackage.haskell.org/package/postgresql-simple-0.4.9.0/docs/Database-PostgreSQL-Simple.html#t:ToRow
instance ToRow CodeBlock where
  toRow (CodeBlock _pId pHeader pCode addedtime score) = toRow (pHeader, pCode, addedtime, score)


data SqliteMaster = SqliteMaster {
        x_type::TS.Text,
        x_name::TS.Text,
        x_tbl_name::TS.Text,
        x_rootpage::Integer,
        x_sql::TS.Text
      } deriving(Eq, Read, Show)
instance FromRow SqliteMaster where
  fromRow = SqliteMaster <$> field <*> field <*> field <*> field <*> field

instance ToRow SqliteMaster where
  toRow (SqliteMaster x_type x_name x_tbl_name x_rootpage x_sql) = toRow (x_type, x_name, x_tbl_name, x_rootpage, x_sql)

-- Used in WaiLib.hs
data UpdateCodeBlock = UpdateCodeBlock{pid::Integer, newcode::String, begt::Integer, endt::Integer} deriving (GEN.Generic, Show)
instance DA.FromJSON UpdateCodeBlock
instance DA.ToJSON UpdateCodeBlock where
    toEncoding = DA.genericToEncoding DA.defaultOptions

data UpdateCodeBlockX = UpdateCodeBlockX{pidx::Integer, pidlistx :: [Integer], newcodex::String, begtx::Integer, endtx::Integer} deriving (GEN.Generic, Show)
instance DA.FromJSON UpdateCodeBlockX
instance DA.ToJSON UpdateCodeBlockX where
    toEncoding = DA.genericToEncoding DA.defaultOptions

data CodeBlockReply = CodeBlockReply{ok::String, retcmd::String, retdata::String, retbegt::Integer, retendt::Integer} deriving (GEN.Generic, Show)
instance DA.FromJSON CodeBlockReply
instance DA.ToJSON CodeBlockReply where
    toEncoding = DA.genericToEncoding DA.defaultOptions

data ReplyCode = ReplyCode{
                            rcmd :: TS.Text,
                            rerror :: TS.Text,
                            stdoutx :: TS.Text
                          } deriving (GEN.Generic, Show)

instance DA.FromJSON ReplyCode
instance DA.ToJSON ReplyCode where
    toEncoding = DA.genericToEncoding DA.defaultOptions


data CommandService = CommandService{cmdServ::String, paramArg::String, inputData::String} deriving (GEN.Generic, Show)
instance DA.FromJSON CommandService
instance DA.ToJSON CommandService where
    toEncoding = DA.genericToEncoding DA.defaultOptions

type CSSPro = (String, String)
-- type CSSKV = (String, String)


-- KEY: constant
epsilon__ :: Double 
epsilon__ = 1e-12


-- | division is painful like hell in Haskell
div'::Int->Int->Double
-- div' n m = (fromInteger(toInteger n)) / (fromInteger(toInteger m))
div' n m = fromIntegral n / fromIntegral m

divInteger::Integer -> Integer -> Double
divInteger n m = (fromIntegral n) / (fromIntegral m)


{-|
    === Num => a ring

    * Addition
    * Addition inverse
    * Multiplcation
    * Multiplcation identity
    * Associativity
    * Distribution over multiplication

    class Num a where
      (+):: a -> a -> a
      (-)::a -> a -> a
      (*)::a -> a -> a
      abs:: a -> a
      negate::a -> a
      signum::a -> a
      fromIntegral Integer -> a

    * Fractional => field
    class Num a => Fractional a where
        (/)::a -> a -> a
        recip :: a -> a
        fromRational :: Rational -> a
-}
-- fromIntegral::(Num a)=> Integral -> a
divII::(Num a, Fractional a) => Integer -> Integer -> a
divII n m = fromIntegral n / fromIntegral m


{-|
    KEY: real to Fractional
-}
rf::(Real a, Fractional b) => a -> b
rf = realToFrac

{-|
    KEY: 'Integral' to 'Num' 
-}
fi::(Integral a, Num b) => a -> b
fi = fromIntegral



------------------------------------------------------------------
-- | = simple matrix can be used in GHCi

mat = [
      [1, 2, 3],
      [4, 5, 6],
      [7, 8, 10]
      ]

matr =[
      [1%1, 2%1, 3%1],
      [4%1, 5%1, 6%1],
      [7%1, 8%1, 10%1]
      ]

mat'= [
       ["-2/3" , "-4/3" , "1"],
       ["-2/3" , "11/3" , "-2"],
       ["1"    , "-2"   , "1"]
      ]




eps::(Num a, Ord a)=> a -> a -> a -> Bool
eps a b e = (abs $ a - b) < e

-- | = Implement complex number operations
data C = C{x::Float, y::Float} deriving (Show)

instance Num C where
        (C x1 y1) + (C x2 y2) = C(x1 + x2) (y1 + y2)
        (C x1 y1) - (C x2 y2) = C(x1 - x2) (y1 - y2)
        (C x1 y1) * (C x2 y2) = C(x1*x2 - y1*y2) (x1*y2 + x2*y2)
        fromInteger n                     = C(fromInteger n) 0
        signum (C x y)              = C(signum x) (signum y)
        abs (C x y)                 = C(abs x) (abs y)

instance Eq C where
        (C x y) == (C x' y') = x == x' && y == y'
        (C x y) /= (C x' y') = not (x == y') && not (y == y')


data RedisGroup = Haskell | Java | Snippet | Cpp

redisBound::RedisGroup -> Integer 
redisBound Haskell = 100000
redisBound Java    = 200000
redisBound Snippet = 300000
redisBound Cpp     = 400000

{-|
   === KEY: complex conjugate
-}
con::C->C
con (C x y) = C x (-y)

{-|
   === KEY: complex magnitude
-}
mag::C->Float
mag (C x y) = abs(sqrt(realToFrac(x*x + y*y)))

norm::C -> Float
norm (C x y) = abs(sqrt(realToFrac(x*x + y*y)))

re::C->Float
re (C x _) = x

im::C->Float
im (C _ y) = y

co::C->(Float, Float)
co (C x y) = (x, y)

{-|
    === Convert Cartesian Coordinates to Polar Coordinates
-}
toPolar::C ->(Float, Float)
toPolar (C x y) = (r, acos (x/r))
    where
        r = sqrt (x*x + y*y)

{-|
    === Convert Polar Coordinates to Cartesian Coordinates
-}
toCard::(Float, Float) -> C
toCard (r, a) = C (r*cos(a)) (r*sin(a))

{-|
  === approximate nature log function with Integral function
  \[
     \log_e x = \int_{1}^{x} \frac{d t}{t}
  \]

  * Partition the interval \( x \gt 1, [1, x] \text{ or } x \lt 1, [x, 1] \) to \( n = 100 \)
  * Compute each step value on x-axis \( 1 + \frac{k - 1}{n}, k \gt 1 \) or \( \frac{1 - k}{n} - 1, k < 1\)
  * Compute the average hight of \( \frac{1}{x} \) between \( x_{i}, x_{i+1} \), \( h_i = \frac{f (x_i) + f(x_{i+1})}{2} \)
  * The width of each step is \( w = (k - 1) \times \frac{1}{n} \)
  * Each \(i\) rectangle area is \( w \times h_i \)
  * Total area of all rectanges are \( \sum_{i=1}^{n} w \times h_i \)
  * NOTE: if \( k \) is close to \( n = 100 \), then \( \log_e \) is not longer accurate value.
  * NOTE: There is no rational polynomial to approximate \( \log_e \) in large value in fast speed.
  * <https://math.stackexchange.com/questions/977586/is-there-an-approximation-to-the-natural-log-function-at-large-values approximate_log_e>
-}
loge::Double -> Double
loge k = integral
    where
        n = 100::Integer
        del = if k == 1 then 0 else (if k > 1 then (k-1)*(divInteger 1 n) else (1-k)*(divInteger 1 n))
        stepList = map(\x ->if k == 1 then 0 else (if k > 1 then  1 + del * (fi x) else del*(fi x) - 1)) [0..n]
        yList = map(\x -> f x) stepList
        midValues = zipWith(\x y -> (x + y)/2 ) (init yList) (tail yList)
        integral = sum $ map(\x -> x*del ) midValues
        f = \x -> 1/x

{-|
    === square root for Complex number
-}
sqrtC:: C -> C -- square root for complex number
sqrtC c = toCard (sqrt r, a/2)
    where
        (r, a) = toPolar c

sqrtC':: C -> C
sqrtC' c = toCard (negate $ sqrt r, a/2)
    where
        (r, a) = toPolar c

{-|

   @
   radianToDegree::Float->Float
   radianToDegree x = x*r where r = 180/pi
   @

-}
radianToDegree::Float->Float -- see degreeToRadian
radianToDegree x = x*r where r = 180/pi
  
{-|

   @
   degreeToRadian::Float->Float
   degreeToRadian x = x*d where d = pi/180
   @

-}
degreeToRadian::Float->Float
degreeToRadian x = x*d where d = pi/180

tri::C->(Float, Float, Float)
tri (C x y) = (r, cos(x/r), sin(y/r)) where r = sqrt(x*x + y*y)

data GPoint = GPoint Int Int
getX::GPoint->Int
getX (GPoint x _) = x

getY::GPoint->Int
getY (GPoint _ y) = y

{-|
    === KEY: upper case string

    Same as 'upperStr'
-}
toUpperStr::String->String
toUpperStr s = foldr(\x y -> (toUpper x):y) [] s

{-|
    === KEY: lower case string

    Same as 'toLower'
-}
lowerStr::String->String
lowerStr s = foldr(\x y -> (toLower x):y) [] s

{-|
    === KEY: upper case string

    Same as 'toUpperStr'
-}
upperStr::String -> String
upperStr = toUpperStr

{-|
    === KEY: upper case
-}
upperChar::Char -> Char
upperChar = toUpper

{-|
    === Lower string

    <https://hoogle.haskell.org/?hoogle=tolower> tolower>

    >toLowerStr s = foldr(\x y -> (toLower x):y) [] s
-}
toLowerStr::String -> String
toLowerStr s = foldr(\x y -> (toLower x):y) [] s

{-|
   === KEY: concat string with delimter string s, concat str, join string

   >concatStr ["dog", "cat"] [] => "dogcat"
   >concatStr ["dog", "cat"] " " => "dog cat"
   >concatStr ["dog", "cat"] ":" => "dog:cat"
-}
concatStr::[String] -> String -> String -- concatStr ["dog", "cat"] ":" => "dog:cat"
concatStr [] s = []
concatStr cs s = dropEnd (len s) $ concatMap(\x -> x ++ s) cs




{-|
   === KEY: cat string, concat string

   >cat ["dog", "cat"] => "dogcat"
-}
-- cat::[String]->String
-- cat  [] = []
-- cat (x:xs) = x ++ cat xs


{-|
   === KEY: cat file, like command line cat

   >cat "/tmp/x.html"
-}
cat::String -> IO()
cat fn = readFileLatin1ToList fn >>= \x -> mapM_ putStrLn x

{-|
    === KEY: read file, find pattern, read dir and find pattern, read all keys

    > p <- getEnv "j"
    >readDirMatchKey p "\\.java$" "KEY:"

    @
    [
    ,
        ( "        KEY: system.in, standard input"
        , "/Users/cat/myfile/bitbucket/java/try_scanner.java"
        )
    ,
        ( "            // KEY: word boundary "
        , "/Users/cat/myfile/bitbucket/java/try_word_boundary.java"
        )
    ]
    @
-}
readDirMatchKey::FilePath -> String -> String -> IO [(String, String)]  -- readDirMatchKey "/tmp" "\\.java$" "KEY:"
readDirMatchKey fp typeRegex key = do
                fpList <- lsRegexFull fp typeRegex
                let zipFiles = zip fpList [1..]
                keyls <-  mapM (\fn -> do
                                      ls <- readFileLatin1ToList fn
                                      let lsz = zip ls (repeatN 10000 fn)
                                      filterM (\(line, _) -> return $ (containAscii line) && containStr key line) lsz ) fpList
                let mylist = foldr (++) [] keyls
                return mylist

{-|
    === Check whether a string @p@ contains a string @s@ pattern

    * Use regex to check match

    >"^dog"
    >"dog$"
    >matchTest (mkRegex p) s

    > containStr "^dog"  "dog cat" => True
    >
    >str = "dog cat"
    >p = "dog"
    >containStr p str == return True
    >containStr p s   == matchTest (mkRegex p) s

    * See 'containPrefix' 'containSuffix'
-}
containStr::String -> String -> Bool -- containStr pat str = matchTest (mkRegex pat) str
containStr p s = matchTest (mkRegex p) s

{-|
    === Check non-ascii char in a string
-}
containAscii::String -> Bool
containAscii s = and $ map isAscii s



{-|
    === Check non-ascii char in a [String]

    check file whether it contains non-ASCII char or line

    >[(Line #, Line)] = [(Integer, String)]

-}
containNonAsciiList::[String] -> [(Integer, String)]
containNonAsciiList ls = filter (\(x, y) -> x /= -1 ) tup
               where
                 tup = zipWith (\x y ->  (not . containAscii) y ? (x, y) $ (-1, [])  ) [0..] ls
                 (?)::Bool -> a -> a -> a
                 (?) pred a b = if pred then a else b
                 infix 1 ?

{-|
    === match once string and return Maybe (index, length)

    >matchAny (mkRegex "dog|cat") "mydog" => Just (2, 3)

    See 'matchOnce' or 'matchAll'
    <https://hackage.haskell.org/package/regex-base-0.94.0.0/docs/Text-Regex-Base-RegexLike.html#v:matchAll matchAll>
-}
matchAny::String -> String -> Maybe (Int, Int)
matchAny rs s = case (matchOnce rex s) of
                    Just x -> Just $ head $ DR.elems x
                    _      -> Nothing
           where
            rex = mkRegex rs

{-|
    === match once string and return Maybe (index, length)

    >matchAnyRegex (mkRegex "dog|cat") "mydog" => Just (2, 3)

    See 'matchOnce' or 'matchAll'
    <https://hackage.haskell.org/package/regex-base-0.94.0.0/docs/Text-Regex-Base-RegexLike.html#v:matchAll matchAll>
-}
matchAnyRegex::Regex -> String -> Maybe (Int, Int)
matchAnyRegex rex s = case (matchOnce rex s) of
                        Just x -> Just $ head $ DR.elems x
                        _      -> Nothing

type SKey = String

{-|
    === Match a string using index as score

    * See 'matchAny'  'matchAnyRegex'

    > type SKey = String
-}
matchScore::SKey -> String -> (Int, String)
matchScore k s = case matchAny k s of
                     Just x -> (fst x, s)
                     _      -> (100,   s)

    where
        ls = filter(\x -> len x > 0) $ splitSPC s

{-|
    === Match a string using index as score

    * See 'matchAny'  'matchAnyRegex'

    > type SKey = String
-}
matchScoreList::SKey -> [String] -> [(Int, String)]
matchScoreList k cs = map(\x -> matchScore k x) cs


{-|
    === Check non-ascii char in a file
    <file:///Users/cat/myfile/bitbucket/stackproject/jupyterlab/containsAscii.html Jupyterlab>
-}
fileContainNonAscii::FilePath -> IO[(Integer, String)]
fileContainNonAscii fp = do
    flist <- readFileLatin1ToList fp
    let nonAsciiList = containNonAsciiList flist
    return nonAsciiList


{-|
    === file has all the lines which contain a 'pattern'
-}
fileContain::FilePath -> String -> IO [String]
fileContain fp pat = do
    ls <- readFileList fp
    let fs = filter(\x -> containStr pat x) ls
    return fs

{-|
    === file has all the lines which contain a 'pattern'

    Same as 'fileContain'
-}
-- fileHas::FilePath -> String -> IO [String]  -- fileHas "/tmp" "txt",  same as fileContain
-- fileHas = fileContain

fileHas::(String -> Bool) -> FilePath -> IO [String]
fileHas f fp = do
  ls <- readFileList fp
  let fs = filter f ls
  return fs

{-|
    === filter out a list of pattern from a list of string

    \[
    \mbox{De Morgan's laws: union and intersection}
    \begin{aligned}
     A ∪ (B ∩ C) &= (A ∩ B) ∪ (A ∩ B) \\
    (A ∪ B)^¬ &= A^¬ ∩ B^¬  \\
    (A ∩ B)^¬ &= A^¬ ∪ B^¬
    \end{aligned}
    \]

    * \( A ⊗ B \)
    * 'containStr' both "a" \( ⊗ \) "b"

    > filterList id ["a", "b"]  ["aa", "kk", "ab"] ⇒ ["ab"]

    * 'containStr' "a"

    > filterList id ["a"]      ["aa", "kk", "ab"] ⇒ ["aa", "ab"]
    >

    * \( (A ⊗ B)^¬ = A^¬ ⊕ B^¬ \)
    * not 'containStr' "a" \( ⊕ \) "b"

    > filterList not ["a", "b"] ["aa", "bb", "ab"] ⇒ ∅
-}
filterList::(Bool -> Bool) -> [String] -> [String] -> [String]
filterList f [] ys =  ys
filterList f (x:cs) ys = filterList f cs (filter (\y -> f $ containStr x y) ys)

{-|
    === KEY: intersection of two lists, intersection of two sets 

    * USE: 'interList'

    * Deprecated
-}
intersectSet::(Eq a, DH.Hashable a) => [a] -> [a] -> [a]
intersectSet s1 s2 = ar
  where
    a1 = HSET.fromList s1
    a2 = HSET.fromList s2
    ar = HSET.toList $ HSET.intersection a1 a2

{-|
    === KEY: Difference between two lists

    @
    cx = ["a", "b"]
    cy = ["a", "c"]
    diffList cx cy => (["b"], ["c"])
    @
-}
diffList :: (Eq a, DH.Hashable a) => [a] -> [a] -> ([a], [a])
diffList cx cy = (ls, lt)
    where
        s1 = HSET.fromList cx
        s2 = HSET.fromList cy
        ix = HSET.intersection s1 s2  
        ls = HSET.toList $ (HSET.difference s1 ix)
        lt = HSET.toList $ (HSET.difference s2 ix)


{-|
    === KEY: Intersection between two lists

    @
    cx = ["a", "b"]
    cy = ["a", "c"]
    diffList cx cy => (["b"], ["c"])
    @
-}
interList :: (Eq a, DH.Hashable a) => [a] -> [a] -> [a]
interList cx cy = ls 
    where
        s1 = HSET.fromList cx
        s2 = HSET.fromList cy
        ix = HSET.intersection s1 s2
        ls = HSET.toList ix

{-|
    === intersection of two files
-}
intersectFile::FilePath -> FilePath -> IO [String]
intersectFile f1 f2 = do
      s1 <- readFileLatin1ToList f1
      s2 <- readFileLatin1ToList f2
      return $ intersectSet s1 s2

{-|
    === KEY: filter non empty string, no empty line
-}
filterNonEmpty::[String] -> [String]
filterNonEmpty cs = filter(\x -> (len . trim) x > 0) cs

{-|
    === Check whether str contains sub as prefix

    >containPrefix "dog" "dogcat"  => True
    >containPrefix "dog" " dogcat" => False

    * See 'containStr' 'containSuffix'
-}
containPrefix::String -> String -> Bool  -- containPrefix "--" "--ab" => True
containPrefix sub str = let m = len sub; n = len str in if m <= n then sub == take m str else False

{-|
    === KEY: Check whether str contains sub as suffix

    >containPrefix "cat" "dogcat"  => True
    >containPrefix "cat" "dogcat " => False

    * See 'containStr' 'containPrefix'
-}
containSuffix::String -> String -> Bool  -- containSuffix "cat" "dogcat" => True
containSuffix sub str = let m = len sub; n = len str in if m <= n then sub == takeEnd m str else False

{-|
    === KEY: Check whether str has sub as prefix

    >containPrefix "cat" "dogcat"  => True
    >containPrefix "cat" "dogcat " => False

    * See 'containPrefix'
-}
hasPrefix::String -> String -> Bool  -- hasPrefix "--" "--ab"  => True
hasPrefix = containPrefix

hasSuffix::String -> String -> Bool -- hasSuffix ".png" "f.png" => True
hasSuffix = containSuffix

hasStr::String -> String -> Bool -- hasStr ".png" "f.png" => True
hasStr = containStr

{-|
   === check image, check png file
-}
isImage::String -> Bool
isImage s = ext == ".png" || ext == ".jpeg" || ext == ".jpg" ? True $ False
  where
    ext = lowerStr $ takeExt s

{-|
   === ls table
-}
lsTable::String -> IO [[String]]
lsTable fp = do
  let cmd = "ls -AhlLt " ++ fp
  (_, stdout, error) <- runSh $ strToStrictText cmd
  if (toStr error) == "" then do
    let ls = linesST stdout
    let lss = map splitSPC $ map toStr ls
    return lss
  else return $ [[""]]


{-|
    === KEY: without any regex substitute,

    ISSUE: See 'replaceFileLine'
    ISSUE: See 'replaceList'

    @
    let sub = "\\"  -- sub is one character => len sub == 1
    pp $ len sub
    ls <- replaceFileLineEscape "abc" sub  "/tmp/11.x"
    writeFileList "/tmp/22.x" ls
    @
-}
replaceFileLineEscape::String -> String -> FilePath -> IO [String]
replaceFileLineEscape pat sub fp = do
  ls <- readFileList fp
  let sub' = dropEnd 1 $ drop 1 $ show sub
  return $ map(\x -> if matchTest (mkRegex pat) x then sub' else x) ls


replaceFileLineEscapeStrict::FilePath -> String -> String -> IO [String]
replaceFileLineEscapeStrict fp pat sub = do
  ls <- readFileListStrict fp
  let sub' = dropEnd 1 $ drop 1 $ show sub
  return $ map(\x -> if matchTest (mkRegex pat) x then sub' else x) ls



{-|
    === KEY: string with escape character

    ISSUE: See 'replaceFileLine' 'replaceList' 'replaceFileLineEscape'

    @
    let s = "\"
    let s1 = show "\\"
    let s2 = dropEnd 1 $ drop 1 s1
    let s2 = \\
    writeFileList "/tmp/x.x" [s2]  -- write s2 to file
    @
-}
strWithSlash::String -> String
strWithSlash x = dropEnd 1 $ drop 1 $ show x

{-|
    === KEY: without any regex substitute, remove string and append

    ISSUE: See 'replaceFileLineEscape'
    ISSUE: See 'replaceList'

    @
    let sub = "\\"  -- sub is one character => len sub == 1
    let sub' = dropEnd 1 $ drop 1 $ show sub
    pp $ len sub
    ls <- replaceFileLine "abc" sub'  "/tmp/11.x"
    writeFileList "/tmp/22.x" ls
    @
-}
replaceFileLine::String -> String -> FilePath -> IO [String]
replaceFileLine pat sub fp = do
  ls <- readFileList fp
  let sub' = sub
  return $ map(\x -> if matchTest (mkRegex pat) x then sub' else x) ls


{-|
    === KEY: Generate [([String], Integer, [String])] from 'captureCppFun'

    @ 
    [(
        [ "AronLib.e"
        , "AronLib.ew"
        , "AronLib.ewl"
        , "AronLib.ewli"
        , "AronLib.ewlin"
        , "AronLib.ewline"
        , "AronLib.i"
        , "AronLib.in"
        , "AronLib.ine"
        , "AronLib.l"
        , "AronLib.li"
        , "AronLib.lin"            <-  [String]
        , "AronLib.line"
        , "AronLib.n"
        , "AronLib.ne"
        , "AronLib.new"
        , "AronLib.newl"
        , "AronLib.newli"
        , "AronLib.newlin"
        , "AronLib.newline"
        , "AronLib.w"
        , "AronLib.wl"
        , "AronLib.wli"
        , "AronLib.wlin"
        , "AronLib.wline"
        ]
    , 40007                        <- Integer
    , [ "void newline(){" ]        <- [String]
    )
    ]
    @ 

    @
    [([String], Integer, [String])]
    @
 -}
redisExtractCppAronLib::String -> [String] -> [([String], Integer, [String])]
redisExtractCppAronLib package cx = retMap 
    where
       -- lt = captureCppFun cx
       ixBound = redisBound Cpp
       lt = extraTags cx
       rMap = zipWith(\n x -> (substr $ snd x, n, [fst x])) [ixBound..] lt 
       retMap = map (\ls -> (map(\x -> package ++ x) $ t1 ls, t2 ls, t3 ls)) rMap
       substr s = unique $ join $ allSubstr s

{-|
 
    @
        etags -e -f $PWD/TAGS $cpplib/AronLib.h $b/clib/AronCLibNew.h       
               ↑ 
               + -> has to be before '-f'
    @
 
    @
    [
        ( "string removeIndex(string s, int inx) {"
        , "removeIndex"
        ),
        ( "vector<T> removeIndex(vector<T>& vec, int inx){"
        , "removeIndex"
        ),
        ( "vector<T> removeIndexRange(vector<T>& vec, int fromInx, int toInx){"
        , "removeIndexRange"
        )
    ]
    @

 -}
readTagsFile::FilePath -> IO [(String, String)]
readTagsFile fp = do 
        ls <- readFileList fp 
        let lss = map (\s -> splitWhen(\x -> x == '\x7f' || x == '\x01') s) ls
        let lstup = map (\s -> if len s == 3 then (head s, (head . tail) s) else ("", "")) lss
        return $ filter (\(a, _) -> len a > 0) lstup

{-|
    === KEY: extra function from emacs TAGS file

    @
        etags -e -f $PWD/TAGS $cpplib/AronLib.h $b/clib/AronCLibNew.h       
               ↑ 
               + -> has to be before '-f'
    @
 -}
extraTags::[String] -> [(String, String)]
extraTags cs = filter (\(a, _) -> len a > 0) lt 
    where
      ls = map (\s -> splitWhen(\x -> x == '\x7f' || x == '\x01') s) cs 
      lt = map (\s -> if len s == 3 then (head s, (head . tail) s) else ("", "")) ls 


{-|
   === KEY: Replace a matching string with other string

   >replaceList [" pat ", "  pat"] "pat" "sub" ⟹   [" sub ", "  sub"]

   ERROR: "\\" and "\\\\" with same output

   @
   let n = "\\"
   let m = "\\\\"
   let n' = subRegex(mkRegex "abc") "abc" n
   let m' = subRegex(mkRegex "abc") "abc" m
   pp $ n' == m'
   @
 -}
replaceList::[String]->String->String->[String]  -- [" pat ", "  pat"] "pat" "sub" => [" sub ", "  sub"]
replaceList [] _ _ = []
replaceList (x:xs) p r = subRegex (mkRegex p) x r : replaceList xs p r


{-|
    === Search a pattern and partition it three parts: [(String, String, String)]

    @
    searchSplitAny "my cat eats my 123 cat"  "[[:digit:]]+"
    [("my cat eats my ","123"," cat")]
    > searchSplitAny "my cat eats my 123 cat 44a"  "[[:digit:]]+"
    [("my cat eats my ","123"," cat 44a"),(" cat ","44","a")]
    @
-}
searchSplitAny::String -> String -> [(String, String, String)]
searchSplitAny s srx = map(\x -> let   t1 = fst x;
                                       t2 = snd x;
                                       c  = (fst t1) + (snd t1) -- 0
                                       fs = fst (snd x) -- 6
                                       sn = snd (snd x) -- 3
                                       re = drop c s
                                       rs = take (fs - c) $ drop c s -- "mydog "
                                       rx = drop (fs - c) s -- "dog-- dog pig cat"
                                       in (rs, take sn $ drop fs s, drop (fs + sn) s)) tp

                            where
                                -- r  = makeRegex ("\\<" ++ srx ++ "\\>")::TD.Regex -- srx boundary
                                r  = makeRegex (srx)::TD.Regex -- srx boundary
                                ar = matchAllText r s
                                tt = (0, 0):(map(\x -> snd $ x DR.! 0 ) ar) -- [(0, 0), (6, 3), (12, 3)]
                                tp = zipWith(\x y -> (x, y)) (init tt) (tail tt) -- [((0, 0), (6, 3)), ((6, 3), (12, 3))]

{-|
    === search and replace WORD only

    @
    searchSplit "mydog dog-- dog pig cat" "dog"
    [("my","dog"," dog-- dog pig cat"),(" ","dog","-- dog pig cat"),("-- ","dog"," pig cat")]
    @
-}
searchSplitWord::String -> String -> [(String, String, String)]
searchSplitWord s srx = map(\x -> let  t1 = fst x;
                                       t2 = snd x;
                                       c  = (fst t1) + (snd t1) -- 0
                                       fs = fst (snd x) -- 6
                                       sn = snd (snd x) -- 3
                                       re = drop c s
                                       rs = take (fs - c) $ drop c s -- "mydog "
                                       rx = drop (fs - c) s -- "dog-- dog pig cat"
                                       in (rs, take sn $ drop fs s, drop (fs + sn) s)) tp

                            where
                                r  = makeRegex ("\\<" ++ srx ++ "\\>")::TD.Regex -- srx boundary
                                -- r  = makeRegex (srx)::TD.Regex -- srx boundary
                                ar = matchAllText r s
                                tt = (0, 0):(map(\x -> snd $ x DR.! 0 ) ar) -- [(0, 0), (6, 3), (12, 3)]
                                tp = zipWith(\x y -> (x, y)) (init tt) (tail tt) -- [((0, 0), (6, 3)), ((6, 3), (12, 3))]


{-|
    === KEY: search replace any char, substitude string

    * The code is based on 'matchAllText' in package: 'Text.Regex.TDFA' as TD

    @
    searchReplaceAny "mydog dog-- dog pig cat" "dog" "[\\0]" => "my[dog] [dog]-- [dog] pig cat"
    @

    * Regex is from TDFA, NOT Text.Regex

    >word => "[[:alpha:]]+"

    * NOTE: support ONE String only, e.g. searchReplace s "dog"  "[\\0]"

    * TODO: support multiple words, e.g. searchReplace s "dog|cat"  "[\\0]"

    See 'searchReplace'

    TODO: add test
-}
searchReplaceAny::String -> String -> String -> String  -- searchReplaceAny "mydog dog pig cat" "dog" "[\\0]" => my[dog] [dog]
searchReplaceAny s srx rep = ls
            where
                ss = searchSplitAny s srx
                ls = if len ss > 0 then
                        let second = t3 $ last ss
                        in foldr(\x y -> x ++ y) [] $ (map(\x -> (t1 x) ++ (TR.subRegex (TR.mkRegex srx) (t2 x) rep)) $ ss) ++ [second]
                     else s

                t1 (x, y, z) = x
                t2 (x, y, z) = y
                t3 (x, y, z) = z


searchReplaceAnyTup::String -> (String, String) -> String
searchReplaceAnyTup s (srx, rep) = searchReplaceAny s srx rep

searchReplaceAnySBS::BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString  -- searchReplaceAnySBS "mydog dog pig cat" "dog" "[\\0]" => my[dog] [dog]
searchReplaceAnySBS s srx rep = toSBS ls
            where
                s'   = toStr s
                srx' = toStr srx
                rep' = toStr rep

                ss = searchSplitAny s' srx'
                ls = if len ss > 0 then
                        let second = t3 $ last ss
                        in foldr(\x y -> x ++ y) [] $ (map(\x -> (t1 x) ++ (TR.subRegex (TR.mkRegex srx') (t2 x) rep')) $ ss) ++ [second]
                     else s'

                t1 (x, y, z) = x
                t2 (x, y, z) = y
                t3 (x, y, z) = z



{-|
    === Search replace word, the code is based on 'matchAllText' in package: Text.Regex.TDFA as TD

    >let s = "mydog dog-- dog pig cat dog fox"
    >searchReplace s "dog" "[\\0]"
    >"mydog [dog]-- [dog] pig cat [dog] fox"

    * Text.Regex => subRegex does not support word boundary

    >r  = makeRegex ("\\<" ++ word ++ "\\>")::TD.Regex -- word boundary

    * Regex is from TDFA, NOT Text.Regex
    >word => "[[:alpha:]]+"

    * NOTE: support ONE word only, e.g. searchReplace s "dog"  "[\\0]"

    * TODO: support multiple words, e.g. searchReplace s "dog|cat"  "[\\0]"

    See 'searchReplaceAny'

    Sun 31 May 13:05:58 2020
    1. Fixed bug: if no matches, then the origin string should be returned
    2. Moved 'searchSplit' outside so that 'searchReplaceAny' can use it.

    @
    searchSplit "mydog dog-- dog pig cat" "dog"
    [("my","dog"," dog-- dog pig cat"),(" ","dog","-- dog pig cat"),("-- ","dog"," pig cat")]
    @

    @
    > searchReplace "mydog dog-- dog pig cat" "dog" "[\\0]"
    "mydog [dog]-- [dog] pig cat"
    > searchReplaceAny "mydog dog-- dog pig cat" "dog" "[\\0]"
    "my[dog] [dog]-- [dog] pig cat"
    >
    @
-}
searchReplace::String -> String -> String -> String -- searchReplace "my dog is dog" "dog" "[\\0]" => "my [dog] is [dog]"
searchReplace s srx rep = ls
            where
                ss = searchSplitWord s srx
                ls = if len ss > 0 then
                        let second = t3 $ last ss
                        in foldr(\x y -> x ++ y) [] $ (map(\x -> (t1 x) ++ (TR.subRegex (TR.mkRegex srx) (t2 x) rep)) $ ss) ++ [second]
                     else s
                -- ls = foldr(\x y -> x ++ y) [] $ (map(\x -> (t1 x) ++ (TR.subRegex (TR.mkRegex srx) (t2 x) rep)) $ ss) ++ [second]

                t1 (x, y, z) = x
                t2 (x, y, z) = y
                t3 (x, y, z) = z

{-|
   === Same as 'searchReplace' but it is better name

   search and replace word
-}
searchReplaceWord::String -> (String, String) -> String  -- searchReplaceWord "my dogman is dog" ("dog", "[\\0]") => "my dogman is [dog]"
searchReplaceWord s (srx, rep) = searchReplace s srx rep

{-|
    === interleave two lists

    >interleave ["a", "b"] ["a1", "b1"]
    >["a", "a1", "b", "b1"]

    Use two operation `tran` or `transpose` and `concat`

    >concat (tran [xs, ys])

    <https://stackoverflow.com/questions/22702064/how-to-interleave-two-lists-in-haskell-in-one-line-with-higher-order-functions SO Ref>
-}
interleave::[a] -> [a] -> [a]
interleave xs ys = concat (tran [xs, ys])


{-|
    === intersperseInner a list

    Same as `intersperse` from <http://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.OldList.html#intersperse Data.List>

    > intersperseInner "-", [] = []
    > intersperseInner "-", ["a"] = ["a"]
    > intersperseInner "-", ["a", "b"] = ["a", "-", "b"]
-}
intersperseInner::a -> [a] -> [a]
intersperseInner a [] = []
intersperseInner a [x] = [x]
intersperseInner a cx = init $ foldr (<>) [] $ map(\x -> [x, a])  cx

{-|
    === Add sth between list

    > listIn "-", [] = []
    > listIn "-", ["a"] = ["a"]
    > listIn "-", ["a", "b"] = ["a", "-", "b"]

    > concat $ listIn "-" ["a", "b"]  -- "a-b"

    same as 'intersperseOuter' but it is better name
-}
listIn::a -> [a] -> [a]  -- listIn "-", ["a", "b"] = ["a", "-", "b"]
listIn x cx = intersperseInner x cx

{-|
    === Add sth OVER the list

    > listOut "-", [] = []
    > listOut "-", ["a"] = ["-", "a", "-"]
    > listOUt "-", ["a", "b"] = ["-", "a", "-", "b", "-"]

    > concat $ listOut "-" ["a", "b"]  -- "-a-b-"

    Same as 'intersperseInner' but it is better name
-}
listOut::a -> [a] -> [a]   -- listOut "-", ["a"] = ["-", "a", "-"]
listOut x cx = intersperseOuter x cx

{-|
    === intersperseOuter a list

    > intersperseOuter "-", [] = []
    > intersperseOuter "-", ["a"] = ["-", "a", "-"]
    > intersperseOuter "-", ["a", "b"] = ["-", "a", "-", "b", "-"]
-}
intersperseOuter::a -> [a] -> [a]
intersperseOuter a [] = []
intersperseOuter a cx = foldl (<>) [a] $ map(\x -> [x, a])  cx

{-|
    === repeat a n times
    >repeat' 0 'a' => ""
    >repeat' 0 "a" => []
    >repeat' 3 'a' => aaa
    >repeat' 3 "a" => ["a", "a", "a"]
    * Deprecated
    * Use: 'repeatN'
-}
repeat'::Integer->a->[a]
repeat' n a = map(const a)[1..n]

{-|
    === repeat n times
    > repeat's 0 'a' => "'
    > repeat's 0 "a" => []
    > repeat's 3 "pig" => ["pig", "pig", "pig"]
    * Use repeatN instead of `repeat'`
-}
repeatN::Integer->a->[a]
repeatN n a = map(const a)[1..n]

{-|
    === repeat n times
    > repeat's 0 'a' => "'
    > repeat's 0 "a" => []
    > repeat's 3 "pig" => ["pig", "pig", "pig"]
    * same as `replicate` but `Integer` type
-}
replicateN::Integer->a->[a]
replicateN n a = map(const a)[1..n]

{-|
    === replicate n times
    >replicate' 4 'a' => "aaaa"
-}
replicate'::Int->a->[a]
replicate' n x = take n (repeat x)

{-|
    == fill a list with missing number

    > fillList 0 [1, 2, 4, 7]
    > [1, 2, 0, 4, 0, 0 7]
    >
    > fillList 0 [-1, 2, 4, 6, 10]
    > [-1,0,0,2,0,4,0,6,0,0,0,10]
    @
    pp $ fillList 0 [] == []
    pp $ fillList 0 [0] == [0]
    pp $ fillList 0 [1] == [1]
    pp $ fillList 0 [1, 2] == [1, 2]
    pp $ fillList 0 [1, 3] == [1, 0, 3]
    @
-}
fillList:: Integer -> [Integer] -> [Integer]
fillList _ [] = []
fillList _ [x] = [x]
fillList a (x:y:cs) = let diff = y - x - 1 in if diff > 0 then x:(replicateN diff a) ++ (fillList a (y:cs)) else x:fillList a (y:cs)



{-|
    === KEY: read file strictly

    See 'SIO.readFile'
    @
     SIO.run::NFData sa => SIO sa -> IO sa
     SIO.readFile::FilePath -> SIO String
    @
-}
-- readFileStrict::FilePath -> IO String
-- readFileStrict x = (SIO.run . SIO.readFile) x

readFileStrict::FilePath -> IO String
readFileStrict x = TSO.readFile x >>= return . toStr

{-|
    === KEY: read file strictly, return a list of string

    See 'SIO.readFile'
-}
-- readFileListStrict::FilePath -> IO [String]
-- readFileListStrict x = (SIO.run . SIO.readFile) x >>= return . lines

readFileListStrict::FilePath -> IO [String]
readFileListStrict x = (fmap linesST $ TSO.readFile x) >>= \e -> return $ fmap toStr e

{-|
    === readfile to list of 'BS.ByteString'
-}
readFileBSList::FilePath -> IO [BS.ByteString]  -- read file => List of strict BS.ByteString
readFileBSList fp = do
                ls <- BS.readFile fp
                let lsBS = linesBS ls
                return lsBS

{-|
    === read file and return strict ByteString
-}
readFileBS::FilePath -> IO BS.ByteString  -- read file => Strict BS.ByteString
readFileBS = BS.readFile

{-|
    === read file and return strict Text  'TS.Text'

    'Data.Text.IO' <https://hackage.haskell.org/package/text-1.2.4.0/docs/Data-Text-IO.html Data.Text.IO>
-}
readFileSText::FilePath -> IO TS.Text   -- read file => strict Text, TS.Text
readFileSText = TSO.readFile

{-|
    === read file to list of 'TS.Text'

    See 'linesSText'  'readFileSText'
-}
readFileSTextList::FilePath -> IO [TS.Text]  -- read file => IO [TS.Text]
readFileSTextList fp = do
                       text <- readFileSText fp
                       return $ linesST text


{-|
    === Char to strict ByteString
-}
charToStrictByteString::Char -> BS.ByteString
charToStrictByteString c = BS.singleton $ BSI.c2w c


{-|
    KEY: break newlines

    Like the 'P.lines' function from Prelude, but treat the @\"\\r\\n\"@ and
    @\"\\r\"@ sequences as newlines too, not just @\"\\n\"@.
-}
plines :: String -> [String]
plines []  = []
plines str = let (a, str') = breakNewline str
             in a : plines str'

{-|
    See 'plines'
-}
breakNewline :: String -> (String, String)
breakNewline []       = ([], [])
breakNewline (x : xs) =
    case x of
        '\n' -> ([], xs)
        '\r' -> ([], case xs of
                         ('\n' : xs') -> xs'
                         _            -> xs)
                -- The reason for the weird case expression instead of just a
                -- ('\r' : '\n' : s) pattern is for better laziness.
                -- Otherwise, lines ("hello\r" ++ undefined) would fail to
                -- completely yield the first line.  If we see a '\r', we know
                -- the line has ended, so don't force the next character
                -- immediately.
        _    -> let (line, rest) = breakNewline xs
                 in (x : line, rest)


{-|
    === KEY: replace pattern inside a file, replace string in a file

    * NOTE: It only replace one string in a file, NOT all strings

    See 'readFileRepPat', 'searchReplaceAny', 'searchReplace'
-}
replaceByteStringFile::FilePath -> BS.ByteString -> BS.ByteString -> IO BS.ByteString  -- replaceByteStringFile f.x pat sub
replaceByteStringFile fp pat rep = do
                       ls <- readFileBS fp
                       let arrtup = matchAllBS pat ls
                       let inx    = (fst . head)  arrtup
                       let ln     = (snd . head)  arrtup
                       let strtup = BS.splitAt inx ls
                       let left   = fst strtup
                       let right  = BS.drop ln (snd strtup)
                       let substr = left <> rep <> right
                       return substr

{-|
    === KEY: replace pattern inside a file, new file is created. Old file is NOT modified

    See 'readFileRepPat', 'searchReplaceAny', 'searchReplace', 'replaceByteStringFile'
-}
replaceFileWithBSToNew::FilePath -> (BS.ByteString, BS.ByteString) -> FilePath -> IO ()  -- replaceFileWithBSToNew "/tmp/keep.x" ("pat", "sub") "/tmp/new.x" , keep.x is not modified
replaceFileWithBSToNew fp cx np = do
  replaceByteStringFile fp (fst cx) (snd cx) >>= writeFileBS np

replaceFileWithStrToNew::FilePath -> (String, String) -> FilePath -> IO ()  -- replaceFileWithStrToNew "/tmp/keep.x" ("pat", "sub") "/tmp/new.x" , keep.x is not modified
replaceFileWithStrToNew fp tu np = do
  ls <- readFileList fp
  let ls' = map(\x -> searchReplaceAny x pat sub) ls
  writeFileList np ls'
    where
      pat = fst tu
      sub = snd tu


{-|
    === KEY: replace file with string, substitude string with a pattern

    * replace all words in a file with pattern string

    EXAMPLE: replaceDirWithStr pat sub dir

    ISSUE: if sub contains '\\' then, only one '\' is written to fname

    NOTE: replace word only

    @
     file.x
        replaceMe

      sub =
           \begin{pmatrix}
           a & b \\
           c & d
           \end{pmatrix}

     replaceFileWithStr "replaceMe" sub "/tmp/f.x"

     cat /tmp/f.x
           \begin{pmatrix}
           a & b \
           c & d
           \end{pmatrix}
    @
-}
replaceFileWithStr::String -> String -> FilePath -> IO() -- replaceFileWithStr pat sub fname
replaceFileWithStr pat sub fp = do
  fstr <- readFile fp
  rn <- timeNowSecond
  let newStr = searchReplaceAny fstr pat sub
  let copyName = takeName fp ++ "_" ++ (show rn)
  let cmd = "mv " ++ fp ++ " /tmp/" ++ copyName
  exitCode <- system cmd
  if exitCode == ExitSuccess then do
    writeFile fp newStr
    else error $ "Error: " ++ cmd

{-|
    === Search and replace word inside a file, the code is based on 'matchAllText' in package: Text.Regex.TDFA as TD

    See 'searchReplace'  replace word only

    >let s = "mydog dog-- dog pig cat dog fox"
    >searchReplace s "dog" "[\\0]"
    >"mydog [dog]-- [dog] pig cat [dog] fox"
-}
replaceFileWithWord::String -> String -> FilePath -> IO() -- replaceFileWithWord pat sub fname
replaceFileWithWord pat sub fp = do
  fstr <- readFile fp
  rn <- timeNowSecond
  let newStr = searchReplace fstr pat sub
  let copyName = takeName fp ++ "_" ++ (show rn)
  let cmd = "mv " ++ fp ++ " /tmp/" ++ copyName
  exitCode <- system cmd
  if exitCode == ExitSuccess then do
    writeFile fp newStr
    else error $ "Error: " ++ cmd

{-|
  === Search replace words with a list of tuple in a file

  See 'searchReplaceListWord' and 'searchReplace'

  @
  > searchReplaceListWord [("dog", "cat")] "mydog dog"
  > "mydog cat"
  @
-}
replaceFileListWord::[(String, String)] -> FilePath -> IO() -- replaceFileWithWord pat sub fname
replaceFileListWord ls fp = do
  str <- readFile fp
  rn <- timeNowSecond
  let newStr = searchReplaceListWord ls str
  let copyName = takeName fp ++ "_" ++ (show rn)
  let cmd = "mv " ++ fp ++ " /tmp/" ++ copyName
  exitCode <- system cmd
  if exitCode == ExitSuccess then do
    writeFile fp newStr
    else error $ "Error: " ++ cmd

{-|
  === Search replace String with a list of tuple in a file

  See 'searchReplaceListStr' and 'searchReplaceAny'

  @
  > searchReplaceListStr [("dog", "cat")] "mydog dog"
  > "mydog cat"
  @
-}
replaceFileListStr::[(String, String)] -> FilePath -> IO() -- replaceFileWithStr pat sub fname
replaceFileListStr ls fp = do
  str <- readFile fp
  rn <- timeNowSecond
  let newStr = searchReplaceListStr ls str
  let copyName = takeName fp ++ "_" ++ (show rn)
  let cmd = "mv " ++ fp ++ " /tmp/" ++ copyName
  exitCode <- system cmd
  if exitCode == ExitSuccess then do
    writeFile fp newStr
    else error $ "Error: " ++ cmd



{-|
    === Search replace words with a list of tuple

    See 'replaceFileListWord' and 'searchReplace' and 'searchReplaceAny'

    @
    > searchReplaceListWord [("dog", "cat")] "mydog dog"
    > "mydog cat"
    @
-}
searchReplaceListWord::[(String, String)] -> String -> String  -- searchReplaceListWord [("dog", "cat")] "mydog dog" => "mydog cat"
searchReplaceListWord [] s = s
searchReplaceListWord (x:xs) s = searchReplaceListWord xs $ searchReplace s (fst x) (snd x)

{-|
    === Search replace words with a list of tuple

    See 'replaceFileListWord' and 'searchReplace' and 'searchReplaceAny'

    @
    > searchReplaceListStr [("dog", "cat")] "mydog dog"
    > "mydog cat"
    @
-}
searchReplaceListStr::[(String, String)] -> String -> String  -- searchReplaceListStr [("dog", "cat")] "mydog dog" => "mydog cat"
searchReplaceListStr [] s = s
searchReplaceListStr (x:xs) s = searchReplaceListStr xs $ searchReplaceAny s (fst x) (snd x)



{-|
    === KEY: replace pattern with a string in all file in a dir

    * See 'replaceFileWithStr'

-}
replaceDirWithStr::String -> String -> FilePath -> IO() -- replaceDirWithStr pat sub dir
replaceDirWithStr pat sub dir = do
  bo <- isDir dir
  if bo then do
    rn <- timeNowSecond
    -- copy dir to /tmp/FileName_1234"
    let cmd = "cp -rf " ++ dir ++ " " ++ "/tmp/" ++ (takeFileName dir) ++ "_" ++ (show rn)
    system cmd
    files <- lsFileFull dir
    mapM_ (\x -> do
              b <- isFile x
              if b then replaceFileWithStr pat sub x
                   else return ()
          ) files
  else error $ "Invalid dir:" ++ dir


{-|
    === KEY: replace string, substitute string

    Use 'subRegex'

    @
    -- Remove all punctuation
    replaceRegex (mkRegex "[[:punct:]]") "[mya,b,c]" ""  ⇒ myabc


    let r1 = mkRegex "google"
    let input = "http://google.com"
    let replacement = "[yahoo]"
    putStrLn $ subRegex r1 input replacement  -- http://[google].com
    @
-}
replaceRegex::Regex -> String -> String -> String  -- replaceRegex (mkRegex "[[:punct:]]") "myabc,,[]" "" ⟹   "myabc"
replaceRegex rx input rep = subRegex rx input rep

{-|
    === KEY: replace pattern inside a file, replace str in file, substitude str in a file

    'replaceByteStringFile'

    @
    bs <- readFileRepPat "src/searchForm.html" "replaceSearchForm" $ toSBS $ optionHtml autoList
    @
-}
readFileRepPat::FilePath -> BS.ByteString -> BS.ByteString -> IO BS.ByteString  -- readFileRepPat f.x pat sub
readFileRepPat = replaceByteStringFile


{-|
    === KEY: replace pattern inside a file, replace str in file, substitude str in a file

    'replaceByteStringFile' 'readFileRepPat'

    @
    bs <- replaceFileWithPat "src/searchForm.html" "replaceSearchForm" $ toSBS $ optionHtml autoList
    @
-}
replaceFileWithPat::FilePath -> BS.ByteString -> BS.ByteString -> IO BS.ByteString  -- replaceFileWithPat f.x pat sub
replaceFileWithPat = replaceByteStringFile


{-|
    === Convert Lazy ByteString to Strict ByteString

    > Data.ByteString => Data.ByteString.Lazy
-}
toStrictBS :: BL.ByteString -> BS.ByteString -- lazy BS to strict BS
toStrictBS = BS.concat . BL.toChunks

{-|
    === Convert Strict ByteString to String

    > strictBSToString = strictTextToStr . strictByteStringToStrictText
-}
strictBSToString = strictTextToStr . strictByteStringToStrictText

{-|
    === Empty Strict ByteString
-}
emptySBS::BS.ByteString
emptySBS = BS.empty

{-|
    === Convert anything to Strict ByteString
-}
toSBS::Typeable a => a -> BS.ByteString
toSBS x = case cast x of
               Just (x::BL.ByteString) -> toStrictBS x      -- Lazy ByteString to Strict ByteString
               _  -> case cast x of
                        Just (x::String) -> s2SBS x   -- String to Strict ByteString
                        _  -> case cast x of
                                 Just (x::TS.Text) -> st2SBS x  -- strict Text To Strict ByteString
                                 _  -> case cast x of
                                       Just (x::TL.Text) -> (lb2SBS . lt2LBS) x  -- lazy Text Text to Strict ByteString
                                       _  -> s2SBS $ showsTypeRep (typeOf x) " toSBS => Invalid Type"
            where
                lb2SBS = lazyByteStringToStrictByteString
                lt2LBS = lazyTextToLazyByteString
                st2SBS = strictTextToStrictByteString
                s2SBS  = strToStrictByteString

{-|
    === Convert anything to Lazy ByteString
-}
toLBS::Typeable a => a -> BL.ByteString
toLBS x = case cast x of
          Just (x::String) -> s2lbs x
          _ -> case cast x of
               Just (x::TL.Text) -> lt2LBS x
               _ -> case cast x of
                    Just (x::TS.Text) -> lt2LBS .  st2lt $ x
                    _ -> case cast x of
                         Just (x::BS.ByteString) -> sbs2lbs x
                         _ -> case cast x of
                              Just (x::BL.ByteString) -> x
                              _ -> s2lbs $ showsTypeRep (typeOf x) " toLBS => Invalid Type"
          where
            s2lbs = strToLazyByteString
            lt2LBS = lazyTextToLazyByteString
            sbs2lbs = strictByteStringToLazyByteString
            st2lt = strictTextToLazyText

{-|
    === Convert String, ByteString, Text to Strict Text

    @
    let st1 = let s = "a"::String in toSText s -- OK
    let st2 = toSText ("a"::String)            -- OK
    let st3 = toSText $ "a"::String            -- Error
    @
-}
toSText::Typeable a => a -> TS.Text -- let s = "str"::String in toSText s, toSText ("a"::String), Error: toSText $ "a"::String => Need annotation
toSText x = case cast x of
            Just (x::String) -> strToStrictText x  -- String to strict Text
            _ -> case cast x of
                 Just (x::TL.Text) -> lt2st x    -- Lazy Text to Strict Text
                 _ -> case cast x of
                      Just (x::BS.ByteString) -> sbs2st x  -- Strict ByteString to Strict Text
                      _ -> case cast x of
                           Just (x::BL.ByteString) ->  sbs2st . lb2SBS $ x
                           _ -> case cast x of
                                Just (x::TS.Text) -> x
                                _ -> strToStrictText $ showsTypeRep (typeOf x) " toSText => Invalid Type"
            where
                lb2SBS = lazyByteStringToStrictByteString
                lt2LBS = lazyTextToLazyByteString
                st2SBS = strictTextToStrictByteString
                s2SBS  = strToStrictByteString
                sbs2st = strictByteStringToStrictText
                lt2st  = lazyTextToStrictText

{-|
    === Convert anything to Lazy Text
-}
toLText::Typeable a => a -> TL.Text
toLText x = case cast x of
            Just (x::String) -> strToLazyText x
            _ -> case cast x of
               Just (x::TS.Text) -> st2lt x
               _ -> case cast x of
                    Just (x::BL.ByteString) -> lbs2lt x
                    _ -> case cast x of
                         Just (x::BS.ByteString) ->  st2lt . sbs2st $ x
                         _ -> case cast x of
                              Just (x::TL.Text) -> x
                              _ -> strToLazyText $ showsTypeRep (typeOf x) " toLText => Invalid Type"
            where
                st2lt = strictTextToLazyText
                lbs2lt = lazyByteStringToLazyText
                sbs2st = strictByteStringToStrictText

{-|
    === Convert anthing to String

    * Lazy Text to String
    * Strict Text to String
    * Lazy ByteString to String
    * Strict ByteString to String
-}
toStr::Typeable a => a -> String
toStr x = case cast x of
          Just (x::BL.ByteString) -> sb2s . toStrictBS $ x
          _ -> case cast x of
               Just (x::BS.ByteString) -> sb2s x
               _ -> case cast x of
                    Just (x::TS.Text) -> st2s x
                    _ -> case cast x of
                         Just (x::TL.Text) -> st2s . lt2st $ x
                         _ -> case cast x of
                              Just (x::String) -> x
                              _   -> showsTypeRep (typeOf x) " toStr => Invalid Type"
          where
              sb2s  = strictBSToString
              lt2st = lazyTextToStrictText
              st2s  = strictTextToStr




{-|
    === can be replaced with following:

    >(++) <$> (Just [1]) <*> (Just [2])
-}
catMaybe::Maybe [a]-> Maybe [a] -> Maybe [a]
catMaybe Nothing Nothing           = Nothing
catMaybe Nothing (Just list)       = Just list
catMaybe (Just list) Nothing       = Just list
catMaybe (Just list1) (Just list2) = Just (list1 ++ list2)

{-|
    === remove white space from string
    See 'trim' 'trimEnd' 'trimStart'

    > removeSpace " a b c " => "abc"
-}
removeSpace::String->String  -- " a b c " => "abc"
removeSpace s = filter(\x -> isSpace x == False) s

-- check if string is empty
isEmpty::String->Bool -- check whether a String is empty or not
isEmpty [] = True
isEmpty _  = False


{-|
   === Escape Html special characters, e.g. only ['<', '>'] currently, remove angle bracket

   Html special characters can be added
   >'<', '>', '\', '%'
-}
escapeHtml::String -> String -- only '<' and '>'
escapeHtml [] = []
escapeHtml (x:cs) = case x of
                 '<' -> "&lt;" ++ (escapeHtml cs)
                 '>' -> "&gt;" ++ (escapeHtml cs)
                 -- '&' -> "&amp;" ++ (es cs)
                 _   -> (x:[]) ++ (escapeHtml cs)


{-|
    === escape amperspand '&' \( \Rightarrow \) '&#38'
    * See 'escapePartial'
    * See 'escapeXML'
-}
escapeAmpersand::String -> String -- only '&'
escapeAmpersand cx = g cx
        where
          g []           = []
          g cx             | (len cx <= 4) = f cx
          g (a:b:c:d:e:cs) | (a == '&') && (b == '#') && (isDigit d) && (isDigit d) && (e == ';') = a:b:c:d:e:(g cs)
                           | (a == '&') = am ++ g (b:c:d:e:cs)
                           | otherwise = a:(g (b:c:d:e:cs))

          f []      | True     = []
          f (x:cx)  | x == '&' = am ++ (f cx)
                    | otherwise = x : (f cx)

          am = "&#38;"

{-|
    === escape '<', '>', '\'', '"'
    * See 'escapeAmpersand'
    * See 'escapeXML'
-}
escapePartial::String -> String -- only '<' and '>' '&', '\''
escapePartial [] = []
escapePartial (x:cs) = case x of
                 '<'  -> "&#60;" ++ (escapePartial cs)
                 '>'  -> "&#62;" ++ (escapePartial cs)
                 '\'' -> "&#39;" ++ (escapePartial cs)
                 '"'  -> "&#34;" ++ (escapePartial cs)
                 _    -> [x]     ++ (escapePartial cs)


{-|
    === escape XML special characters '<', '>', '\'', '"', '&'
    * See 'escapeAmpersand'
    * See 'escapePartial'
-}
escapeXML::String -> String
escapeXML cx = escapeAmpersand $ escapePartial cx

{-|
    === Remove element from a list

    'removeRowCol'

    'principleSubmatrix'

    'insertIndexAt'  ⟹  insert to the next index

    > removeIndex 1 [1,2,3]
    > [1, 3]
    >
    > removeIndex 3 []
    > []
    >
    >
    > removeIndex 1 "abc"
    > "ac"

    @
        removeIndex 100 [1, 2] => [1, 2]
        removeIndex -1  [1, 2] => [1, 2]
    @
-}
removeIndex::Int->[a]->[a]
removeIndex _ [] = []
removeIndex n cx = if n < 0 || n >= len cx then cx else ls
    where
      ls = (fst $ splitAt n cx) ++ (tail $ snd $ splitAt n cx)

removeIndex_new::Integer -> [a] -> [a]
removeIndex_new _ [] = []
removeIndex_new n cx = if n < 0 || n >= len cx then cx else ls
    where
      ls = (fst $ splitAt (fi n) cx) ++ (tail $ snd $ splitAt (fi n) cx)

{-|
    === Remove specific row and column from a given matrix

    > removeRowCol 1 2  -- remove the first row and second column

    'removeIndex'

    'principleSubmatrix'

    \[
         M =\begin{bmatrix}
            1 & 2 & 3 \\
            4 & 5 & 6 \\
            7 & 8 & 9
            \end{bmatrix}
            \rightarrow
            \begin{bmatrix}
            4 & 6 \\
            7 & 9
            \end{bmatrix}
    \]
-}
removeRowCol::Integer->Integer->[[a]]->[[a]]
removeRowCol _ _ [] = []
removeRowCol m n cx = L.transpose $ removeIndex (fromInteger n) $ L.transpose $ removeIndex (fromInteger m) cx

{-|
    === Principle submatrix

    Principle submatrix is a square matrix obtained by removing the same set of rows and columns.

    'removeIndex'

    'removeRowCol'

    > principleSubmatrix 1 mat

    \[
      M = \begin{bmatrix}
        a & b & c \\
        d & e & f \\
        g & h & i \\
        \end{bmatrix}
        \rightarrow
        \begin{bmatrix}
        a & c \\
        g & i \\
        \end{bmatrix}
    \]
-}
principleSubmatrix::Int -> [[a]] -> [[a]]
principleSubmatrix n m =  tran $ removeIndex n $ tran $ removeIndex n m

{-|
    === drop n elements from the end

    >pp $ dropEnd 3 "abcd" == "a"
-}
dropEnd::Integer ->[a] -> [a]
dropEnd n s = take (fromInteger l) s
    where
        l = (len s) - n


{-|
    === take n elements from the end

    >pp $ takeEnd 3 "abcd" == "bcd"
-}
takeEnd::Integer ->[a] -> [a]
takeEnd n s = drop (fromInteger l) s
    where
        l = (len s) - n
{-|
    === 'take' support negative number

    @
      if n >= 0 then take n
      else           takeEnd (-n)
    @
-}
takeX::Integer -> [a] -> [a]  -- n < 0 : takeEnd (-n) else take n
takeX n = if n < 0 then takeEnd n' else take $ fi n
  where
    n' = fi $ negate n

{-|
    === 'drop' support negative number

    @
      if n >= 0 then drop n
      else           dropEnd (-n)
    @
-}
dropX :: Integer -> [a] -> [a]
dropX n = n < 0 ? dropEnd (abs n) $ drop (fi n)

{-|
    === same 'takeWhile'

    from GHC.List
-}
takeWhileX::(a -> Bool) -> [a] -> [a]  -- (\x -> x > 0) [1, 0, 2] => [1], (\x -> x > 0) [0, 1, 2] => []
takeWhileX = takeWhile

{-|
    === take BS.ByteString

    * strict ByteString
    <http://hackage.haskell.org/package/bytestring-0.10.10.0/docs/Data-ByteString.html Data.ByteString>
-}
takeBS::Integer -> BS.ByteString -> BS.ByteString
takeBS n s = BS.take (fromInteger n) s

{-|
    === drop BS.ByteString

    * strict ByteString
    <http://hackage.haskell.org/package/bytestring-0.10.10.0/docs/Data-ByteString.html Data.ByteString>
-}
dropBS::Integer -> BS.ByteString -> BS.ByteString
dropBS n s = BS.drop (fromInteger n) s

{-|
    === split BS.ByteString

    >BS.split (c2w_ 'a') "aXaXa" => ["", "X", "X", ""]
-}
splitBS::DW.Word8  -> BS.ByteString -> [BS.ByteString]
splitBS w s = BS.split w s

{-|
    trim whitespace from the whole string, NOT just head and tail

    see 'trim' head and tail

    >trimWS s   = filter(not . isSpace) s
-}
trimWS::String->String -- trimWS == trimAll => all white space  " a b " => "ab"
trimWS []  = []
trimWS s   = filter(not . isSpace) s

{-|
    trimAll == trimWS, better name:)
-}
trimAll::String->String --  trim all white space " a b " => "ab"
trimAll = trimWS


{-|
    === Trim leading and tailing white space from Data.Text String, trim text, remove white space

    'trim' String

    'trimT' TS.Text

    >trimT = Data.Text.strip

    <http://hackage.haskell.org/package/text-1.2.3.1/docs/Data-Text.html Data.Text>
-}
trimT::TS.Text -> TS.Text
trimT = TS.strip

{-|
    === trimBoth == trim, better name:)
-}
trimBoth::String -> String -- trimBoth == trim
trimBoth = trim

{-|
    === trim lazy ByteString

    * trim leading/trailing whitespace
-}
trimLBS::IN.ByteString -> IN.ByteString
trimLBS s = ss
    where
        bs = LC8.dropWhile isSpace s -- remove leading space
        ss = LC8.foldr(\x y -> if (isSpace x && y == e) then e else LC8.cons x y) e bs   -- remove trailing space
        e  = LC8.empty


{-|
    === Remove whitespace characters from the start of string.

    " dog" => "dog"
-}
trimStart::String -> String
trimStart = dropWhile isSpace

{-|
    === Remove whitespace characters from the end of string.

    >" a b c " => " a b c"
    >"abc"     => "abc"
-}
trimEnd::String -> String
trimEnd s = foldr(\x y -> if (y == [] && isSpace x) then [] else x:y) [] s

{-|
    === split list of string when string is empty/space

    >["dog", " ", "", "cat"] => [["dog"], [], ["cat"]]
-}
splitListEmptyLine::[String]->[[String]] -- ["dog", " ", "", "cat"] => [["dog"], [], ["cat"]]
splitListEmptyLine xs = filter(\x-> length x > 0) $ splitWhen(\x -> (length $ trimWS x) == 0) xs

{-|
    === split list into half

    * split list into half, ["a", "b", "c"] => (["a", "b"], ["c"])
-}
splitListHalf::[String]->([String], [String])  -- split list into half, ["a", "b", "c"] => (["a", "b"], ["c"])
splitListHalf [] = ([], [])
splitListHalf [x] = ([x], [])
splitListHalf (x:y:xs) = (x:xp, y:yp) where (xp, yp) = splitListHalf xs

t1 (a, b, c) = a
t2 (a, b, c) = b
t3 (a, b, c) = c

-- | tuples to list
tuplify2::[a]->(a, a)
tuplify2 [a, b] = (a, b)

tuplify3::[a]->(a, a, a)
tuplify3 [a, b, c] = (a, b, c)

{-|
    === split list 2d with lambda function

    Use 'splitWhen' from 'Data.List.Split.Internals'

    @
    splitListWhen(\x -> x == "b") ["a", "b"] => [["a"], []]
    splitListWhen(\x -> x == "b") ["b", "c"] => [[], ["c"]]
    splitListWhen(\x -> x == "b") ["b"] => [[], []]

    splitListWhen(\x -> x == []) [["a"], ["b"], [], ["c"]]
    [[["a"], ["b"]], [["b"]]]
    @
-}
splitListWhen::(a -> Bool) -> [a] -> [[a]]  -- splitListWhen (\x -> x == "b") ["a", "b", "c"] => [["a"], ["c"]], (\x -> x == "b") ["b"] => [[], []]
splitListWhen f cx = splitWhen f cx



{-|
    === Calculate the length of a curve for *any* functions

    \[ f(x) = x^2  \quad x \in [0, 1] \]

    <http://localhost/image/length_curve_x2.svg  length_of_curve>

    > f x = x*x
    > lengthcurve f (0, 1)
    > 1.4689349039867374

    \[
      \begin{align*}
            & \text{Divide the curve into }  n \text{ parts}                          \\
            &x_i \in \{x_0, x_1, x_2, \cdots, x_{n-1}, x_n\}                          \\
            &f(x_i) \in \{f(x_0), f(x_1), f(x_2), \cdots, f(x_{n-1}), f(x_n) \}       \\
            &y_i \in \{x_0^{2}, x_1^{2}, x_2^{2}, \cdots, x_{n-1}^{2}, x_n^{2} \}     \\
            s &= \sum_{i=1}^{n} \sqrt{(x_{i-1} - x_i)^2 + (y_{i-1} - y_i)^2}          \\
            s_1 &= \sqrt{(x_0 - x_1)^2 + (x_0^{2} - x_1^2)^2}                         \\
            s_2 &= \sqrt{(x_1 - x_1)^2 + (x_1^{2} - x_2^2)^2}                         \\
            \vdots                                                                    \\
            s_{n-1} &= \sqrt{(x_{n-2} - x_{n-1})^2 + (x_{n-2}^{2} - x_{n-1}^2)^2}     \\
            s_n &= \sqrt{(x_{n-1} - x_n)^2 + (x_{n-1}^{2} - x_n^2)^2}                 \\  \\
            s &= \sum_{i=1}^{n} \sqrt{(x_{i-1} - x_i)^2 + (y_{i-1} - y_i)^2}          \\
            s &= \sum_{i=1}^{n} \sqrt{ \Delta x_{i}^2 + \Delta y_{i}^2}        \qquad \\
      \end{align*}
    \]

    * The code is based on <http://localhost/bitbucketHTML/math/length_of_curve.pdf  length_of_curve>
-}
lengthcurve::(Double -> Double) -> (Int, Int) -> Double
lengthcurve f (a, b) = l
    where
       diff = fromIntegral $ b - a
       del = diff/100
       xs = map(\n -> n*del) [1..100]
       ys = map(\x -> f x) xs
       xs1 = init xs
       xs2 = tail xs
       ys1 = init ys
       ys2 = tail ys
       x2 = zipWith(\a1 a2 -> (a1 - a2)**2) xs1 xs2
       y2 = zipWith(\b1 b2 -> (b1 - b2)**2) ys1 ys2
       xy = zipWith(\x y -> sqrt(x + y)) x2 y2
       l = sum xy

{-|
    === Calculate the length of a curve for *any* functions in a given interval

    \[ f(x) = x^2  \quad x \in [0, 1] \]

    > lengthcurveInter f (0, 1) 200
-}
lengthcurveInter::(Double -> Double) -> (Int, Int) -> Int -> Double
lengthcurveInter f (a, b) n = l
    where
       diff = fromIntegral $ b - a
       -- del = diff/n
       del = div' diff n
       xs = map(\n -> (fi n)*del) [1..n]
       ys = map(\x -> f x) xs
       xs1 = init xs
       xs2 = tail xs
       ys1 = init ys
       ys2 = tail ys
       x2 = zipWith(\a1 a2 -> (a1 - a2)**2) xs1 xs2
       y2 = zipWith(\b1 b2 -> (b1 - b2)**2) ys1 ys2
       xy = zipWith(\x y -> sqrt(x + y)) x2 y2
       l = sum xy

{-|
    === KEY: balance bracket, check balanced bracket

    @
     []   => True
     [{}] => True
     [}   => False
     [{]} => False
    @
-}
isBalanced3::String -> String -> Bool
isBalanced3 [] s = len s == 0
isBalanced3 (x:cx) s = case x of
                 var | x == '[' -> isBalanced3 cx ('[':s)
                     | x == ']' -> if head s == '[' then isBalanced3 cx (tail s) else False
                     | x == '{' -> isBalanced3 cx ('{':s)
                     | x == '}' -> if head s == '{' then isBalanced3 cx (tail s) else False
                     | x == '<' -> isBalanced3 cx ('<':s)
                     | x == '>' -> if head s == '<' then isBalanced3 cx (tail s) else False
                     | otherwise -> False


-- balance bracket
isBalanced2::String -> String -> (String, Bool)  -- isBalanced "{}" [] => True
isBalanced2 [] s = (s, length s == 0)
isBalanced2 (x:cs) s = if x == '{' then f cs (x:s) else
                        if x == '}' then (if length s > 0 then f cs $ tail s else (s, False)) else (f cs s)
        where
            f = isBalanced2

{-|
    === Find all bracket '{, }' until they are balanced
    @
    "{{ {    <- 1
         }   <- 2
        }    <- 3
    }        <- 4
    }"       <- 5
    return  => ([(5,"}")],True)
    @

    <file:///Users/cat/myfile/bitbucket/haskell/balanceBracket.hs balanceBracket>
-}
findBalance::[(Integer, String)] -> String -> ([(Integer, String)], Bool)
findBalance [] s = ([], length s == 0)
findBalance ((n, x):cs) s = let fs = fst $ isBalanced2 x s
                                sn = snd $ isBalanced2 x s
                            in if sn == False then findBalance cs fs else (cs, True)


{-|
    === KEY: balance brackets

    @
    pp $ isBalanced "" == True
    pp $ isBalanced "{" == False
    pp $ isBalanced "}" == False
    pp $ isBalanced "{}" == True
    pp $ isBalanced "}{" == False
    pp $ isBalanced "{}{" == False
    pp $ isBalanced "}{}" == False
    @

    <file:///Users/cat/myfile/bitbucket/haskell/balanceBracket.hs balanceBracket>
-}
isBalanced::String -> Bool
isBalanced ss = checkBalance ss []
        where
            checkBalance::String -> String -> Bool  -- checkBalance "{}" [] => True
            checkBalance [] s = length s == 0
            checkBalance (x:cs) s = if x == '{' then f cs (x:s) else
                                    if x == '}' then (if length s > 0 then f cs $ tail s else False) else (f cs s)
                    where
                        f = checkBalance


{-|
    === String to Lazy Text
-}
strToLazyText::String -> TL.Text -- String to Lazy Text
strToLazyText = TL.pack

lazyTextToStr::TL.Text -> String -- Lazy Text to String
lazyTextToStr = TL.unpack

{-|
   === KEYS: string to strict text, str to strict text, str to text

   'String' to 'Data.Text'
 -}
strToStrictText::String -> TS.Text -- String to Strict Text
strToStrictText = TS.pack

{-|
    === Strict Text to String

    KEY: strict text to str, text to string
-}
strictTextToStr::TS.Text -> String
strictTextToStr = TS.unpack

{-|
    === lazy Text to lazy ByteString

    KEY: lazy Text to lazy ByteString
-}
lazyTextToLazyByteString::TL.Text -> BL.ByteString
lazyTextToLazyByteString = TLE.encodeUtf8

lazyByteStringToLazyText::BL.ByteString -> TL.Text
lazyByteStringToLazyText = TLE.decodeUtf8

{-|
    === strict Text to strict ByteString
-}
strictTextToStrictByteString::TS.Text -> BS.ByteString
strictTextToStrictByteString = TSE.encodeUtf8

{-|
    === Convert Strict ByteString to Strict Text
-}
strictByteStringToStrictText::BS.ByteString -> TS.Text
strictByteStringToStrictText = TSE.decodeUtf8


strictByteStringToLazyByteString::BS.ByteString -> BL.ByteString
strictByteStringToLazyByteString = BL.fromStrict

{-|
    === Convert lazy ByteString to strict ByteString
-}
lazyByteStringToStrictByteString::BL.ByteString -> BS.ByteString
lazyByteStringToStrictByteString = BL.toStrict

strictTextToLazyText::TS.Text -> TL.Text
strictTextToLazyText = TL.fromStrict

{-|
    === Lazy Text to Strict Text
-}
lazyTextToStrictText::TL.Text -> TS.Text
lazyTextToStrictText = TL.toStrict

{-|
    === String to Strict ByteString, String to ByteString, str to ByteString

    >strToStrictByteString = strictTextToStrictByteString . strToStrictText
-}
strToStrictByteString::String -> BS.ByteString
strToStrictByteString = strictTextToStrictByteString . strToStrictText


{-|
    === String to lazy ByteString, only for ASCII, not for unicode

    @
    str1 = "好"
    str2 = "good"

    main = do
            pp ((LC8.unpack $ LC8.pack str1) == str1) -- False
            pp ((LC8.unpack $ LC8.pack str2) == str2) -- True
    @
-}
strToLazyByteString::String -> BL.ByteString -- only for ASCII, not for Unicode
strToLazyByteString  = LC8.pack


{-|
    === padding whitespace to 2d list or matrix
-}
ppad m = tran $ map (f) $ tran m
    where
        m' = (map . map) show m
        f n = map(\x -> pad x ma) $ map show n
            where
                ma = foldr(\x y -> max x y) 0 $ map len $ join m'
                pad x n = if length x < n  then pad (x ++ " ") n else x ++ ""


{-|
    === KEY: String to Maybe DA.FromJSON

    See 'toLBS'

    @
    toLBS::Typeable a => a -> BL.ByteString
    @
-}
jsonDecode::(DA.FromJSON a) => String -> Maybe a
jsonDecode = DA.decode . strToLazyByteString


{-|
    === KEY: JSON to record, JSON file to record

    

    @
    SEE: /Users/aaa/myfile/bitbucket/stackproject/JsonAeson

    -- JSON file
    {"editorbeg":100,
     "editorend":200,
     "editorfile":"try919591",
     "editorcmd":"save",
     "editorcode":"Hello World",

     "mycat":{"name":"meowmers",
              "age":1,"list":["dog","cat"]
             }
    }
    --

    data EditorCode = EditorCode{
      editorbeg::Integer,
      editorend::Integer,
      editorfile::String,
      editorcmd::String,
      editorcode::String,
      mycat::Cat
      } deriving (Show, Generic)

    instance DA.ToJSON EditorCode where
      toEncoding = DA.genericToEncoding DA.defaultOptions

    instance DA.FromJSON EditorCode

    data Cat = Cat { name :: Text,
                     age :: Int,
                     list :: [String]
                   } deriving (Show, Generic)

    instance DA.ToJSON Cat where
      toEncoding = DA.genericToEncoding DA.defaultOptions

    instance DA.FromJSON Cat

    meowmers = Cat { name = "meowmers",
                     age = 1,
                     list = ["dog", "cat", "independency Injection"]
                   }

    editCode = EditorCode {
      editorbeg = 100,
      editorend = 200,
      editorfile = "try919591",
      editorcmd = "save",
      editorcode = "Hello World",
      mycat = meowmers
    }

    decodeStr <- jsonToRecord "\/tmp\/json.json" :: IO (Maybe EditorCode)
    case decodeStr of
      Nothing -> Prelude.putStrLn "Not a Valid JSON file"
      (Just x) -> Prelude.putStrLn $ show x
    @
-}
jsonToRecord::(DA.FromJSON a) => FilePath -> IO (Maybe a)
jsonToRecord fp = readFileStr fp >>= \x -> return $ (DA.decode . strToLazyByteString) x



{-|
    === Compile java file

    * Given an absoluate path
    * return and ExitCode

    * The function is used in filewatcher.

    > docExitCode <- compileJava "/Users/cat/myfile/bitbucket/testfile/compileJavaTest.java"
    > case docExitCode of
    >      ExitSuccess -> sys "notify.sh \"gene_javadoc.sh => ExitSuccess\""
    >      ExitFailure x -> sys "notify.sh \"gene_javadoc.sh => ExitFailure\""
-}
compileJava::FilePath -> IO ExitCode
compileJava p = do
        home <- getEnv "HOME"
        pp home
        pwd <- getPwd
        let jar = home </> "myfile/bitbucket/javalib/jar/*"
        pp $ "jar=" ++ jar
        let slash = take 1 p
        let absPath = if slash == "/" then  p else pwd </> p
        let dir = takeDirectory absPath
        let jclass = (dropExtension . takeFileName) absPath
        -- let cmd = "javac -cp " ++ jar ++ ":" ++ dir ++ " " ++ jclass
        let cmd = "javac -cp " ++ jar ++ ":" ++ dir ++ " " ++ absPath
        pp cmd
        exCode <- sys cmd
        return exCode

-- compileJava::FilePath -> IO ExitCode
-- compileJava p = do
--         home <- getEnv "HOME"
--         pwd <- getPwd
--         let jar = home </> "myfile/bitbucket/javalib/jar/*"
--         let dir = takeDirectory p
--         let name = takeFileName p
--         let cmd = "javac -cp " ++ jar ++ ":" ++ dir ++ " " ++ p
--         pp cmd
--         exCode <- sys cmd
--         return exCode



{-|
    === Print matrix with label, print matrix with string,

    >pm "my matrix" mat
    >------------------------------------my matrix-----------------------------------
    >[1,2,3]
    >[4,5,6]
    >[7,8,10]
-}
pm::(Show a)=>String->[[a]]->IO()
pm s x = fw s >> mapM_ print x


{-|
   === print list of string without double quotes

   * rename:  ps => pls (Wed Oct 16 22:18:00 2019)
 -}
pls::[String] -> IO() -- print string without double quotes
pls x = mapM_ putStrLn x


{-|
   === KEY: print string without double quotes
 -}
ps::String -> IO()
ps x = putStrLn x
  
{-|
   === KEY: print matrix, print 2d list

   SEE 'pmat' 'matrixToStr'
   @
   let ls = [[1..4] | _ <- [1..4]]
   printMat ls
   @
 -}
printMat ::(Show a) => [[a]] -> IO()
printMat cx = mapM_ putStrLn $ map concat $ tran lt
  where
   ls = tran $ (map . map) show cx
   ln = map (+1) $ map maxList $ (map . map) len ls
   lt = map (\(n, xs) -> map (\s -> let d = n - len s
                                        p = replicate d ' '
                                    in s ++ p) xs) $ zip ln ls

{-|
   === KEY: print matrix, print 2d list

   SEE 'printMat' 'matrixToStr'
   @
   let ls = [[1..4] | _ <- [1..4]]
   pmat ls
   @
 -}
pmat :: (Show a) => [[a]] -> IO()
pmat = printMat
  
pMat :: (Show a) => [[a]] -> IO()
pMat = printMat  

{-|
   === KEY: matrix to string, 2d list to string, print matrix

   SEE 'printMat'

   @
   > ls = [[1..4] | x <- [1..4]]
   > s = matrixToStr ls
   > mapM_ putStrLn s
     1 2 3 4
     1 2 3 4
     1 2 3 4
     1 2 3 4
   > wfl "/tmp/kk.x" s
   > :!cat /tmp/kk.x
     1 2 3 4
     1 2 3 4
     1 2 3 4
     1 2 3 4
   @
-}
matrixToStr :: (Show a) => [[a]] -> [String]
matrixToStr cx = map concat $ tran lt
  where
   ls = tran $ (map . map) show cx
   ln = map (+1) $ map maxList $ (map . map) len ls
   lt = map (\(n, xs) -> map (\s -> let d = n - len s
                                        p = replicate d ' '
                                    in s ++ p) xs) $ zip ln ls
{-|
    === prefix n tabs

    >psTab 3 "dog"
-}
psTab::Integer -> String -> IO()
psTab n s = putStrLn $ (concatStr (takeN n t) "") ++ s
    where
      t = repeat "\t"


{-|
    === print matrix, show matrix
-}
pa::(Show a)=>[[a]]->IO() -- print matrix, show matrix
pa x = mapM_ print x


{-|
    === KEY: print matrix, print string without double quotes
-}
paa::(Show a)=>[[a]]->IO()
paa x = mapM_ (putStrLn . init . tail . show) x

{-|
 - KEY: list slide, list sliding, window sliding, sliding window
 -
 -  @
    listSlide [a b c d] 2
    > [[a, b], [b, c], [c, d]]

    listSlide [a b c d] 5 
    > []
-}
listSlide :: [a] -> Int -> [[a]]
listSlide cx n = len cx >= n ? (take n cx):listSlide (tail cx) n $ []

{-|
    === Partition list to n chunks

    UPDATE: Thursday, 16 November 2023 14:42 PST
    FIXBUG: partList 0 [1, 2] => in infinite loop 

    __Note__: print partList n [] will cause error since print needs concrete type
  
    split list to n blocks

    @
    >partList 2 [1, 2, 3, 4, 5]
    >[[1,2], [3, 4],[5]]
    
    >partList 2 [1, 2, 3, 4, 5, 6]
    >[[1,2], [3, 4],[5, 6]]

    > partList 0 [1, 2]
    > [[1, 2]]

    > partList -1 [1, 2]
    > [[1, 2]]
    @
-}
partList::Int->[a]->[[a]]
partList _ [] = []
partList n xs = n <= 0 ? [xs] $ ((take n xs):(partList n $ drop n xs))

{-|
    === Partition list to n chunks

    Deprecated:
    Use: 'partListDiff'
-}
partList2_delete ::Int->[a]->[[a]]
partList2_delete _ [] = []
partList2_delete n xs = (take n xs) : (partList n $ drop 1 xs)

{-|
    === Partition list to n chunks

    __Note__: print partList n [] will cause error since print needs concrete type

    split list to n blocks
    @
        partListDiff  1  2  [1, 2, 3, 4]
                      ↑  ↑ 
                      |  + -> block size 
                      | 
                      + -> distance 1

        => [1, 2]
           [2, 3]
           [3, 4]
    @
-}
partListDiff ::Int -> Int->[a]->[[a]]  -- partListDiff 1 2 [1, 2, 3, 4] => [1,2][2,3][3,4]
partListDiff _ _ [] = []
partListDiff d n xs = (take n xs) : (partList n $ drop d xs)


{-|
    === KEY: read line, read input, flush stdout

    @
     hFlush stdout >> getLine
    @
-}
getLineX::IO String
getLineX = hFlush stdout >> getLine

{-|
    === KEY: copy file, copyFile

    'copyFile' from 'System.Directory'
-}
copyFileX::FilePath -> FilePath -> IO()  -- cp
copyFileX = copyFile

{-|
    === KEY: copy file, copyFile

    'copyFile' from 'System.Directory'
    'copyFileX'
-}
cp::FilePath -> FilePath-> IO() -- copyFileX
cp = copyFile

{-|
    === See 'span'
-}
spanA::(a -> Bool) -> [a] -> ([a], [a])
spanA f xs = (takeWhile f xs, dropWhile f xs)

{-|
    === KEY: split string with "[ ]+"
-}
splitSPC::String -> [String]
splitSPC = splitStr "[ ]+"

{-|
    === KEY: split file with Regex e.g. empty line

    NOTE: use better name, splitTable ?
    @
    ls <- readFileToList "/tmp/x.x"
    let lss = splitBlock ls "^[[:space:]]*"  => split file with whitespace
    pp lss

    let lss = splitBlock ls "^[[:space:]]*(---){1,}[[:space:]]*"  => split file "---"
    let lss = splitBlock ls "^[[:space:]]*(===){1,}[[:space:]]*"  => split file "==="

    let lss = splitBlock ["a", " ", "b"] "^[[:space:]]*"  => [["a"], ["b"]]
    let lss = splitBlock ["a", "\n", "b"] "^[[:space:]]*"  => [["a"], ["b"]]
    let lss = splitBlock ["a", "\t", "b"] "^[[:space:]]*"  => [["a"], ["b"]]
    
    -- It works in WhiteSpace
    
    let lss = splitBlock ["a", " ", "b"] "^[[:space:]]*"  => [["a"], ["b"]]
    let lss = splitBlock ["a", "\n", "b"] "^[[:space:]]*"  => [["a"], ["b"]]
    let lss = splitBlock ["a", "\t", "b"] "^[[:space:]]*"  => [["a"], ["b"]]
    @

    NOTE: IT DOES NOT WORK if Empty string in the list. See following.
    @
    let lss = splitBlock ["a", "", "b"] "^[[:space:]]*"  => [["a", "", "b"]]
    @

    <file:///Users/cat/myfile/bitbucket/haskell/splitBlock.hs TestFile>
-}
splitBlock::[String] -> String -> [[String]] -- splitBlock ls "^[[:space:]]*"    split block using empty line
splitBlock [] _ = []
splitBlock cx pat = splitWhen (\x -> matchTest (mkRegex pat) x) cx

{-|
    === KEY: parse file and partition it into blocks according to patterns: bs ws

    >bs = "^[[:space:]]*(---){1,}[[:space:]]*" -- split with delimiter "---"
    >ws = "[,. ]"                              -- split with [,. ]
    >parseFileBlock bs ws ["rat",
    >                      "-----",
    >                      "my dog eats my cat, my cat eats my dog."
    >                     ]
    >[(0, ["rat"]), (1, ["my", "dog", "eats", "my", "cat"])]

    <file:///Users/cat/myfile/bitbucket/haskell/parseRandom.hs Test_File>
-}
parseFileBlock::String->String-> [String] -> [(Integer, [String])]
parseFileBlock _ _ [] = []
parseFileBlock bs ws cx = lt
    where
        bl = filter(\x -> length x > 0) $ splitBlock cx bs
        ls = (map . map) (\x -> splitStrChar ws x) bl
        ds = (map . map ) (\r -> filter(\w -> (len w > 0) && isWord w) r) ls
        sb = map(\x -> unique $ join x) ds --                                = > [["dog", "cat"], ["cow"]]
        lt = zip [0..] sb --                                                 = > [(0, ["dog", "cat"])]


{-|
    === KEY: all prefix strings from given string
-}
prefix::String->[String]
prefix s = filter(\x -> length x > 0) $ map(\n -> take n s) [0..length s]

prefixSuffix::String -> [String]
prefixSuffix s = unique $ pre ++ suf
    where
        pre = prefix s
        suf = filter(\x -> length x > 0) $ map(\n -> drop n s) [0..length s]


{-|
    === substring length k

    >lengthK 2 ["abcd"]
    >["ab", "bc", "cd"]
-}
lengthK::Int -> String -> [String]
lengthK _ [] = []
lengthK n s = if len s >= n then take n s : lengthK n (drop 1 s) else []

{-|
    === substring length from 1 to len s

    >substr 1 "abc"
    >["a", "b", "c", "ab", "bc", "abc"]

-}
substr::Int -> String -> [[String]]
substr _ [] =  []
substr n s = if n <= len s then lengthK n s : substr (n+1) s else []

{-|
    === all substrings length from 1 to len s

    >allSubstr "abcd"
    >[["a","b","c","d"],["ab","bc","cd"],["abc","bcd"],["abcd"]]

-}
allSubstr::String -> [[String]]
allSubstr s = ss
        where
            ss = substr 1 s


{-|
    === unique, remove duplicate elements from a list
    Convert the list to `Set` and convert the `Set` back to `List`
    `unique` does not keep the order of elements

    @
    2 3 4 4 5 3 5 => Remove from the right => 2 3 4 5 3
    2 3 4 4 5 3 5 => Remove from the left  => 2 3 4 3 5
    @

    `uniqueOrder` keeps the order of elements from left \( \Rightarrow \) right
-}
unique::(Ord a)=>[a]->[a]
unique xs = DS.toList $ DS.fromList xs


{-|
    remove duplicated elements and keep the order from left to right

    @
    ["a", "b", "a", "b"] => ["a", "b"]
    s = ["a"]
    s = ["a", "b"]
    "a" `elem` s => ["a", "b"]
    "b" `elem` s => ["a", "b"]
    @

    Recur from the right => left instead of (x:cx)

    The runtime will be \( \color{red}{\mathcal{O}(n^2)} \)

    NOTE: can be better algorithm
-}
uniqueOrder::(Ord a) => [a] -> [a]
uniqueOrder [] = []
uniqueOrder cx = L.reverse $ uniq cx'
        where
            cx' = L.reverse cx
            uniq []     = []
            uniq (x:cs) = if not $ x `elem` uniq cs then x:(uniq cs) else uniq cs


-- length of two lists has to be same
-- [["a"], ["b"]] [["1"], ["2"]] -> [["a", "1"], ["b", "2"], []]
mergeListList::[[String]]->[[String]]->[[String]]
mergeListList [[]] [[y]] = [["", y]]
mergeListList [[x]] [[]] = [[x, ""]]
mergeListList [[]] (y:ys) = ([]:y):ys
mergeListList (x:xs) [[]] = ([]:x):xs
mergeListList (x:xs) (y:ys) = (zipWith(\ex ey -> [ex, ey]) x y) ++ (mergeListList xs ys)

-- [1, 2, 3] [10, 11] => [1, 10, 2, 11, 3]
mergeList::[a]->[a]->[a]
mergeList _ [] = []
mergeList [] _ = []
mergeList (x:xs) (y:ys) = x:y:mergeList xs ys

-- both list has to be the same length
-- ow. return Nothing
mergeListLen::[a]->[a]->Maybe [a]
mergeListLen [] [] = Just []
mergeListLen (x:xs) (y:ys) =
            case mergeListLen xs ys of
            Just merged -> Just (x:y:merged)
            Nothing  -> Nothing
mergeListLen _ _  = Nothing

-- | merge two sorted lists
-- | [1, 4] [3] => [1, 3, 4]
mergeSortList::(Ord a)=>[a]->[a]->[a]
mergeSortList x [] = x
mergeSortList [] x = x
mergeSortList (x:xs) (y:ys) = if x < y then x:(mergeSortList xs (y:ys)) else y:(mergeSortList (x:xs) ys)

-- iterate a list like a for loop or forM_
iterateList::[a]->(a -> IO ()) -> IO ()
iterateList [] f = return ()
iterateList (x:xs) f = f x >> iterateList xs f

unwrap::Maybe a -> a
unwrap Nothing = error "There are some errors, Cruft"
unwrap (Just x) = x

codeCapture::String->String
codeCapture str = subRegex(mkRegex pat) str rep
                    where rep = "<cool>\\1</cool>"
                          pat = "(([^`]|`[^[]]*|\n*)*)"

{-|
    === Binary Search
-}
binarySearch::(Ord a)=>a -> [a]-> Bool
binarySearch k [] = False
binarySearch k cx = if l == 1 then k == head cx
                     else (if k < head re then binarySearch k le
                           else (if k > head re then binarySearch k (tail re) else True))
                    where
                        l = length cx
                        m = div l 2
                        le = take m cx
                        re = drop m cx

{-|
    === quick sort with lambda function

    bad version

    >qqsort(\x y -> len x < len y) ["tiger", "dog"]  => ["dog", "tiger"]
-}
qqsort::(a->a->Bool)->[a]->[a] -- qqsort(\x y -> len x < len y) ["tiger", "dog"]  => ["dog", "tiger"]
qqsort cmp [] = []
qqsort cmp (x:xs) = qqsort cmp ([ e | e <- xs, cmp e x ]) ++ [x] ++ qqsort cmp [ e | e <- xs, not $ cmp e x]

{-|
    === Sort list in nature ordering, e.g. [2, 1] => [1, 2]
-}
sort::(Ord a)=>[a]->[a]
sort (x:xs) = qqsort cmp ([ e | e <- xs, cmp e x ]) ++ [x] ++ qqsort cmp [ e | e <- xs, not $ cmp e x]
    where
        cmp x y = x < y

{-|
    === Quick Sort in Vector, Did not see any speed decreasing
-}
sqVec::(Ord a) => V.Vector a -> V.Vector a
sqVec v = if V.length v == 0 then v
                          else (sqVec $ V.filter(\x -> x < h ) rest) V.++ (V.fromList [h]) V.++ (sqVec $ V.filter(\x -> x > h) rest)
    where
        h = V.head v
        rest = V.tail v

{-|
    === quicksort better version
-}
quickSort::[Int]->[Int]
quickSort [] = []
quickSort [x] = [x]
quickSort l = quickSort(left) ++ [p] ++ quickSort right
                    where
                        left =  [x | x <- init l, x < p]
                        right = [x | x <- init l, x >= p]
                        p = last l

{-|
    === nice version

    * TODO rename it
-}
quickSort'::[Int]->[Int]
quickSort' [] = []
quickSort' (x:xs) = quickSort' ([ l | l <- xs, l < x]) ++ [x] ++ quickSort' ([ r | r <- xs, r >= x])

{-|
    === Use 'quickSortAny', Deprecated

    * note: quickSort1 [] -- get error

    >print quickSort1 ([]::Int)  -- it works
-}
quickSort1::(Ord a)=>[a]->[a]
quickSort1 [] = []
quickSort1 (x:xs) = quickSort1 ([ l | l <- xs, l < x ]) ++ [x] ++ quickSort1 ([ r | r <- xs, r >= x])

{-|
    === Quick Sort for any type, same as quickSort1, just a better name
-}
quickSortAny::(Ord a)=>[a]->[a]
quickSortAny [] = []
quickSortAny (x:xs) = quickSortAny ([ l | l <- xs, l < x ]) ++ [x] ++ quickSortAny ([ r | r <- xs, r >= x])

{-|
    === merge sorted list
-}
mergeSortedList::(Ord a) => [a] -> [a] -> [a]
mergeSortedList [] cy = cy
mergeSortedList cx [] = cx
mergeSortedList cx cy = let h1 = head cx; h2 = head cy in if h1 < h2 then h1 : mergeSortedList (drop 1 cx) cy else h2 : mergeSortedList cx (drop 1 cy)


{-|
    === rotate a list to the right

    >rotateRight 2 [1, 2, 3]
    >[2, 3, 1]
-}
rotateRight::Integer -> [a] -> [a]
rotateRight _ [] = []
rotateRight n cx = let r = snd $ divMod n (len cx) in (takeEnd r cx) ++ (dropEnd r cx)

{-|
    === rotate a 2d matrix to the left

    >rotateLeft2 mat 
-}
rotateLeft2 :: [[a]] -> [[a]]
rotateLeft2 = reverse . tran

{-|
    === rotate a 2d matrix to the right 

    >rotateRight2 mat 
-}
rotateRight2 :: [[a]] -> [[a]]
rotateRight2 = tran . reverse

{-|
    === rotate a list to the left

    >rotateLeft 2 [1, 2, 3]
    >[3, 1, 2]
-}
rotateLeft::Integer -> [a] -> [a]
rotateLeft _ [] = []
rotateLeft n cx = let r = snd $ divMod n (len cx) in (drop' r cx) ++ (take' r cx)


{-|
    === Merge sort for any type

    * TODO rename it
-}
mergeSortC::(a -> a -> Bool)->[a]->[a]
mergeSortC _ []  = []
mergeSortC _ [x] = [x]
mergeSortC f l   = merge f (mergeSortC f left) (mergeSortC f right)
                 where
                    half = (length l) `div` 2
                    left = take half l
                    right= drop half l

                    merge::(a -> a -> Bool)->[a]->[a]->[a]
                    merge f [] r = r
                    merge f l [] = l
                    merge f (x:xs) (y:ys)  = if f x y
                                            then
                                               x:merge f xs (y:ys)
                                            else
                                               y:merge f (x:xs) ys

{-|
    === Merge sort for 'Int'
-}
mergeSort::[Int]->[Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort l = merge(mergeSort left) (mergeSort right)
                where
                    half = (length l) `div` 2
                    left = take half l
                    right= drop half l

                    merge::[Int]->[Int]->[Int]
                    merge [] r = r
                    merge l [] = l
                    merge (x:xs) (y:ys)  = if x < y
                                            then
                                               x:merge xs (y:ys)
                                            else
                                               y:merge (x:xs) ys


{-|
    === Merge sort for matrix, special merge sort

    @
    [0, 1]  [1]         => False
    [0, 1]  [1, 1]      => True
    [1, 1]  [0, 1]      => False
    [0, 1, 0] [1, 0, 0] => True
    @
-}
mergeSortM::(Num a, Eq a)=>[[a]]->[[a]]
mergeSortM [] = []
mergeSortM [x] = [x]
mergeSortM l = merge(mergeSortM left) (mergeSortM right)
                where
                    half = (length l) `div` 2
                    left = take half l
                    right = drop half l

                    merge::(Num a, Eq a)=>[[a]]->[[a]]->[[a]]
                    merge [] r = r
                    merge l [] = l
                    merge (x:xs) (y:ys)  = if not $ moreZero x y
                                            then
                                               x:merge xs (y:ys)
                                            else
                                               y:merge (x:xs) ys

                    -- [0, 1]  [1]         => False
                    -- [0, 1]  [1, 1]      => True
                    -- [1, 1]  [0, 1]      => False
                    -- [0, 1, 0] [1, 0, 0] => True
                    moreZero::(Num a, Eq a)=>[a]->[a]->Bool
                    moreZero [] [] = False
                    moreZero m1 m2 = (len $ takeWhile (== 0) m1) > (len $ takeWhile(== 0) m2)


{-|
    === groupCount == groupBy . sort

    >groupCount ["a", "b", "a"] => [("b", 1), ("a", 2)]
    >groupCount == groupBy . sort

    * See 'groupBy'
-}
groupCount::[String] -> [(String, Integer)] -- groupCount ["a", "b", "a"] => [("b", 1), ("a", 2)]
groupCount [] = []
groupCount cx = m
  where
    n = L.groupBy(\x y -> x == y) $ L.sort cx
    m = qqsort(\x y -> snd x > snd y) $ zip (map head n) (map len n)

{-|
    === Create Symbol link for vim under home

    >.vimrc -> $b/vim/vim.vimrc

    * simplify link file in new system
    * it might not be very useful
-}
vimLink::IO()
vimLink = do
        home <- getEnv "HOME"
        let vimrc = home </> "myfile/bitbucket/vim/vim.vimrc"
        let rename = vimrc + "_old"
        cd home
        bo <- fExist (home </> ".vimrc")
        if | bo -> do
                print $ "Move " + (home </> ".vimrc") + " to /tmp ? yes/no"
                input <- getLine
                if | input == "yes"  -> do
                            sys $ "mv " + vimrc + " " + rename
                            sys $ "ln -s " + vimrc + " .vimrc"
                            return ()
                   | otherwise -> return ()
           | otherwise -> do
                          sys $ "ln -s " + vimrc + " .vimrc"
                          return ()
    where
        (+) = (++)

{-|
    === Watch directory

    * If any file is modified, then return True
    * If a file is added to directory, then return True

    * The delay is one second
-}
watchDir::FilePath -> IO Bool
watchDir p = do
             ls <- lsFileFull p
             prev <- mapM (\x -> getFileStatus x >>= return . modificationTime) ls
             threadDelay 1000000 -- delay one second
             curr <- mapM (\x -> getFileStatus x >>= return . modificationTime) ls
             let orls = zipWith(\x y -> x < y) prev curr
             return $ or orls

{-|
    === KEY: file modification time, timestamp, epoch time in second

    'EpochTime'  \( \Rightarrow \) 'Int64'
-}
fileModTime::FilePath -> IO EpochTime
fileModTime fp = getFileStatus fp >>= return . modificationTime

{-|
    === KEY: file modification time, timestamp, epoch time in second

    KEY: epochtime to int

    'EpochTime' \( \Rightarrow \) 'Int'

    'EpochTime'  \( \Rightarrow \) 'Int64'

    'fromEnum'   'Int64' \( \Rightarrow \) 'Int'
-}
fileModTimeInt::FilePath -> IO Int
fileModTimeInt fp = fileModTime fp >>= return . fromEnum

{-|
    === KEY: file modification time, timestamp, epoch time in second

    'EpochTime'  \( \Rightarrow \) 'Int64'
    'fromIntegral'   'Int' \( \Rightarrow \) 'Integer'
-}
fileModTimeInteger::FilePath -> IO Integer
fileModTimeInteger fp = (fileModTimeInt fp) >>= return . fromIntegral


{-|
    === KEY: convert int to 'CTime'

    Use 'toEnum'

    'EpochTime'  \( \Rightarrow \) 'Int64'
-}
intToCTime::Int -> FCT.CTime
intToCTime = toEnum


{-|
    === return a list of timestamps that any files is modified in a directory

    'EpochTime'  \( \Rightarrow \) 'Int64'
-}
dirModified::FilePath -> IO [EpochTime]
dirModified p = do
                ls <- lsFileFull p
                mapM (\x -> getFileStatus x >>= return . modificationTime) ls

{-|
    KEY: Get file size in byte

    See 'hFileSize'  'openFile' 'ReadMode' 'Handle'
-}
fileSizeA::FilePath -> IO Integer
fileSizeA fp = do
  handle <- openFile fp ReadMode
  hFileSize handle

{-|
    === Read Snippet file $b/snippets/snippet.hs

    * See 'readSnippetStr'
    * <file:///Users/cat/myfile/bitbucket/stackproject/jupyterlab/jupyterlab.html snippet_test.hs>
    * Input file: $b/snippets/snippet.hs

    @
    main:*.hs: my dog, my cat
    my fox eats my rat
    @

    >output:
    >[([head1], [head1_content1]), ([head2], [head2_content2])]
    >[(["main", "*.hs", "my cat", "my dog"], ["main:*.hs: my dog, my cat","my fox eats my rat "])]

    'readFileLatin1ToList'

    > $b/snippet/snippet.hs
    >partition blocktexts into list
    >
    >parse the header => latex_eqq:*.tex: latex equation
    >return [(["latex_eqq", "latex equation"]), ["latex_eqq:*.tex: latex equation", "code block"])]

    TODO: write readSnippet for Data.ByteString ?

    HELP: gx file:///Users/cat/myfile/bitbucket/stackproject/jupyterlab/jupyterlab.html

    <file:///Users/cat/myfile/bitbucket/stackproject/jupyterlab/jupyterlab.html snippet_test.hs>
-}
readSnippet::FilePath->IO [([String], [String])]
readSnippet path = do
            -- list <- readFileToList path;
            list <- readFileLatin1ToList path;
            let ll = filter(\x -> length x > 0) $ splitWhen(\x -> (length $ trim x) == 0) list
            -- let plist = map(\x -> ((splitStrChar "[,:]" $ head x), x) ) ll
            let plist = map(\x -> ((removeIndex 1 $ splitStrChar "[:]" $ head x), x) ) ll
            let pplist = map(\k -> (
                                       -- remove duplicated elem and keey the order
                                       -- L.nubBy (\x y -> x == y) $ foldr(++) [] (map(\x -> map(\r -> trim r) $ splitStrChar "[,]" x) (fst k)),
                                       uniqueOrder $ foldr(++) [] (map(\x -> map(\r -> trim r) $ splitStrChar "[,]" x) (fst k)),

                                       -- NOTE: fix bug, unique does not keep the order of elem
                                       -- unique $ foldr(++) [] (map(\x -> map(\r -> trim r) $ splitStrChar "[,]" x) (fst k)),
                                       snd k
                                   )
                            ) plist
            return pplist

{-|
    === readSnippetStr without any split

    See 'readSnippet'

    >[("key_name:*.tex: latex", "key_name:*.tex: latex\nline 1\n\line2"])]
    >readSnippetStr::FilePath->IO [(header, codeblock)]
-}
readSnippetStr::FilePath->IO [(String, String)]
readSnippetStr path = do
            list <- readFileLatin1ToList path;
            let ll = filter(\x -> length x > 0) $ splitWhen(\x -> (length $ trim x) == 0) list
            let plist = map(\x -> (head x, unlines x) ) ll
            return plist


{-|
    === KEY: Read snippet file, insert into database, snippet to database, snippet to db

    * read snippet file, insert to database sqlite3

    See 'CodeBlock'
    @
    data CodeBlock =
        CodeBlock
        { id        :: Int64
        , header    :: TS.Text
        , codeblock :: TS.Text
        } deriving (Eq, Read, Show)
    @

-}
readSnippetToDatabase::FilePath -> Connection -> IO()
readSnippetToDatabase fp conn = do
                ls <- readSnippetStr fp
                let lsText = map (\x ->  fu strToStrictText x) ls
                execute_ conn create_table
                mapM_ (\x -> do
                    let header = fst x
                    let block = snd x
                    execute conn insert_table (CodeBlock 0 header block 0 0)
                    ) lsText
                return ()
        where
            fu::(a -> b) -> (a, a) -> (b, b)
            fu f (a, b) = (f a, f b)
            -- create_table = Query {fromQuery = toSText "CREATE TABLE IF NOT EXISTS CodeBlock (id INTEGER PRIMARY KEY AUTOINCREMENT, header TEXT, codeblock TEXT)"}
            create_table = Query{fromQuery = strToStrictText "CREATE TABLE IF NOT EXISTS CodeBlock (id INTEGER PRIMARY KEY AUTOINCREMENT, header TEXT, codeblock TEXT, addedtime DATETIME DEFAULT (strftime('%s', 'now')), score INTEGER DEFAULT 0)"}
            -- insert_table = Query {fromQuery = toSText "INSERT INTO CodeBlock (header, codeblock) VALUES (?,?)"}
            insert_table = Query {fromQuery = strToStrictText "INSERT INTO CodeBlock (header, codeblock) VALUES (?,?)"}

--  execute_ conn Query{fromQuery = s2Text "CREATE TABLE IF NOT EXISTS CodeBlock (id INTEGER PRIMARY KEY AUTOINCREMENT, header TEXT, codeblock TEXT, addedtime DATETIME DEFAULT (strftime('%s', 'now')), score INTEGER DEFAULT 0)"}

{-|
    === Query Table CodeBlock and write to a file

    >mapM_ :: Monad m => (a -> m b) -> [a] -> IO()

    <http://localhost/bitbucket_symbol/haskell/writeDatabaseToFile.hs How To Use It>

     @
     import           Database.SQLite.Simple
     import           Database.SQLite.Simple.FromRow
     import           Database.SQLite.Simple.FromField
     import           Database.SQLite.Simple.ToField
     import           Database.SQLite.Simple.Internal
     import           Database.SQLite.Simple.Ok

     codeBlocks <- query_ conn "SELECT id, header, codeblock from CodeBlock" :: IO [CodeBlock]
     let codeList = map (\x -> lines . toStr . codeblock $ x) codeBlocks
     mapM_ (\b -> do
                  writeToFileAppend fp b
           ) codeList
     @
-}
queryDatabaseToFile::FilePath -> Connection -> IO()
queryDatabaseToFile fp conn = do
              codeBlocks <- query_ conn sql_select :: IO [CodeBlock]
              let codeList = map (\x -> lines . toStr . codeblock $ x) codeBlocks
              mapM_ (\b -> do
                        writeToFileAppend fp (b ++ [" "])
                    ) codeList
              return ()
       where
         sql_select = Query {fromQuery = strToStrictText "SELECT id, header, codeblock from CodeBlock ORDER BY id ASC"}

{-|
    KEY: Haskell connect to mysql

    <Users/cat/myfile/bitbucket/haskell/writeDatabaseToFile.hs mysql-haskell>

    >create user 'user1'@'localhost' identified by 'password';    # mysql => create new user
    >GRANT select, update, delete ON *.* TO 'user1'@'localhost';  # grand permission

    >GRANT ALL ON *.* to 'user1'@'localhost';                     # grand all permissions
    >ALTER USER 'user1'@'localhost' IDENTIFIED WITH mysql_native_password BY 'password'   # program can connect to
    >--------------------+
    >mysql> source /tmp/user.file
    >--------------------+

    >sudo mysql -u user1 -p   # password
    >---------------------
    >mysql> CREATE DATABASE testdb   # create database in mysql
    >mysql> USE testdb               # use testdb - database
    >mysql> source mytable.sql       # load table, source table, add table

    @
    DROP TABLE mytable;
    CREATE TABLE mytable(
        id INTEGER NOT NULL AUTO_INCREMENT,
        name TEXT NOT NULL,
        email TEXT NOT NULL,
        PRIMARY KEY(id)
    );
    INSERT INTO mytable(name, email) VALUES("name1", "mail@gmail.com");
    INSERT INTO mytable(name, email) VALUES("name2", "mail@gmail.com");
    INSERT INTO mytable(name, email) VALUES("name3", "mail@gmail.com");
    @
strToStrictByteString
-}
mysqlQuery::IO()
mysqlQuery = do
    conn <- MSQL.connect
        MSQL.defaultConnectInfo {MSQL.ciUser = strToStrictByteString "user1", MSQL.ciPassword = strToStrictByteString "password", MSQL.ciDatabase = strToStrictByteString "testdb"}
    (defs, is) <- MSQL.query_ conn MSQL.Query{ MSQL.fromQuery= strToLazyByteString "SELECT * FROM mytable"}
    pre =<< Streams.toList is



{-|
    === Create a config map for different OS

    Use it to replace 'readConfig'

    @
    config =[
             [("os", "darwin")],
              [
                ("testdb",          "myfile/bitbucket/testfile/test.db"),
                ("webappdb",        "myfile/bitbucket/testfile/haskellwebapp2.db"),
                ("host",            "http://localhost"),
                ("snippetpath",     "myfile/bitbucket/snippets/snippet.hs"),
                ("port",            "8000"),
                ("readSnippetFile", "False")
              ]
            ]
    @
-}
createConfigMap::[[(String, String)]] -> M.HashMap String (M.HashMap String String)
createConfigMap lss = if isTwoList then M.insert osValue configMap M.empty else error $ "createConfigMap => Invalid Input List size len=" ++ (show $ len lss) ++ "Valid List size is 2"
        where
          isTwoList = len lss == 2
          osMap = M.fromList $ head lss
          configMap = M.fromList $ last lss
          osValue = case M.lookup "os" osMap of
                         Just k -> k
                         _      -> error "No OS found"




{-|
    === read file remotely and write to local file

    >readFileRemote "https://some.com/file.txt"  "/tmp/file.xt"
-}
readFileRemote::FilePath -> FilePath -> IO() -- readFileRemote "https://some.com/file.txt"  "/tmp/file.xt"
readFileRemote url fn = CO.simpleHttp url >>= BL.writeFile fn


{-|
    === read file remotely and return IO BL.ByteString

    > bs <- readFileRemoteToList "https://some.com/file.txt"
-}
readFileRemoteToList::FilePath -> IO BL.ByteString
readFileRemoteToList url = CO.simpleHttp url



readFileLatin1::FilePath -> IO String
readFileLatin1 p = do
            h <- openFile p ReadMode
            hSetEncoding h latin1
            contents <- hGetContents h
            return $ contents

{-|
    === Read generic type from a file and conver to two dim list, read table

    * Seperator is Space
    * See 'splitSPC'

    @
    fn = "/tmp/f.x"
    
    /tmp/f.x
    1 2 3
    3 4 5

    ls <- readFile2d fn :: IO[[Integer]]
    pre ls

    fn = "/tmp/f.x"
    
    /tmp/f.x
    0.2 0.3
    0.1 0.5

    ls <- readFile2d fn :: IO[[Float]]
    ls ls
    @

    FIX: trim string before read it
-}
readFile2d::(Read a) => FilePath -> IO [[a]]  -- s <- readFile2d "/tmp/x" :: IO [[Int]]
readFile2d f = do
               list <- readFileLatin1ToList f
               return [[read s | s <- filter (\x -> (len . trim) x > 0) $ splitSPC r] | r <- list]


{-|
    === read two dim matrix from a file, read a list of list Integer from a file

    'readFile2d' for different type

    > p1 = "/Users/cat/myfile/bitbucket/testfile/matrix.txt"
    > lss <- readFileToInteger2d p1
    > pa lss

-}
readFileToInteger2d::FilePath -> IO[[Integer]]
readFileToInteger2d f = do
                   list <- readFileLatin1ToList f
                   let lss = map(\x -> let ls = splitSPC x
                                       in map strToInteger ls
                              ) list
                   return lss

{-|
    === Read dir, all dir or files in [String]
    === There is error when reading some non utf8 or weird char.
    === Deprecated, use 'readFileLatin1ToList'
    > hGetContents: invalid argument (invalid byte sequence)
    >
    >["f1", "f2"]
-}
--readFileToList::FilePath->IO [String]
--readFileToList = (fmap lines) . readFile -- or lines <$> readFile


{-|
    === Emacs Org mod forms a list

    > ["dog", "cat"] => ["|", "dog", "|", "cat", "|"]
-}
orgList::[String] -> [String]
orgList cs = ["|"] ++ (interleave cs $ repeat "|")

{-|
    === Emacs Org mode forms a table

    * | cat | dog |
    * | cow | pig |
-}
orgTable::FilePath -> [[String]] -> IO()
orgTable p cs = mapM_ (\r -> writeToFileAppend p [concat r]) css
    where
        css = map(\r -> orgList r) cs

{-|
    === Write [String] to file

    * write string to file
    <https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:writeFile writeFile>

    * Append string to file
    <https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:appendFile appendFile>
-}
writeToFile::FilePath->[String]->IO()
writeToFile p list = writeFile p $ unlines list

{-|
    === Write BS.ByteString to file

    * writeFileBS used BS.writeFile, nothing else

    >writeFileBS "file.x" (strToStrictByteString "data")

    <http://hackage.haskell.org/package/bytestring-0.10.10.0/docs/Data-ByteString.html writeFile>
-}
writeFileBS::FilePath -> BS.ByteString -> IO() -- writeFile "fname" ByteString
writeFileBS p bs = BS.writeFile p bs


{-|
    === Write [BS.ByteString] to file

    * writeFileListBS uses BS.writeFile and BS.concat, nothing else

    <http://hackage.haskell.org/package/bytestring-0.10.10.0/docs/Data-ByteString.html writeFile>
-}
writeFileListBS::FilePath -> [BS.ByteString] -> IO() -- writeFileListBS "fname" [ByteString]
writeFileListBS p cs = BS.writeFile p (BS.concat cs)



-- {-|
--     === shorthand Write [String] to file
-- -}
-- wfs::FilePath->[String]->IO()
-- wfs p list = writeToFile p list
--
-- wf::FilePath->String->IO()
-- wf = writeFile

{-|
    === append list to file

    TODO: rename it

    Same as 'writeFileListAppend'
-}
writeToFileAppend::FilePath -> [String] -> IO()
writeToFileAppend f list = appendFile f $ unlines list

{-|
    === append list to file

    TODO: rename it

    Same as 'writeToFileAppend'
-}
writeFileListAppend::FilePath -> [String] -> IO()
writeFileListAppend f list = writeToFileAppend f list

{-|
    === Matrix: Integer to Fractional, convert integer matrix to Fractional matrix

    see 'nToNumMat'

    >class (Num a)=>Fractional a where
    >class (Num a, Ord a)=> Real where
    >class (Real a, Enum a)=> Integral where
-}
nToFractMat::(Real a, Fractional b)=>[[a]]->[[b]]
nToFractMat = (map . map) realToFrac

{-|
    === Conver 'Integer' to 'Num'
-}
toNum::(Num a)=> Integer -> a
toNum = fromInteger

{-|
    === Convert an Integer matrix to an Num Matrix

    see 'nToFractMat'
-}
nToNumMat m = (map . map) fromInteger m

{-|
    === Matrix: Real to Rational

    see 'nToFractMat', 'nToNumMat'

    >data Rational = Ratio Integer
-}
rToRatMat::(Real a)=>[[a]] -> [[Rational]]
rToRatMat = (map. map) toRational

{-|
    === Write an 2d Matrix to file, write an 2d list to file, write matrix to file

    see 'writeToFileMat'

    \[
        \begin{bmatrix}
        1 & 2 \\
        3 & 4 \\
        \end{bmatrix} \Rightarrow
        \begin{matrix}
        1 & 2 \\
        3 & 4 \\
        \end{matrix}
    \]

    > m = [[1, 2], [3, 4]]
    > writeToFile2dMat "/tmp/m.x" m
    > :!cat /tmp/m.x
-}
writeToFile2dMat::(Num a, Fractional a, Show a)=>FilePath->[[a]]->IO()
writeToFile2dMat p m2 = writeToFile p $ map(\r -> foldr(\x y -> x ++ " " ++ y) [] r) $ ppad m2

{-|
    === write String matrix to file

    see 'writeToFile2dMat'
-}
writeToFileMat::FilePath->[[String]]->IO()
writeToFileMat p m2 = writeToFile p $ map(\r -> foldr(\x y -> x ++ " " ++ y) [] r) $ ppad m2

{-|
    === KEY: filter and map simultaneous

    >filtermap(\x -> elem x ['a', 'e', 'i', 'o', 'u', 'y'] then Nothing else Just (toUpper x)) "abc"
    >remove all the vowels and toUpper => "BC"

    Prepend an element to a list if available.  Leave the list as it is if the
    first argument is Nothing.
    Variant of map which deletes elements if the map function returns Nothing.

    https://snipplr.com/view/59474/simultaneous-filter-and--map/
-}
filtermap :: (a -> Maybe b) -> [a] -> [b]
filtermap _ [] = []
filtermap f (a:as) = maybecons (f a) $ filtermap f as
        where
            maybecons :: Maybe t -> [t] -> [t]
            maybecons Nothing l = l
            maybecons (Just e) l = e : l

putStrLnBS::BS.ByteString -> IO()
putStrLnBS = S8.putStrLn

putStrBS::BS.ByteString -> IO()
putStrBS = S8.putStr


{-|
    === Backward substitute

    Given an upper triangle matrix \(A\) and a vector \(\vec{b}\), solve for \(x\)

    <http://localhost/pdf/backwardsubstitute.pdf backward_substitute>

    \[
        Ax = b
    \]
    \[
        A = \begin{bmatrix}
            1 & 2 \\
            0 & 4 \\
            \end{bmatrix}
            \begin{bmatrix}
            x_1 \\
            x_2
            \end{bmatrix} =
            \begin{bmatrix}
            b_1 \\
            b_2
            \end{bmatrix}
    \]
-}
bs::[[Double]] -> [Double] -> [Double] -> [Double]
bs []  _  _        = []
bs _   _  []       = []
bs m x b = f (reverse m) x (reverse b)
    where
        f []  _  _        = []
        f _   _  []       = []
        f (r:xa) x (b:xb) = let x' = bs' r x b in x':(f xa (x':x) xb)
            where
                bs' r x b = xi
                    where
                        s = sum $ (init $ reverse r) * x -- a[i][1]x[1] + a[i][2]x[2]
                        a = last $ reverse r
                        xi = (b - s)/a  --  x_i = (b_ii - sum ())/ a_i

{-|
    === forward substitute

    Given a lower triangle matrix \(A\) and a vector \(\vec{b}\), solve for \(x\)

    <http://localhost/pdf/backwardsubstitute.pdf backward_substitute>

    \[
        Ax = b
    \]
    \[
        A = \begin{bmatrix}
            1 & 0 \\
            2 & 4 \\
            \end{bmatrix}
            \begin{bmatrix}
            x_1 \\
            x_2
            \end{bmatrix} =
            \begin{bmatrix}
            b_1 \\
            b_2
            \end{bmatrix}
    \]
-}
fs::[[Double]] -> [Double] -> [Double] -> [Double]
fs []  _  _        = []
fs _   _  []       = []
fs m x b = f (id m) x (id b)
    where
        f []  _  _        = []
        f _   _  []       = []
        f (r:xa) x (b:xb) = let x' = fs' r x b in x':(f xa (x':x) xb)
            where
                fs' r x b = xi
                    where
                        s = sum $ (init $ id r) * x -- a[i][1]x[1] + a[i][2]x[2]
                        a = last $ id r
                        xi = (b - s)/a  --  x_i = (b_ii - sum ())/ a_i


-- dot product list
listDot::(Num a)=>[a]->[a]->a
listDot [] y = 0
listDot x [] = 0
listDot x y = sum $ zipWith(*) x y

listDots::(Num a)=>[[a]]->[[a]]->a
listDots x y = (sum . join) $ (zipWith . zipWith)(*) x y

-- multiply scale and list
scaleList::(Num a)=>a -> [a] -> [a]
scaleList _ [] = []
scaleList c v  = map(\x -> c*x) v

listScale::(Num a)=>[a] -> a -> [a]
listScale [] _ = []
listScale v  c = map(\x -> c*x) v

-- add two lists
listAdd::(Num a)=>[a]->[a]->[a]
listAdd [] _ = []
listAdd _ [] = []
listAdd u v  = zipWith(\x y -> x+y) u v

listSub::(Num a)=>[a]->[a]->[a]
listSub [] _ = []
listSub _ [] = []
listSub u v  = zipWith(\x y -> x - y) u v

listMul::(Num a)=>[a]->[a]->[a]
listMul [] _ = []
listMul _ [] = []
listMul u v  = zipWith(\x y -> x * y) u v

--listDiv::[Integer]->[Integer]->[a]
--listDiv [] _ = []
--listDiv _ [] = []
--listDiv u v  = zipWith(\x y -> (realToFrac x)/ (realToFrac y)) u v

listNeg::(Num a)=>[a]->[a]
listNeg [] = []
listNeg u  = map(\x -> -x) u


{-|
    === Find the dimension of a matrix -> (nrow, ncol)
    > dim [] => (0, 0)

    NOTE: there is bug
    >dim [1]
-}
dim::[[a]]->(Int, Int)
dim  [] = (0, 0)
dim  (x:xs) = (length (x:xs), length x)

{-|
    === zipWith two mutable array

    TODO: change Int to generic type with forall a. ?
    Problem: when you initialize the array, you need a concrete type such as Int, Float
    Solution: constrain to (Num a) type
-}
zipWithArr::(Num a)=>(a -> a -> a) -> IOArray Int a -> IOArray Int a  -> IO(IOArray Int a)
zipWithArr f iarr1 iarr2 = do
                            bound <- getBounds iarr1
                            narr <- newArray bound 0
                            let b1 = fst bound
                            let b2 = snd bound
                            mapM (\x -> do
                                           e1 <- readArray iarr1 x
                                           e2 <- readArray iarr2 x
                                           writeArray narr x (f e1 e2)
                                  ) [b1..b2]
                            return narr

{-|
    === zipWith two mutable array, and convert to list

    TODO: change Int to generic type with forall a. ?
    Problem: when you initialize the array, you need a concrete type such as Int, Float
    Solution: constrain to (Num a) type
-}
zipWithArrToList::(Num a)=>(a -> a -> a) -> IOArray Int a -> IOArray Int a  -> IO[a]
zipWithArrToList f iarr1 iarr2 = zipWithArr f iarr1 iarr2 >>= getElems




{-|
    === Use co-factor expantion to find a determinant of n by n matrix.

    * co-factor expantion
    * It is very slow.
    * Note: \( \det M = \det M^{T} \)
    * NOTE: the function should not been used for 'Floating' type

    <http://localhost/html/indexMathDefinition.html#prove_determinant Proof>
-}
det::(Num a)=>[[a]] -> a
det [[x]] = x
det m = sum $ zipWith(\x y -> let a = fst x; b = snd x in (-1)^a*b*(det y)) c' co
        where
            row = head m
            len = length m
            len' = toInteger len
            c   = [0..len']
            c'  = zipWith(\x y -> (x, y)) c row
            lm  = replicate len m
            co  = zipWith(\x m -> removeRowCol 0 x m) c lm

-- | matrix multiplicaiton in Int
multiMatInt::[[Int]]->[[Int]]->[[Int]]
multiMatInt a b = [ [sum $ zipWith (*) ar bc | bc <- (L.transpose b)]  | ar <- a]

{-|
  KEY: matrix multiply a column vector

  NOTE: return a column-matrix, the vector is in the first column

  INPUT: column-matrix
  RETURN: column-matix

  @
    -- treat 'v' as column vector
    fw "mat"
    printMat mat
    v = [1, 2, 3]
    fw "v"
    v
    m1 = mat `multiVec` v
    fw "m1"
    printMat m1
  @
-}
multiVec ::(Num a) => [[a]] -> [a] -> [[a]]
multiVec m v = tran $ [map (\r -> sum $ r * v) m]

multiVecL :: (Num a) => [[a]] -> [a] -> [a]
multiVecL m v = map (\r -> sum $ r * v) m
  
  
{--  
multiVec m v = getColumn (multiMat m mm) 1
            where
              gm mx n = replicate mx $ replicate n 0
              vc = tran [v]
              nRow = fst $ dim m 
              nCol = snd $ dim m 
              b1 = nRow == nCol && len v == nRow 
              m1 = gm nRow (nCol - 1) 
              mm = zipWith (++) vc m1 
--}
  
{-|
    === Matrix multiplication in String

    @
    m1 =
    ["a","b"]
    ["c","d"]
    m2 =
    ["x","y"]
    ["z","w"]

    > mapM_ print $ multiMatStr m1 m2
    ["axbz","aybw"]
    ["cxdz","cydw"]

    > mapM_ print m1
    ["a","b"]
    ["c","d"]

    > mapM_ print m2
    ["x","y"]
    ["z","w"]

    > pmat mat
    1 2 3
    4 5 6
    7 8 10
    > pmat $ tran mat
    1 4 7
    2 5 8
    3 6 10
    > pmat $ (map . map) (\r -> concatStr r "+") $ (out . zipWith) (\a b -> show a ++ "x" ++ show b) mat $ tran mat
    "1x1+2x4+3x7"  "1x2+2x5+3x8"  "1x3+2x6+3x10"
    "4x1+5x4+6x7"  "4x2+5x5+6x8"  "4x3+5x6+6x10"
    "7x1+8x4+10x7" "7x2+8x5+10x8" "7x3+8x6+10x10"
    >
    @
-}
multiMatStr::[[String]] -> [[String]] -> [[String]]
multiMatStr sx yx = (map . map) (\(x, y) -> dotStr x y) $ cart sx yx
  where
    dotStr cx cy = concat $ zipWith(++) cx cy  -- ["a", "b"] ["x", "y"] => ["ax" "by"] => "axby"

{-|
  @
  cart::[[a]] ->[[a]] -> [[([a], [a])]]
  cart cx cy = tran $ [[(x, y) | x <- cx] | y <- tran cy]
  @
-}
cart::[[a]] ->[[a]] -> [[([a], [a])]]
cart cx cy = tran $ [[(x, y) | x <- cx] | y <- tran cy]


-- | matrix multiplication in Double
-- multiMatDouble::[[Double]]->[[Double]]->[[Double]]
-- multiMatDouble a b = [ [sum $ zipWith (*) ar bc | bc <- (L.transpose b)]  | ar <- a]

multiMatDouble::[[Double]] -> [[Double]] -> [[Double]]
multiMatDouble cx cy = (map . map) (\(x, y) -> dot x y) $ cart cx cy
  where
    dot u v = sum $ zipWith(*) u v

--multiMatVec::(Num a)=>[[a]]->[a]->[a]
--multiMatVec m v = map(\r -> listDot r v) m

{-|
  matrix multiplication in Rational Number

  @
  > pmat matr
  1 % 1 2 % 1 3 % 1
  4 % 1 5 % 1 6 % 1
  7 % 1 8 % 1 10 % 1

  > pmat $ multiMatR matr matr
  30 % 1  36 % 1  45 % 1
  66 % 1  81 % 1  102 % 1
  109 % 1 134 % 1 169 % 1
  @

-}
multiMatR::[[Rational]]->[[Rational]]->[[Rational]]
multiMatR mat1 mat2 = L.transpose $ map(\m2 -> map(\m1 -> sum $ zipWith(*) m1 m2) mat1) $ L.transpose mat2

multiMatNum::(Num a)=>[[a]] -> [[a]] ->[[a]]
multiMatNum cx cy = (map . map) (\(x, y) -> dot x y) $ cart cx cy
  where
    dot u v = sum $ zipWith(*) u v

-- mat ∘ mat
-- (∘) = multiMat

{-|
    === KEY: Use outer product to compute matrix multiplication.

   \[
     \begin{bmatrix}
         1 & 2 & 3 \\
         4 & 5 & 6 \\
         7 & 8 & 9
     \end{bmatrix}
   \]


   * use outer product to compute matrix mulitplication
   * outer(col_1  row_1) + outer(col_2  row_2) + outer(col_3 row_3)

   * matrix multiply vector

   @
   -- column vector
   [[1],
    [2],
    [3]]
   -- row vector
    [[1, 2, 3]]

   -- matrix * vector

   -- matrix m
   m = [
        [1, 2, 3],
        [4, 5, 6],
        [7, 8, 9]
       ]
   -- vector V
     [a,
      b, 
      c]
    to matrix form
    v = [
         [a, 0, 0],
         [b, 0, 0],
         [c, 0, 0]
        ]
    matrix multiply vector
    m * v
   @

   \[
     \begin{bmatrix}
         1 & 2 & 3 \\
         4 & 5 & 6 \\
         7 & 8 & 9 \\
     \end{bmatrix}
     \times
     \begin{bmatrix}
         1 \\
         2 \\
         3 \\
     \end{bmatrix} \\
     Can be written as following
     \begin{bmatrix}
         1 & 2 & 3 \\
         4 & 5 & 6 \\
         7 & 8 & 9 \\
     \end{bmatrix}
     \times
     \begin{bmatrix}
         1 & 0 & 0\\
         2 & 0 & 0\\
         3 & 0 & 0\\
     \end{bmatrix}
   \]
-}
multiMat::(Num a)=>[[a]]->[[a]]->[[a]]
multiMat mat1 mat2= foldr(\x y ->x + y) mz ma
                where
                    -- a list of matriess
                    mz = matZero $ len mat1 
                    ma = zipWith(\x y -> outer x y) (L.transpose mat1) mat2

matDiv::[[Integer]]->Integer->[[Integer]]
matDiv m n = map(\x -> map(\y -> div y n) x) m

-- | matrix multiplication in String/Rational
multiRatMat::[[String]]->[[String]]->[[String]]
multiRatMat a b = [[sumRatList $ zipWith (multR) ar bc | bc <- (L.transpose b)]  | ar <- a]

sumRatList::[String]->String
sumRatList cx = foldl(addR) "0/1" cx


-- add rational numbers as String
addR::String->String->String
addR s1 s2 = cx
            where
                n1 = splitR s1
                n2 = splitR s2
                nn1 = map(\x -> x*(last n2)) n1
                nn2 = map(\x -> x*(last n1)) n2
                nn = ((head nn1) + (head nn2)):(tail nn1)
                list = reduce nn
                cx = show (head list) ++ "/" ++ show (last list)

{-|
   === KEY: identity matrix

   @
   matId 3
   out (\a b -> a == b ? 1 $ 0) [1..3] [1..3]
   @
-}
matId ::(Num a) => Int -> [[a]]
matId n = out (\a b -> a == b ? 1 $ 0) [1..n] [1..n]

{-|
   === KEY: zero matrix

   @
   matId 3
   out (\_ _ -> 0) [1..3] [1..3]
   @
-}
matZero :: (Num a) => Int -> [[a]]
matZero n = out (\_ _ -> 0) [1..n] [1..n]


{-|
    === Convert Rational to Integer:
    >>> ratToInt "3/1"
    3
-}
ratToInt::String->String
ratToInt s = if dn == 1 then show nu else s
        where
            xs = splitR s
            nu = head xs
            dn = last xs
{-|
    === 'normalR' -3/-1 = 3/1, 3/-2 => -3/2
-}
normalR::String->String
normalR s = ss
        where
            xs = splitR s
            nu = head xs
            dn = last xs
            ss = if nu < 0 && dn < 0 then show (-nu) ++ "/" ++ show (-dn)
                    else (if nu > 0 && dn < 0 then show (-nu) ++ "/" ++ show (-dn) else s)

{-|
    === 'reduceForm' list of rational
-}
reduceForm::[[String]]->[[String]]
reduceForm m = map(\r -> map (ratToInt . normalR) r) m

{-|
    === reduce numerator and denominator:
    >>> reduce 6/2
    3/1
-}
reduce::[Integer]->[Integer]
reduce cx = cx'
            where
                d = gcd (head cx) (last cx)
                cx' = map(\x -> x `quot` d) cx




{-|
    === Convert string to Float, string to Double

    * 'strToF'

    >stringToFloat "123"
    >123
    >
    >stringToFloat "123.3"
    >123.3
    >
    >stringToFloat "a"
    >error

    * The Read class
    <https://hackage.haskell.org/package/base-4.12.0.0/docs/Text-Read.html#v:readPrec Text.Read>
-}
stringToFloat::String -> Float
stringToFloat s = read s::Float

strToF = stringToFloat


{-|
    === Convert string to a list of 'Int'

    >stringToListInt "[1, 2]"
    >[1, 2]
    >
    >stringToListInt "[1..3]"
    >error

    <https://hackage.haskell.org/package/base-4.12.0.0/docs/Text-Read.html#v:readPrec Text.Read>
-}
stringToListInt::String -> [Int]
stringToListInt s = read s::[Int]


{-|
    === split first word by delimiter: isSpace

    >split "my dog eats my cat"
    >("my", "dog eats my cat")

    <https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/8_Parser parser>
-}
word::String -> (String, String)
word [] = ([], [])
word (c:cs) | isSpace c = ([], cs)
            | otherwise = let (c', cs') = word cs
                          in  (c:c', cs')

{-|
    === split all words with delimiter: isSpace

    <https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/8_Parser parser>

    >print $ sentence "dog cat 133"
    >["dog", "cat", "133"]
-}
sentence::String -> [String]
sentence [] = []
sentence str = let (w, cs) = word str
               in w:sentence cs

{-|
    === Check a string whether it is a word or not

    > Word is defined as  "^[[:alpha:]]+$"
    >isWord "dog"
    >True
    >
    >isWord "dog1"
    >False
    >
    >isWord::String -> Bool
    >isWord s = matchTest (mkRegex "^[[:alpha:]]+$") s
-}
isWord::String -> Bool
isWord s = matchTest (mkRegex "^[[:alpha:]]+$") s

{-|
    === Check a string whether it is a word or not

    > Word should be defined as [a-zA-Z0-9_] according to vim doc :h pattern
-}
isInWord :: Char -> Bool
isInWord c = n `elem` s
  where
    s1 = [ord 'a' .. ord 'z']
    s2 = [ord 'A' .. ord 'Z']
    s3 = [ord '0' .. ord '9']
    s4 = [ord '_']
    s = s1 ++ s2 ++ s3 ++ s4
    n = ord c


{-|
    === Check whether String contain Word char
    @
      Word = [a-zA-Z0-9_]
      hasWordChar "abc_efg"  -- True
      hasWordChar "abc-"     -- False
    @
-}
hasWordChar :: String -> Bool
hasWordChar [] = False
hasWordChar s = b
  where
    ls = map (\c -> isInWord c) s
    b = foldl (\a b -> a && b) True ls

{-|
    === Non negative integer
    * \( 0, 1, 2, \cdot \)
-}
isZeroPos::String -> Bool
isZeroPos s = matchTest (mkRegex "(^[1-9]+[0-9]*$)|(^0$)") s

{-|
    === Permutation of any type, the algo is based on prefix string

    <http://localhost/image/permtree1.svg permutation_tree>

    @
    fun (prefix, int index, removeIndex(index str))
    @

    > perm "12"
    >[["12", "21"]]

    TODO: fix, the code has error.
-}
perm::[a] -> [[[a]]]
perm [] = []
perm cx = map (\i -> pp i cx) [0..(length cx) - 1]
    where
        -- p 1 "dog" => "odg"
        -- p i s = (s !! i) : (removeIndex i s)
        move i s = (s !! i) : (removeIndex i s)
        --
        --
        -- a b c
        -- pp 0 abc => abc
        -- pp 1 abc => bac
        -- pp 2 abc => cab
        pp i s = if i < len then [move i s] ++ (pp (i+1) s) else []
            where
                len = length s

{-|
    === Permutation of any type

    The code is based on prefix string algorithm

    > perm "12"
    >[["12", "21"]]

    @
        abc
        a p(bc)
           b p(c)
             c p()
           c p(b)
             b p()
        b p(ac)
          a p(c)
            c p()
          c p(a)
            a p()
        c p(ab)
          a p(b)
            b p()
          b p(a)
            a p()
    @
    @
        abc
            a   bc
                b c
                    [[]]
                c b
                    [[]]

            b   ac
                a c
                    [[]]
                c a
                    [[]]

            c   ab
                a b
                    [[]]
                b a
                    [[]]
    @

-}
perm2::(Eq a)=>[a] -> [[a]]
perm2 [] = [[]]
perm2 cx = [ pre:r | pre <- cx, r <- perm2 $ filter(\x -> x /= pre) cx]


{-|
    === KEY: facebook interview question

    * find all the list that in increasing order, and the length is n

    > rangeNum 3 [1, 2, 3, 4]
    > [1, 2, 3], [2, 3, 4], [1, 2, 4]
-}
rangeNum::(Eq a)=>Integer -> [a] -> [[a]]
rangeNum _ [] = [[]]
rangeNum k cx = [(fst pre):r | pre <- tup, r <- rangeNum k $ drop ((snd pre) + 1) cx]
        where
            tup = zip cx [0..]

{-|
    === And other Permutation

    <https://stackoverflow.com/questions/40097116/get-all-permutations-of-a-list-in-haskell SO>


      [1, 2, 3]

      x :  xs
      1  [2,3]
         x : xs
         2   [3]
             x : xs
             3   []

      perm([1, 2, 3])
        1 : perm([2, 3])
              2 : perm([3])
                    3: perm([])

      [1 2 3]  [2 3 1]  [3 1 2]
       x : cx  x:cx      x : cx
       1:[2,3] 2:[3,1]   3:[1,2]
       1:[3,2] 2:[1,3]   3:[2,1]
                  
-}
perm3 :: [a] -> [[a]]
perm3 []        = [[]]
perm3 ls@(x:xs) = concatMap ((rotations len).(x:)) (perm3 xs)
                  where len = length ls

                        -- iterate (\(y:ys) -> ys ++ [y]) [1, 2, 3]
                        -- [1, 2, 3], [2, 3, 1], [3, 1, 2] ...
                        rotations :: Int -> [a] -> [[a]]
                        rotations len xs = take len (iterate (\(y:ys) -> ys ++ [y]) xs)



splitR::String->[Integer]
splitR s = xx'
            where
                xs = splitRegex(mkRegex "/") s
                -- it is integer only:"3" => "3/1"
                xs' = if length xs == 1 then xs ++ ["1"] else xs
                xx = map(\x -> read x::Integer) xs'
                xx' = case last xx of
                        0 -> error "denominator cannot be zero"
                        _ -> xx

multR::String->String->String
multR s1 s2 = ss
            where
                n1 = splitR s1
                n2 = splitR s2
                nn = zipWith(\x y-> x*y) n1 n2
                cx = reduce nn
                ss = show (head cx) ++ "/" ++ show (last cx)

-- "3" ["1, "2"] => ["3", "6"]
multRL::String->[String]->[String]
multRL s cx = map(\x -> multR s x) cx

normR::[String]->[String] ->String
normR [] _ = []
normR _ [] = []
normR x y = foldl(addR) "0/1" $ zipWith(\x y -> multR x y) x y

getVec::Int->[[a]]->[[a]]
getVec n m = map(\x -> [x]) $ map(\r -> r !! n) m

{-|
  === Normalize a list as vector
  @
  [[1, 2, 3]]     is row vector
  [[1], [2], [3]] is column vector
  @
 \[
  \| \vec{v} \| = \sqrt{x^2 + y^2 + z^2} \quad \text{ where }
  \vec{v} = \begin{bmatrix}
  x & y & z
  \end{bmatrix}
 \]
-}
normList::(Floating a)=>[[a]] -> [[a]]
normList m = (map . map) (/n) m
        where
            n = sqrt $ sum $ join $ (zipWith . zipWith) (*) m m

-- u project on w
proj::[String]->[String]->[String]
proj w u = multRL (divR (normR w u) (normR w w)) w

--projm::[[Rational]]->[[Rational]]->[[Rational]]
--projm c r = multiMat c r
--        where
--            m = multiMat c r
--            n2= multiMat c (tran r)
--            m2 = map(\x ->map(\y -> y*y) x) m



-- | projection from u onto v in n dimensional list
-- |
-- |   puv = (u v /<v, v>) v
-- |
-- |
projn::(Fractional a)=>[[a]]->[[a]]->[[a]]
projn u v = c `mu` v
  where
   dot w k = (sum . join) $ (zipWith . zipWith) (*) w k
   d      = dot u v
   c      = d/(dot v v)
   mu a w = (map . map)(*a) w

{-|
    === Form a almost R matrix: almost an upper triangle matrix

    <http://localhost/pdf/gram_schmidt.pdf QR_Decomposition>
    \[
       R' = \begin{bmatrix}
            \left<e_1, v_1 \right> & \left<e_1, v_2 \right> & \left< e_1, v_3 \right> \\
            \emptyset              & \left<e_2, v_2 \right> & \left< e_2, v_3 \right> \\
            \emptyset              & \emptyset              & \left< e_3, v_3 \right>
            \end{bmatrix} \quad
       R = \begin{bmatrix}
            \left<e_1, v_1 \right> & \left<e_1, v_2 \right> & \left< e_1, v_3 \right> \\
            0                      & \left<e_2, v_2 \right> & \left< e_2, v_3 \right> \\
            0                      & 0                      & \left< e_3, v_3 \right>
            \end{bmatrix} \\
       R' \neq R
    \]
-}
rMatrixUpperTri ex vx = (zipWith . zipWith) (\e v -> listDots e v) ex' vx'
    where
        ex' = liste ex
        vx' = listv vx
        liste cx  = zipWith(\n v -> replicate n v) (reverse [1..len cx]) cx
        listv cx  = map(\n -> drop n cx) [0..(len cx - 1)]

{-|
    === rejection \(v_k\) onto span of \( \{ a_1 \dots a_{k-1} \} \)
    <http://localhost/pdf/gram_schmidt.pdf QR_Decomposition>
    \[
        a_k = v_k - \sum_{i=1}^{k-1} \frac{\left<v_k, a_i \right>}{\left<a_i, a_i \right> } a_i
    \]

    * v3 projects onto \( \{a_1, a_2 \} \)

    > a3 = v3 - ((projn v3 a1) + (projn v3 a2))

    * if the matrix is singular then one of \( \left\| a_k \right\| = 0 \)
    * that can be used to determinate the determinant of a matrix, but not the sign of a determinant
    * compute the determiant is hard problem: \( \mathcal{O}(n^3) \), there is better algo.
-}
rejection::(Fractional a)=>[[[a]]] -> [[[a]]] -> [[[a]]]
rejection  an [] = an
rejection  an (vv:cx) = rejection ([vv - (foldr(\x acc -> x + acc) z $ join $ [map(\a -> projn vv a) an])] ++ an)  cx
        where
            z = replicate (len vv) [0]

{-|
    === QR decomposition or QR factorization
    <http://localhost/pdf/gram_schmidt.pdf QR_Decomposition>

     * Given \(M\) is square matrix, there exists a pair of matrices \( Q \) is unitary matrix
      and \( R \) is upper triangle matrix such that:
    \[
        M = QR
    \]
-}
qrDecompose'::[[Double]]->([[Double]], [[Double]])
qrDecompose' m = (qm, rm)
    where
        -- [v1, v2, v3]
        vx = map(\r -> map(\x -> [x]) r) $ tran m
        -- [e1, e2, e3]
        hd = take 1 vx
        rs = tail vx
        ex = reverse $ map normList $ rejection hd rs
        -- R matrix, upper triangle matrix
        tm = rMatrixUpperTri ex vx
        -- R matrix, padding with zeros
        rm = zipWith(\n  x -> (replicate n 0) ++ x) [0..] tm
        -- Q matrix
        qm = tran $ map (\x -> foldr(++) [] x) ex

{-|
    === Addition for two list of Rational strings

    >addRL ["1/2", "1/2"] ["1/2", "1/2"]
    >["1/1", "1/1"]
-}
addRL::[String]->[String] ->[String]
addRL [] _ = []
addRL _ [] = []
addRL x y = zipWith(\x y -> addR x y) x y


{-|
    === Division for Rational string
    > divR "1/2" "3/4"
    > "2/3"
-}
divR::String->String->String
divR s1 s2 = multR s1 $ invR s2
        where
            n1 = reduce $ splitR s1
            n2 = reduce $ splitR s2

divR'::Integer->Integer->String
divR' m n = divR m' n'
        where
            m' = show m
            n' = show n

invR::String->String
invR s = xx
            where
                cx = splitR s
                xx = case head cx of
                        0 -> error "denominator can not be zero"
                        _ -> (last cx') ++ "/" ++ (head cx')
                            where
                                cx' = map(\x -> show x) cx


subR::String->String->String
subR s1 s2 = addR s1 $ negR s2

negR::String->String
negR s = multR "-1" s

negList::[String]->[String]
negList cx = map(\x -> negR x) cx

subRL::[String]->[String]->[String]
subRL cx cs = addRL cx (negList cs)


{-|
    === The algorithm uses backtrack.
    * Move top left corner to right and down
    * IF the move is valid THEN set (c, r) = 1, AND move one level down to (c+1, 0)
    * IF the move is not valid THEN move to (c, r+1)
    * Move (c,r) from left to right
    * IF (c,r) is valid move
    * THEN go down one level deep, => (c+1,0)
    * ELSE goto (c, r+1)
    * IF (c, r) is the right most position, AND IF (c,r) is valid move
    * THEN go down to (c+1, 0)
    * ELSE backtrack => take the previous valid move and reset (c,r)=0 AND check whether (c, r+1) is valid move.
    * IF (c, 0) is the bottom level,
    * THEN we are done!

    >let m1 = geneMat 10 Zero
    >pa $ eightQueen m1 [] 0 0
-}
eightQueen::[[Integer]] -> [(Integer, Integer)] -> Integer -> Integer -> [[Integer]]
eightQueen [] _  _ _ = []
eightQueen cs cx m n = if | m < nc && n < nc        && check cs m n -> eightQueen cs' ((m,n):cx) (m+1) 0
                          | m < nc && n < nc        && not (check cs m n) -> eightQueen cs cx m (n+1)
                          | m < nc && n == (nc - 1) && not (check cs m n) -> eightQueen cs'' (tail cx) c (r+1) -- backtrack
                          | otherwise -> if | len cx == nc -> cs
                                            | otherwise -> eightQueen cs'' (tail cx) c (r+1) -- backtrack
            where
                nc = len cs
                cs' = rep2d cs m n 1
                c = fst $ head cx
                r = snd $ head cx
                cs'' = rep2d cs c r 0
                (!) = (!!)

                {-|
                    === Check whether the move is valid or not

                    >check:: 2d array -> column position -> row position -> bool
                -}
                check::[[Integer]] -> Integer -> Integer -> Bool
                check [] _ _ = False
                check cs m n = if | (cs ! to m ! to n) == 1 -> False
                                  | (sum $ cs ! to m) == 1 -> False            -- sum mth row
                                  | (sum $ (tran cs) ! to n) == 1   -> False   -- sum nth col
                                  | (sumRight cs m n) == 1 -> False   -- right diagonal
                                  | (sumLeft cs m n) == 1  -> False   -- left  diagonal
                                  | otherwise                      -> True    -- valid move
                                  where
                                    to = fromIntegral


sumLeft::[[Integer]]->Integer -> Integer -> Integer
sumLeft s c r = (s ! c' ! r') + (down s (c'+1) (r'-1)) + (up s (c'-1) (r'+1))
            where
                r' = fromIntegral r
                c' = fromIntegral c
                down s c r = if c < ns && r >= 0 then s ! c ! r + down s (c+1) (r-1) else 0
                up   s c r = if c >= 0 && r < ns then s ! c ! r + up s (c-1) (r+1) else 0
                ns = len s
                (!) = (!!)



sumRight::[[Integer]]->Integer -> Integer -> Integer
sumRight s c r = s ! c' ! r' + (down s (c'+1) (r'+1)) + (up s (c'-1) (r'-1))
            where
                r' = fromIntegral r
                c' = fromIntegral c
                down s c r = if c < ns && r < ns then s ! c ! r + down s (c+1) (r+1) else 0
                up   s c r = if c >= 0 && r >= 0 then s ! c ! r + up   s (c-1) (r-1) else 0
                ns = fromIntegral $ len s
                (!) = (!!)

{-|
    === Replace element in position (c, r) with element n in 2d matrix. replace element, replace matrix, replace 2d array
-}
rep2d s c r n = (take' c s) ++  [(row (head $ drop' c s) r n)] ++ (tail (drop' c s))
        where
            row s ix n = (take' ix s) ++ [n] ++ (tail (drop' ix s))

{-|
   === Replace index p element with a function f

   @
   replace1d 1 (const 2) [0, 0, 0] -- [0, 2, 0]
   @
-}
replace1d::Int ->(a -> a) ->[a] -> [a]
replace1d p f cx = [if p == i then f x else x | (x, i) <- zip cx [0..]]

{-|
   === Replace index (c, r) element with a value v

   @
    replace2d 1 (const 2) [0, 0, 0] -- [0, 2, 0]
    replace2d 5 (2, 1) [[1, 1, 1], [1, 1, 1], [1, 1, 1]]
    [[1,1,1],[1,1,1],[1,5,1]]

   @
-}
replace2d::a -> (Int, Int) -> [[a]] -> [[a]]
replace2d a (c, r) = replace1d c (replace1d r (const a))

{-|
    === get the diagonal of the matrix
-}
diag1::(Num a)=>[[a]]->[a]
diag1 (cx:[]) = [head cx]
diag1 (x:cx) = (head x):diag1(map tail cx)

-- | get upper triangle of matrix
uptri::[[a]]->[[a]]
uptri (cx:[]) = [cx]
uptri (x:cx) = x:uptri(map tail cx)

{-|
    === Collect all the elements from left diagonal of a matrix.

    @
    mat = [
          [1, 2, 3],
          [4, 5, 6],
          [7, 8, 10]
          ]
    leftDiagonal mat 0 0
    [7, 5, 3]
    @

-}
leftDiagonal::[[Integer]]->Integer -> Integer -> [Integer]
leftDiagonal [] _ _ = []
leftDiagonal s c r = (down s (c'+1) (r'-1)) ++ (s ! c' ! r') : (up s (c'-1) (r'+1))
            where
                r' = fromIntegral r
                c' = fromIntegral c
                down s c r = if c < ns && r >= 0 then s ! c ! r : down s (c+1) (r-1) else []
                up   s c r = if c >= 0 && r < ns then s ! c ! r : up s (c-1) (r+1) else []
                ns = len s
                (!) = (!!)

{-|
    === Collect all the elements from right diagonal of a matrix.

    @
    mat = [
          [1, 2, 3],
          [4, 5, 6],
          [7, 8, 10]
          ]
    rightDiagonal mat 0 0
    [10, 5, 1]
    @

-}
rightDiagonal::[[Integer]]->Integer -> Integer -> [Integer]
rightDiagonal s c r = down s (c'+1) (r'+1) ++ s ! c' ! r' : up s (c'-1) (r'-1)
            where
                r' = fromIntegral r
                c' = fromIntegral c
                down s c r = if c < ns && r < ns then s ! c ! r : down s (c+1) (r+1) else []
                up   s c r = if c >= 0 && r >= 0 then s ! c ! r : up   s (c-1) (r-1) else []
                ns = fromIntegral $ len s
                (!) = (!!)

-- | reverse words in string
reverseWord::String->String
reverseWord s = trim $ foldr(\x y-> x ++ " " ++ y) [] $ reverse $ splitRegex(mkRegex " ") s

rw = reverseWord

{-|
 === Transpose matrix
 \[
    \begin{bmatrix}
    a & b \\
    c & d \\
    \end{bmatrix} \Rightarrow
    \begin{bmatrix}
    a & c \\
    b & d \\
    \end{bmatrix}
 \]

    * NOTE:
    > map tail [[1], [2]] => [[], []]

    @    
    -- Fri 14 Oct 23:32:14 2022 
    -- The code DOES NOT work if the matrix:
    m = [[1, 2], [3]]
    
    tran::[[a]]->[[a]]
    tran [] = []
    tran ([]:_) = []
    tran x = (map head x):tran(map tail x)

     [1, 2, 3] : 
    x0 = [1, 2, 3] => [2, 3] => [3] => [] => []
    x1 = [4, 5, 6] => [5, 6] => [6] => [] => []
    x2 = [7, 8, 9] => [8, 9] => [9] => [] => []
                 
                        1 :  <-  4 :    <-   7 : []

                        2 :  <-  5 :    <-   8 : [] 

                        3 :  <-  6 :    <-   9 : []


                        (9 ) <-  (8)  <-  (7)
                        (6)  <-  (5)  <-  (4)
                        (3)  <-  (2)  <-  (1) 

    @
-}
tran :: [[a]] -> [[a]]
tran = L.transpose

-- Sat Dec 15 00:46:53 2018
-- TODO delete it, never use it
--data Vec a = Vec [String] deriving Show
--data Rat a = Rat String deriving Show
--
---- | unicode operator
---- | dot product of two vectors
--Vec u ⨂ Vec v = Rat $ normR u v

---- | addition of two vectors
--Vec u ⨁ Vec v = Vec $ addRL u v
---- | subtraction of two vectors
--Vec u ⦵	Vec v = Vec $ subRL u v
---- | division of two rational numbers
--Rat x ⨸	Rat y = Rat $ divR x y
-- | concat operator
-- | pp $ "n=" +: fun x
-- | (+:)::(Show a)=> String-> a -> String
-- | (+:) s a = s ++ (show a)

-- | sort row for list of list
sortRow::[[Integer]]->[[Integer]]
sortRow [] = []
sortRow (x:cx) = (sortRow [s | s <- cx, not $ moreZero s x]) ++ [x] ++ (sortRow [ s | s <- cx, moreZero s x])
        where
            moreZero::[Integer]->[Integer]->Bool
            moreZero _ [] = False
            moreZero (x:cx) (y:cy) = if length (x:cx) /= length (y:cy) then False
                                    else
                                        if x == 0 && x == y then moreZero cx cy
                                         else
                                            (if x == 0 && y /= 0 then True else False)


{-|
    === Tranform the matrix to echolon/upper triangle form

    KEY: upper triangle, matrix to upper triangle

    * Thu Jan 10 23:29:20 2019

    * gx <http://localhost/pdf/upper_triangle.pdf Matrix to Upper Triangle>

    * gf /Users/cat/myfile/bitbucket/math/upper_triangle.tex

    * Note: the determinant is different than the original matrix

    * Thu Jan 10 23:29:27 2019

    * Add lcm to the multiplier, make matrix dim from 6 to 13

    * TODO: fix the num overflow issue

    * Wednesday, 15 May 2024 11:45 PDT

    * Update: Use 'mergeSortC' with compare function
-}
upperTri::[[Integer]]->[[Integer]]
upperTri  []         = []
upperTri  m = (head sm):(upperTri am)
        where
            -- | Note: quicksort will not work here because it is unstable sort, not sure about that ?
            -- sm = mergeSortM m
            sm = mergeSortC cmpNumZero m

            -- [
            --  [1, 2, 3]
            --  [4, 5, 6]
            --  [7, 8, 10]
            -- ]
            -- first row
            -- m1 = [
            --       [1, 2, 3]
            --       [1, 2, 3]
            --      ]
            m1 = toInt $ init $ map(\x -> head sm) sm
            -- rest of the rest
            -- m2 =  [
            --        [4, 5, 6 ]
            --        [7, 8, 10]
            --       ]
            m2 = toInt $ tail $ sm

            -- m1 = [          m2=[
            --       4*[1, 2, 3] - [4, 5, 6 ] = [0, 3, 6 ]
            --       7*[1, 2, 3] - [7, 8, 10] = [0, 9, 11]
            --      ]             ]
            ma = zipWith(\rx ry ->
                            let
                                xx = head rx  -- head [1 2 3] => 1
                                yy = head ry  -- head [4, 5, 6] => 4
                                lc = lcm' xx yy -- lcm 4 1 => 4
                                x' = div lc xx -- div 4 1 => 4
                                y' = div lc yy -- div 4 4 => 1
                            in
                                if xx == 0 || yy == 0
                                then ry
                                else zipWith(\x y -> x'*x - y'*y) rx ry
                        ) m1 m2
            am = map (tail) ma
            toInt = (map . map) toInteger
            lcm' a b = lcm (toInteger a) (toInteger b)
            -- [0, 1]  [1]         => False
            -- [0, 1]  [1, 1]      => True
            -- [1, 1]  [0, 1]      => False
            -- [0, 1, 0] [1, 0, 0] => True
            cmpNumZero::(Num a, Eq a)=>[a]->[a]->Bool
            cmpNumZero [] [] = False
            cmpNumZero m1 m2 = (len $ takeWhile (== 0) m1) < (len $ takeWhile(== 0) m2)
  

upperTri'::[[Rational]]->[[Rational]]
upperTri'  []         = []
upperTri'  m = (head sm):(upperTri' am)
        where
            -- | Note: quicksort will not work here because it is unstable sort
            sm = mergeSortM m

            -- [
            --  [1,2, 3]
            --  [4,5, 6]
            --  [7,8, 9]
            -- ]
            -- first row
            -- m1 = [
            --       [1, 2, 3]
            --       [1, 2, 3]
            --      ]
            m1 = init $ map(\x -> head sm) sm
            -- rest of the rest
            -- m2 =  [
            --        [4, 5, 6]
            --        [7, 8, 9]
            --       ]
            m2 = tail $ sm
            ma = zipWith(\rx ry ->
                            let
                                xx = head rx  -- head [1 2 3] => 1
                                yy = head ry  -- head [4, 5, 6] => 4
                                de = denominator
                                nu = numerator
                                xx' = div (nu xx) (de xx)
                                yy' = div (nu yy) (de yy)
                                lc = lcm xx' yy' -- lcm 4 6 => 12
                                x' = toRational $ div lc xx'   -- div 12 4 => 3
                                y' = toRational $ div lc yy'   -- div 12 6 => 2
                            in
                                if xx == 0 || yy == 0
                                then ry
                                else zipWith(\x y -> x'*x - y'*y) rx ry
                        ) m1 m2
            am = map (tail) ma

{-|
    === Division is painful
    @
    class (Num a)=> Fractional  a where
        (/):: a -> a -> a

    divI::(Fractional a)=>Integer -> Integer -> a

    class Num a where
        fromInteger::Integer -> a

    class (Num a, Ord a)=> Real a where


    fromIntegral::(Integral a, Num b) => a -> b
    fromIntegral = fromInteger . toInteger

    class (Real a, Enum)=> Integral a where   [1]
    toInteger(Real a, Enum a)=> a -> Integer  [2]
    fromInteger::(Num a)=>Integer -> a        [3]

    proof:
    [1] [2] [3] =>
    fromIntegral(Num a) => Integral -> a

    @
-}
divI::(Fractional a)=>Integer->Integer->a
divI n m = (fromInteger n) / (fromInteger m)



--divI::(Fractional a)=>Integer->Integer->a
--divI n m = (realToFrac n) / (realToFrac m)

{-|
    === Find the invertible matrix, return ([[]], [[]]) if the matrix is singular
    * The code does not check whether the matrix is singular or not

    >m <- randomMatrix 7 7  -- Int matrix
    >m' = (map . map) rf m  -- Double matrix
    >inverse m'
    >
    >inverse::(Integral a, Fractional a, Real a, Show a, Eq a)=>[[a]]->([[a]], [[String]])
    >inverse::(Fractional a, Show a, Eq a)=>[[a]]->([[a]], [[String]])

    * TODO: Remove division
-}
inverse::[[Double]]->([[Double]], [[String]])
inverse m = if diag == 0 then ([[]], [[]]) else (mb', mc)
        where
            id = ident' $ length m
            -- argumented matrix [m] ++ [id]
            argm = zipWith(\x y -> x ++ y) m id
            -- argm =
            -- [[1 2 3 1 0 0]]
            -- [[4 5 6 0 1 0]]
            -- [[7 8 9 0 0 1]]
            -- mt = upperTri $ (map . map) toInteger argm
            mt = upperTri $ (map . map) round argm
            -- mt =
            -- [[1, 2, 3 x x x]]
            -- [[   2, 2 x x x]]
            -- [[      1 x x x]]
            --
            -- If diag[onal] == 0 then it is single matrix
            diag = foldl(*) 1 [head x | x <- mt]
            ar = zipWith(\x y -> (replicate x 0) ++ y) [0..] mt
            -- ar =
            -- [[1 2 3 x x x]
            --  [0 2 2 x x x]
            --  [0 0 1 x x x]]
            pm = map(\x -> partList (length ar) x ) ar
            -- pm =
            -- [[[1 2 3] [x x x]]
            --  [[0 1 2] [x x x]]
            --  [[0 0 1] [x x x]]]
            m1 = map(\r -> head r) pm
            m2 = map(\r -> last r) pm
            -- m1 =
            -- [[1 2 3]
            --  [0 1 2]
            --  [0 0 1]]
            -- m2 =
            -- [[x x x]
            --  [x x x]
            --  [x x x]]
            m11= reverse $ map(\x -> reverse x) m1
            -- m11 =
            -- [[3 2 1]
            --  [2 1 0]
            --  [1 0 0]]
            -- [[1 0 0]
            --  [2 1 0]
            --  [3 2 1]]
            m22= reverse $ map(\x -> reverse x) m2
            -- m22 =
            -- [[x x x]
            --  [x x x]
            --  [x x x]]
            m3 = zipWith(\x y -> x ++ y) m11 m22
            m4 = (map . map) fromIntegral $ upperTri m3
            --m4'= map(\r -> map(\x -> divI x   $ toInteger (head r)) r) m4
            -- Fri Dec 14 16:04:32 2018
            -- remove the division here
            -- m4'= map(\r -> map(\x -> divI x $ head r) r) m4
            -- m4'= map(\r -> map(\x -> let h = head r in divI x h ) r) m4
            -- m4'= map(\r -> map(\x -> let h = head r in divI (toInteger x) h  ) r) m4
            m4'= map(\r -> map(\x -> let h = head r in (rf x)/(rf h) ) r) m4
            mm'= zipWith(\x y -> (replicate x 0) ++ y) [0..] m4'
            mm = map(\x -> partList (length mm') x) mm'
            m1'= map(\x -> head x) mm
            m2'= map(\x -> last x) mm
            ma'= map(\x -> reverse x) $ reverse m1'
            -- mb'= (map . map) fromIntegral $ map(\x -> reverse x) $ reverse m2'
            mb'= map(\x -> reverse x) $ reverse m2'
            -- Rational representation, e.g. "3/4", "3/1"
            -- m4''= map(\r -> map(\x -> divR' x $ toInteger (head r)) r) m4
            m4''= map(\r -> map(\x -> divR' x $ head r) r) m4
            mm''= zipWith(\x y -> (replicate x "0") ++ y) [0..] m4''
            xm' = map(\x -> partList (length mm'') x) mm''
            m1''= map(\x -> head x) xm'
            m2''= map(\x -> last x) xm'
            ma''= map(\x -> reverse x) $ reverse m1''
            mb''= map(\x -> reverse x) $ reverse m2''
            mc  = reduceForm $ map(\r -> map ratToInt r) mb''

--- | return all odd elements from list
odds::[a]->[a]
odds cx = map(\x -> snd x) $ filter(\(n, _) -> (mod n 2) == 1) $ zip [1..] cx

--- | return even elements from list
evens::[a]->[a]
evens cx = map(\x -> snd x) $ filter(\(n, _) -> (mod n 2) == 0) $ zip [1..] cx

-- TODO
--inverseR::[[Rational]]->[[Rational]]
--inverseR m =
--

{-|
    === check whether a matrix is singular using <http://localhost/pdf/gram_schmidt.pdf QR_Decompoisition>
-}
isInver::(Fractional a, Ord a)=> [[a]] -> Bool
isInver m = if len (filter(< 0.0001) cx) > 0 then False else True
    where
        -- compute all the dot products of [a1, a2, ..] => [a1*a1, a2*a2, ...]
        -- if any dot product of (a_k, a_k) is zero or approx zero then the matrix is singular
        cx = map(\col -> listDots col col) alist
        -- [a1, a2, ..]
        alist = rejection vFir vTai
        -- first column
        vFir = [getVec 0 m]
        -- rest of columns
        vTai = f m
        -- function to extract all columns from matrix:
        -- m = [[a]] => [a1, a2..] = [[[a]], [[a]], ..]
        f ma = map(\n -> getVec n ma) [1..(len ma - 1)]

{-|
    === Multiply all the diagonal elements and check whether the product is zero or not

    Algorithm:

    1. Find the upper triangle of m

    2. Multiply all the diagonal entries

    3. If their product is NOT zero then it is __invertible__, otherwise __singular__

    @
    -- Test case:
    rm <- randomMatrix 100 100
    pp $ isInvertible rm
    @
-}
isInvertible::[[Integer]]->Bool
isInvertible [] = False
isInvertible m = if p == 0 then False else True
            where
                mx = upperTri m
                p  = foldl(*) 1 $ map(\x -> if (length x > 0) then head x else 0 ) mx

-- | Generate n dimentional identity matrix
ident::Integer->[[Integer]]
ident n = map(\x -> (takeN x m) ++ [1] ++ (takeN (n-1 - x) m)) [0..n-1]
        where
            m = repeat' n 0
            takeN::Integer->[Integer]->[Integer]
            takeN n cx = take (fromIntegral n) cx

ident'::(Num a)=>Int->[[a]]
ident' n = map(\x -> (take x m) ++ [1] ++ (take (n-1 - x) m)) [0..n-1]
        where
            m = replicate n 0

identS::Integer->[[String]]
identS n = map(\x -> (takeN x m) ++ ["1"] ++ (takeN (n-1-x) m)) [0..n-1]
        where
           m = repeat' n "0"
           takeN::Integer->[String]->[String]
           takeN n cx = take (fromIntegral n) cx

{-|
    === Integer multiply integer list

    >reverse $ 9 x [9, 8] = [8, 8, 2]
-}
mlist::Integer -> [Integer] -> [Integer]
mlist  n [] = []
mlist  n s = let s' = map(*n) $ (reverse s) ++ [0] in f' n 0 s'
  where
    f' _ _ [] = []
    f' n c (y:ys) = let (q, r) = divMod (y + c) 10 in  (inte r):(f' n (inte q) ys)
    inte = toInteger


{-|
    === KEY: random temp file name, random tmp file

    @
    randomTemp::IO String  -- randomTemp  =>  /tmp/223423423.txt
    @
-}
randomTemp::IO String  -- randomTemp  =>  /tmp/223423423.txt
randomTemp = do
  r1 <- randomInt 10000 100000 >>= return . show
  r2 <- randomInt 10000 100000 >>= return . show
  return $ "/tmp" </> r1 ++ r2 ++ ".txt"


{-|
    === KEY: random Integer

    * generate Integer from x to y random number
-}
drawInteger::Integer->Integer->IO Int
drawInteger x y = getStdRandom(randomR(fromInteger x, fromInteger y))

{-|
    === KEY: random Integer

    * alias of 'drawInteger'
-}
randomInteger::Integer->Integer->IO Integer
randomInteger x y = fmap toInteger $ drawInteger x y



{-|
    === Generate list of Double in \( x \in [0 \dots n], n = 100 \)

    >[0.3, 0.4, 0]

    Use 'randomFrac'
-}
randomDouble::Int->IO [Double]
randomDouble n = mapM(\x -> fmap (\x -> div' x mx) $ drawInteger (0::Integer) (toInteger x)) $ replicate n mx
                where
                    mx = 100

{-|
    === Generate list of Double in \( x \in [0 \dots n], n = 100 \)

    >[0.3, 0.4, 0]

    Use 'randomFrac'
-}
randomFloat::Int->IO [Float]
randomFloat n = mapM(\x -> fmap (\x -> rf $ div' x mx) $ drawInteger (0::Integer) (toInteger x)) $ replicate n mx
                where
                    mx = 100

{-|
    === Generate list of Fractional in \( x \in [0 \dots n], n = 100 \)

    >[0.3, 0.4, 0]

    Num and Fractional are type class so you can NOT do [Num] or [Fractional]

    Type class in Haskell is similar like interface in Java, but there are many differences

    e.g. Integer implement Num => Integer is a concrete class
    e.g. Fractional inherits from Num => Fractional type class
    e.g. Float implement Fractional => Float is concrete class
-}
randomFrac::(Fractional a)=>Int->IO [a]
randomFrac n = mapM(\x -> fmap (\x -> rf $ div' x mx) $ drawInteger (0::Integer) (toInteger x)) $ replicate n mx
                where
                    mx = 100

-- | generate random of list Integer
randomList::Integer ->IO [Integer]
randomList 0 = return []
randomList n = do
        r <- randomRIO (10, 400)
        rs <- randomList (n - 1)
        return (r:rs)

{-|
    === List of random Int

    NOTE: Use randomIntegerList

    @
    randomIntList::Integer ->(Integer, Integer) -> IO [Integer]
    randomIntList 0 _ = return []
    randomIntList n (s, e)= do
      r <- randomRIO (s, e)
      rs <- randomIntList (n - 1) (s, e)
      return (r:rs)
    @
-}
randIntList::Int ->(Int, Int) -> IO [Int]
randIntList 0 _ = return []
randIntList n (s, e)= do
  r <- randomRIO (s, e)
  rs <- randIntList (n - 1) (s, e)
  return ((fi r):rs)

{-|
    === list of random Int

    Same as 'randIntList'
    SEE: 'randomIntegerList'
-}
randomIntList::Int ->(Int, Int) -> IO [Int]
randomIntList 0 _ = return []
randomIntList n (s, e)= do
  r <- randomRIO (s, e)
  rs <- randIntList (n - 1) (s, e)
  return ((fi r):rs)

randomIntegerList::Integer -> (Integer, Integer) -> IO [Integer]
randomIntegerList 0 _ = return []
randomIntegerList n (s, e) = do
  r <- randomRIO (s, e)
  rs <- randomIntegerList (n - 1) (s, e)
  return (r:rs)

{-|
    === Generate \( m \times n \) random matrix.
-}
randomMatrix::(Num a)=>Int->Int->IO [[a]]
randomMatrix 0 _ = return []
randomMatrix _ 0 = return []
randomMatrix m n = do
        list <- replicateM m $ randomList $ fi n
        return $ (map . map) fromIntegral list

{-|
    === Generate \( m \times n \) random matrix.

    Same as 'randomMatrix'
-}
geneRandMat::(Num a)=>(Int, Int) -> IO[[a]]
geneRandMat (a, b) = randomMatrix a b

-- | generate zero matrix
data Mat = Zero | Id

{-|
    === Generate a matrix from 1 to \( n \times n \) with dimention n

    > geneMat1ToN 2
    > [[1, 2],
    >  [3, 4]]
-}
geneMat1ToN::(Num a)=>Integer->[[a]]
geneMat1ToN n = (map . map) fromInteger [[ x + n*y  | x <- [1..n] ] | y <- [0..n-1]]

{-|
    === Generate a matrix from 1 to \( m \times n \)

    > geneMat1ToN 2 3
    > [[1, 2, 3],
    >  [4, 5, 6]]
-}
geneMatMN::Integer -> Integer ->[[Integer]]
geneMatMN m n = [ [ n' + n*m'  | n' <- [1..n] ] | m' <- [0..m-1]]

-- | generate zero or identity matrix
-- |
geneMat::Integer ->Mat ->[[Integer]]
geneMat n m = case m of
                Zero -> map(\x -> repeat' n 0) [1..n]
                Id   -> ident n


help::IO()
help = do
        putStrLn "------------------------------------------"
        putStrLn "run [zip]  [foo]                     => zip -r foo.zip foo"
        putStrLn "run [zip]  [foo] [foo.zip]           => zip -r foo.zip foo"
        putStrLn "run [gz]   [file.txt]                => gzip foo => foo.gz"
        putStrLn "run [tar]  [file.txt]                => tar -czvf foo.tar.gz"
        putStrLn "------------------------------------------"
        putStrLn "run uzip file.txt.zip          => unzip foo.zip foo"
        putStrLn "run ugz  file.txt.gz           => gunzip foo.gz foo"
        putStrLn "run utar file.txt.tar.gz       => file.txt"
        putStrLn "------------------------------------------"
        putStrLn "run grep pattern               => grep --color --include=\"*.java\" -Hnris pattern ."
        putStrLn "run grep \"*.hs\" pattern      => grep --color --include=\"*.hs\"   -Hnris pattern ."
        putStrLn "------------------------------------------"
        putStrLn "run find \"*.hs\"              => find -iname \"*.hs\" -print"
        putStrLn "run find a1                    => find -iname [\"*.hs\"] -print"
        putStrLn "run ssh                        => ssh-keygen -C noname"
        putStrLn "------------------------------------------"


-- | Add more doc here
cmd::[String] -> IO ()
cmd x = case x of
        [op]     -> case op of
                         "h"  -> help

                         "k"  -> createProcess (proc "ssh-keygen" ["-C", "noname"]){ cwd = Just "." } >>= \_ -> return ()
                         "ssh" -> do
                                    print cmd
                                    (Nothing, Just hout, Nothing, ph) <- createProcess p
                                    out <- hGetContents hout
                                    mapM_ putStrLn $ lines out
                                    where
                                        p = (shell cmd)
                                            { std_in  = Inherit
                                            , std_out = CreatePipe
                                            , std_err = Inherit
                                            }
                                        cmd = "ssh-keygen -C noname"
                         _    -> putStrLn "Invalid option"
        [op, a1] -> case op of
                         "zip" -> createProcess (proc "/usr/bin/zip"    ["-r", (a1 ++ ".zip"), a1]){ cwd = Just "." } >>= \_ -> return ()
                         "gz"  -> createProcess (proc "/usr/bin/gzip"   [a1]){ cwd = Just "." } >>= \_ -> return ()
                         "tar" -> createProcess (proc "/usr/bin/tar"    ["-czvf", (a1 ++ ".tar.gz"), a1]){ cwd = Just "." } >>= \_ -> return ()
                         "utar"-> createProcess (proc "/usr/bin/tar"    ["-xzvf", a1]){ cwd = Just "." } >>= \_ -> return ()
                         "uzip"-> createProcess (proc "/usr/bin/unzip"  [a1]){ cwd = Just "." } >>= \_ -> return ()
                         "ugz" -> createProcess (proc "/usr/bin/gunzip" [a1]){ cwd = Just "." } >>= \_ -> return ()
                         "find"-> do
                                    print cmd
                                    (Nothing, Just hout, Nothing, ph) <- createProcess p
                                    ec <- waitForProcess ph
                                    out <- hGetContents hout
                                    mapM_ putStrLn $ lines out
                                    where
                                        p = (shell cmd)
                                            { std_in  = Inherit
                                            , std_out = CreatePipe
                                            , std_err = Inherit
                                            }
                                        cmd = "/usr/bin/find . -iname \"" ++ a1 ++ "\" -print"
                         "grep"-> do
                                    (Nothing, Just hout, Nothing, ph) <- createProcess p
                                    ec <- waitForProcess ph
                                    out <- hGetContents hout
                                    mapM_ putStrLn $ lines out
                                    where
                                        p = (shell $ "grep --color --include=\"*.hs\" -Hnris " ++ a1 ++ " . ")
                                            { std_in  = Inherit
                                            , std_out = CreatePipe
                                            , std_err = Inherit
                                            }
                         _     -> print $ "[" ++ op ++ "][" ++ a1 ++ "]"

        [op, a1, a2] -> case op of
                 "zip" -> createProcess (proc "/usr/bin/zip" ["-r", a2, a1]){ cwd = Just "." } >>= \_ -> return ()
                 "grep"-> do
                            (Nothing, Just hout, Nothing, ph) <- createProcess p
                            ec <- waitForProcess ph
                            out <- hGetContents hout
                            mapM_ putStrLn $ lines out
                            where
                                p = (shell $ "grep --color --include=" ++ "\"" ++ a1 ++ "\"" ++ " -Hnris " ++ a2 ++ " . ")
                                    { std_in  = Inherit
                                    , std_out = CreatePipe
                                    , std_err = Inherit
                                    }
                 _     -> print $ "[" ++ op ++ "][" ++ a1 ++ "][" ++ a2 ++ "]"

        _ -> help


-- | Compile haskell code to $ff/mybin/myHaskell => create symbol link  $sym/sym
-- | [file:myHaskell.hs]  [sym:symbol] link name in $sym
compileHaskellToBin::String->String->IO()
compileHaskellToBin file sym = rm bin >> (run $ toBin file) >> run link >> return ()
                where
                    toBin s = "ghc -i/Users/cat/myfile/bitbucket/haskell -O2 /Users/cat/myfile/bitbucket/haskell/" ++ s ++ " -o " ++ bin
                    link    = "ln -f -s " ++ bin ++ " " ++ symbin
                    symbin  = "/Users/cat/myfile/symbin" </> sym
                    mybin   = "/Users/cat/myfile/mybin"
                    base    = foldr(++) [] $ init $ splitRegex(mkRegex "\\.") file
                    bin     = mybin </> base




-- | compare two string ignore cases
strCompareIC::String->String->Bool
strCompareIC x y = toUpperStr x == toUpperStr y


{-|
    === file base name

    KEY: file extension, extension, basename, base name, file ext

    >baseName "/dog/file.txt" => "file"
    >takeFileName gives "file.ext"
    >takeDirectory gives "/directory"
    >takeExtension gives ".ext"
    >dropExtension gives "/directory/file"
    >takeBaseName gives "file"
    >"/directory" </> "file.ext".
    >"/directory/file" <.> "ext".
    >"/directory/file.txt" -<.> "ext".

-}
baseName::FilePath -> String -- baseName /dog/cat/file.x => file
baseName = takeBaseName

dropExt::FilePath -> String -- /dog/cat/file.x => /dog/cat/file
dropExt = dropExtension

takeExt::FilePath -> String
takeExt = takeExtension

{-|
    === take file name

    See 'takeFileName'
-}
takeName::FilePath -> String -- takeFileName "/tmp/f.x" => f.x
takeName = takeFileName

{-|
    === drop file name from a path

    See 'dropFileName'
-}
dropName::FilePath -> FilePath  -- dropName "/tmp/f.x" => "/tmp/"
dropName = dropFileName

{-|
    === KEY: take directory only, same as 'dropName'

    See 'dropFileName' 'dropName'

    * It is the same name as in Python AronLib.py
-}
takeDir::FilePath -> FilePath  -- takeDir "/tmp/f.x" => "/tmp/"
takeDir = dropFileName

{-|
   === KEY: dropName TS.Text
-}
dropNameT::TS.Text -> TS.Text
dropNameT  = strToStrictText . dropName . toStr

{-|
   === KEY: dropExt TS.Text
-}
dropExtT::TS.Text -> TS.Text
dropExtT = strToStrictText . dropExt . toStr


{-|
    === Take the base name from dir in Text
-}
baseNameT::TS.Text -> TS.Text -- Text => baseNameT /dog/cat/file.x => file
baseNameT s = toT $ baseName $ toS s
    where
        toS = strictTextToStr
        toT = strToStrictText

takeFileNameT::TS.Text -> TS.Text -- Text => takeFileNameT /dog/cat/file.x => file.x
takeFileNameT s = toT $ takeName $ toS s
    where
        toS = strictTextToStr
        toT = strToStrictText

{-|
    === Goto current directory, use in script
-}
gotoCurrDir::IO()
gotoCurrDir = do
              curr <- getPwd
              cd curr

{-|
    === KEY: Walking directory with filter or lambda function: upper case file extension

    * if there is file, then append it to list

    * otherwise, walking inside the dir

    * Pass lambda func: (FilePath -> IO [String]) as argument

    @

    -- ls all files recursively
    let f fn = return [fn] in dirWalk f "/tmp"

    let f fname = takeExtension fname == ".png" then return [fname] else return []
    ls <- dirWalk f "/tmp"

    ls <- dirWalk (\x -> x == ".png") "/tmp"
    @
-}
dirWalk :: FilePath -> (FilePath -> IO [String]) -> IO [String]  -- let f fn = takeExtension fn == ".png" then return [fn] else return [] in dirWalk f "/tmp"
dirWalk top filefunc = do
  isDir <- dirExist top
  if isDir
    then
      do
        files <- lsDir top
        foldrM(\f d -> (dirWalk (top </> f) filefunc >>= \x -> return (x ++ d))) [] files
    else
      filefunc top

{-|
    === KEY: walk through a list of dir

    @
    p1 <- getEnv "m"
    p2 <- getEnv "www" >>= \x -> return $ x </> "pdf"
    dirWalkPathList (\p -> let ex = takeExt p in ex == ".tex" ? return [x] $ return []) [p1, p2]
    @
-}
dirWalkPathList::(FilePath -> IO [String]) -> [FilePath] -> IO [String]
dirWalkPathList f cx = mapM (\p -> dirWalk p f) cx >>= \x -> return $ join x

{-|
    === KEY: list all files in dir, not recursive
-}
lsCurr::String->IO [String]  -- lsCurr "/tmp", ls current dir, not recursive
lsCurr = lsFileFull

lsDir::FilePath -> IO [FilePath]
lsDir = listDirectory

{-|
   === get directory contents
-}
getDirContent::FilePath -> IO [FilePath]
getDirContent p = getDirectoryContents p

{-|
    === KEY: list file with regex match, see 'lsRegexFull', list file with filter, file filter

    >ff <- lsRegex (getEnv j) "\\.java$" -- list all java file
    >ff <- lsRegex (getEnv j) "^try"     -- list all file names starts with "try"
-}
lsRegex::String-> RegexStr ->IO [String]
lsRegex s r = lsFile s >>= \f -> return $ filter(matchTest reg) f
            where
                reg = mkRegexWithOpts r True False

{-|
   === KEY: touch a file
-}
touch::FilePath -> IO()
touch fn = fExist fn >>= \b -> unless b $ writeFile fn "" >> touchFile fn

{-|
    === remove file only, delete file
-}
rm::FilePath -> IO()
rm s =  doesFileExist s >>= \x -> if x then (isFile s >>= \x -> if x then removeFile s else return ()) else return ()

{-|
    === check whether a given file is a directory or symbol link to a directory

    > isDir p = doesDirectoryExist p

    <https://hackage.haskell.org/package/directory-1.3.4.0/docs/System-Directory.html#v:doesDirectoryExist doesDirectoryExist>

    Also see 'isFile'
-}
isDir::FilePath->IO Bool
isDir = doesDirectoryExist

dirExist::FilePath -> IO Bool
dirExist = doesDirectoryExist

isFile::FilePath->IO Bool
isFile s =  getFileStatus s >>= \st ->return $ isRegularFile st

{-|
    === Check if file exist

    NOTE: ONLY for file
-}
fExist::FilePath -> IO Bool     -- Check if file exist, ONLY for file
fExist = doesFileExist 

fileExistA::FilePath -> IO Bool  -- from System.Posix.Files
fileExistA = fileExist

doesExistF :: FilePath -> IO Bool
doesExistF = doesFileExist

{-|
    === Same as 'fExist'
-}
fe = fExist


{-|
    === remove directory recursive

    >rm "dir"
-}
rmDir::FilePath->IO()
rmDir s = removeDirectoryRecursive s


--removeDirectoryRecursive s
--isFile::FilePath->IO Bool
--isFile s =  getFileStatus s >>= \st ->return $ isRegularFile st

{-|
    === Show current directory
-}
pwd::IO()
pwd = sys "pwd" >> return ()
--pwd::IO()
--pwd = createProcess(proc "pwd"  [] ) >> return ()



{-|
    === change dir
-}
cd::FilePath->IO()
cd p = setCurrentDirectory p

{-|
   === set current directory
-}
setCurrentDir::FilePath -> IO()
setCurrentDir p = setCurrentDirectory p

{-|
    === KEY: get environment variable, getEnv 

    SEE: 'getEnv'
-}
en::String->IO String  -- getEnv
en s = getEnv s

cc::String->IO ()
cc cmd = callCommand cmd

g::IO()
g = getEnv "g" >>= \x -> print x >> return ()

{-|
    === Sleep n seconds

    > sleepSec 2
-}
sleepSec::Int -> IO()
sleepSec n = threadDelay $ n*1000000

{-|
    === sleep 1 sec = 100000
-}
sleep::Int->IO()
sleep n = threadDelay n

{-|
    === split path

    > asplitPath "/dot/cat/"
    > ["dog", "cat"]
    >
    > asplitPath "dot/cat/"
    > ["dog", "cat"]
    >
    > asplitPath "/dot/cat"
    > ["dog", "cat"]
    >
    > asplitPath "dot/cat"
    > ["dog", "cat"]
-}
asplitPath::FilePath -> [String]
asplitPath s =  filter( \x -> length x > 0) $ splitRegex(mkRegex "/") s

{-|
    === splitPathA from System.FilePath.Posix.splitPath

    >splitPathA "/dog/cat/"
    >["/", "dog/", "cat/"]
    >
    >splitPathA "/dog/cat"
    >["/", "dog/", "cat"]
    >
    >splitPathA "dog/cat"
    >["dog/", "cat"]
    >
    >splitPathA "/"
    >["/"]
    >
    >splitPathA "./"
    >["./"]
-}
splitPathA::FilePath -> [FilePath]
splitPathA s = splitPath s


take'::(Integral n)=> n -> [a] -> [a]
take' n cx = take (fromIntegral n) cx

{-|
  === take all the elements if \( f(x) \) condition is true

  * It is same as `takeWhile`

  >
  > takeIf (\x -> head x == '-') ["-a", "-b", "cc"]
  > ["cc"]

 -}
takeIf::(a -> Bool) -> [a] -> [a]
takeIf f cx = takeWhile f cx

{-|
  === drop all the elements if \( f(x) \) condition is true

  * It is same as `dropWhile`

  >
  > dropIf (\x -> head x == '-') ["-a", "-b", "cc"]
  > ["-a", "-b"]
  >
  > dropIf (\x -> x == ' ') "   abc"
  > "abc"

 -}
dropIf::(a -> Bool) -> [a] -> [a]
dropIf f cx = dropWhile f cx

{-|
    Integer type
-}
takeN::(Integral n)=> n -> [a] -> [a]
takeN n cx = take (fromIntegral n) cx


drop'::(Integral n)=> n -> [a] -> [a]
drop' n cx = drop (fromIntegral n) cx

pathBase::FilePath -> FilePath
pathBase s = if length list > 0 then last list else []
            where
                list = asplitPath s

dropPath::Integer -> String -> String
dropPath n s = concatStr (takeEnd (en - n) ls)  "/"
               where
                 ls = splitRegex (mkRegex "/") s
                 en = len ls

dropPathEnd::Integer -> String -> String
dropPathEnd n s = takePath n' s
                  where
                    ls = splitRegex (mkRegex "/") s
                    n' = (len ls) - n

takePath::Integer -> String -> String
takePath n s = concatStr (take' n ls) "/"
               where
                 ls = splitRegex (mkRegex "/") s

takePathEnd::Integer -> String -> String
takePathEnd n s = concatStr (takeEnd n $ splitRegex (mkRegex "/") s)  "/"

-- | 1. copy FilePath to "/tmp"
-- | 2. move the file back to dir with newName
-- | copy file and rename it in the same dir
copyRename::FilePath->String->IO()
copyRename fp newName = do
           copyFileToDir fp "/tmp"
           mv ("/tmp" </> fname) (dir </> newName)
           rm ("/tmp" </> fname)
           where
                fname = takeFileName fp
                dir = takeDirectory fp

{-|
    ==== If source is valid file and dest is valid dir, otherwise error
-}
copyFileToDir::FilePath -> FilePath -> IO()
copyFileToDir s d = copyFile s (d </> pathBase s)


-- | create empty file
createFile::FilePath->IO()
createFile f = writeFile f []

{-|
    === List file with a Regex filter

    SEE: Same as 'lsFileFilter'

    e.g. all html files
    @
    >ls <- listDirFilter pa "\\.html$"
    @
-}
listDirFilter::FilePath -> String -> IO [FilePath]  -- ls <- listDirFilter path "\\.html$"
listDirFilter p regex = filter mt <$> listDirectory p
    where
        mt = matchTest $ mkRegexWithOpts regex False False

{-|
    === List file with a Regex filter

    SEE: Same as 'listDirFilter'

    e.g. all html files
    @
    >ls <- lsFileFilter pa "\\.html$"
    @
-}
lsFileFilter::FilePath -> String -> IO [FilePath]  -- ls <- lsFileFilter path "\\.html$"
lsFileFilter p regex = filter mt <$> listDirectory p
    where
        mt = matchTest $ mkRegexWithOpts regex False False

  

-- | copy dir to other dir
copyDir::FilePath -> FilePath -> IO()
copyDir s d = do
                status <- getFileStatus s
                if isRegularFile status then copyFileToDir s d
                    else do
                            tmpList <- listDirectory s
                            let list = map(\x -> s </> x) tmpList
                            fList <- filterM(\x -> getFileStatus x >>= \x -> return $ isRegularFile x) list
                            dirList <- filterM(\x -> getFileStatus x >>= \x -> return $ isDirectory x) list
                            let dList = map(\x -> pathBase x) dirList
                            let soList = asplitPath s
                            let deList = asplitPath d
                            let full = d </> pathBase s
                            mkdir full
                            mapM(\x -> copyFile x (full </> (pathBase x)) ) fList
                            mapM(\x -> copyDir x  full) dirList
                            return ()

{-|
    === rename dir or file => dir or file
-}
mv::FilePath->FilePath->IO()
mv s d = isFile s >>= \x -> if x then renameFile s d else renameDirectory s d

{-|
    === mv file, rename file
-}
rename::FilePath -> FilePath -> IO()
rename = mv

{-|
    === rename all files in path, e.g. s=XXX img.JPG => img_XXX.JPG
-}
renameAllFile::String->String->IO()  -- renameAllFile "/tmp" "_xxx"  => /tmp/{f1_xxx.txt, f2_xxx.txt}
renameAllFile p s = do
                list <- lsFileFull p
                mapM(\x -> mv x ((dropExtension x) ++ s ++ (takeExtension x))) list
                return ()

mvFiles = renameAllFile
mvFile = renameFile


{-|
    === Create directory, make directory
-}
mkdir::FilePath -> IO ()
mkdir s = createDirectoryIfMissing False s

mkdirp::FilePath->IO()
mkdirp s = createDirectoryIfMissing True s

sfilter::String->[String]->[String]
sfilter s l = filter(matchTest $ mkRegex s) l


{-|
    === KEY: set cursor position, move cursor

    + - - - - - >
    |      |
    |  - - + (nrow, ncol)
    |
    |
    V

    @ 
        ESC[{line};{col}H
        ESC[{line};{col}f
              ↑ 
              + -> move cursor to line #, column #
    @

    <https://gist.github.com/bsdshell/b7458770250c2b0cb71712ec9c8867ef Escape code>
-}
setCursorPos::Int -> Int -> IO ()  -- setCursorPos nrow ncol
setCursorPos = AN.setCursorPosition

setCursorPosStr::Int -> Int -> String  -- setCursorPosStr nLine nCol
setCursorPosStr ln cn = "\x1b[" ++ (show ln) ++ ";" ++ (show cn) ++ "H"

{-|
    === KEY: get terminal size, get screen size, get console size

    * See 'getScreenSize'
    * See 'System.Console.ANSI'
-}
getTerminalSize::IO (Int, Int)
getTerminalSize = do
  ls <- run "stty size"
  if len ls > 0 then do
    let tuple = let lt = map (\x -> read x :: Int) $ splitSPC $ head ls in (head lt, last lt)
    return tuple
  else error "Can not get terminal size"

{-|
    === KEY: get screen size, get console size

    * See 'getTerminalSize'
    * See 'System.Console.ANSI'
-}
getScreenSize::IO (Int, Int)
getScreenSize = getTerminalSize

{-|
    === Do not need to return IO [String] like run

    If command error, process will not be terminated unless following error:

    * PermissionDenied
    * ResourceExhausted
    * UnsupportedOperation

    More detail
    <http://hackage.haskell.org/package/process-1.6.5.1/docs/System-Process.html system>

    * following will not work if use 'run'

    >mapM (\x -> run "git lone " ++ x) listdir

    * change to

    >mapM (\x -> system "git clone " ++ x) listdir

    >system::String -> IO ExitCode
    >sys s = system s

    Use inside GHCi
    or use :! cmd in GHCi

    'ExitCode' See <http://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.IO.Exception.html#ExitCode ExitCode document>

    * Defines the exit codes that a program can return.

    @
       data ExitCode
            = ExitSuccess -- ^ indicates successful termination;
            | ExitFailure Int
                -- ^ indicates program failure with an exit code.
                -- The exact interpretation of the code is
                -- operating-system dependent.  In particular, some values
                -- may be prohibited (e.g. 0 on a POSIX-compliant system).
            deriving (Eq, Ord, Read, Show, Generic)
    @
-}
sys::String -> IO ExitCode
sys s = system s

{-|
    === The power of typeable

    <https://chrisdone.com/posts/data-typeable/ Data.Typeable>
-}
typeChar::Typeable a => a -> Bool
typeChar s = typeOf s == typeOf 'a'

{-|
    === Check type of list of String

    <https://chrisdone.com/posts/data-typeable/ Data.Typeable>
-}
-- typeListStr::Typeable a => a -> Bool
-- typeListStr s = typeOf s == typeOf [""]

{-|
    === Check type of list of ByteString

    <https://chrisdone.com/posts/data-typeable/ Data.Typeable>
-}
-- typeByteString::Typeable a => a -> Bool
-- typeByteString s = typeOf s == typeOf (strToStrictByteString "a")

{-|
    === Check type of String

    <https://chrisdone.com/posts/data-typeable/ Data.Typeable>
-}
-- typeString::Typeable a => a -> Bool
-- typeString s = typeOf s == typeOf "a"

linesBS::BS.ByteString -> [BS.ByteString]
linesBS = S8.lines

{-|
    KEY: lines for strict Text

    Deprecated
    USE: 'linesST'
-}
linesSText::TS.Text -> [TS.Text]
linesSText = TS.lines

{-|
    KEY: lines for strict Text
-}
linesST :: TS.Text -> [TS.Text]
linesST = TS.lines


--lens::Typeable a => a -> Integer
--lens s | typeByteString s = len s
--       | otherwise = 1
--

{-|
   === Check if char is hex.

   * Same as 'isHexDigit'
-}
isHex::Char -> Bool
isHex s = isHexDigit s

{-|
   === Does string contain hex only.

   * See 'isHexDigit'
-}
isHexStr::String -> Bool
isHexStr s = and $ map isHex s

{-|
   === Does string contain digit only.

   * See 'isDigit'
-}
isDigitStr::String -> Bool
isDigitStr s = and $ map isDigit s

{-|
   === Does string contain letter only.

   * See 'isLetter' 'isLetterChar'
-}
isLetterStr::String -> Bool
isLetterStr s = and $ map isLetter s

{-|
   === Check letter

   * It is same as 'isLetter'

   * See 'isLetterChar' 'isDigitStr'
-}
isLetterChar::Char -> Bool
isLetterChar = isLetter

{-|
   === Check digit

   * It is same as 'isDigit'

   * See 'isDigitStr' 'isLetterStr'
-}
isDigitChar::Char -> Bool
isDigitChar = isDigit

{-|
    === run shell cmd, send output to std_out

    * run' does not wait for the process to finish
-}
run'::String->IO()
run' s = sys s  >> return ()
-- run' s = createProcess(proc s  [] ) >> return ()

{-|
    === Run shell cmd, capture std_out

    * some issue with waitForProcess
    * it might be deadlock, e.g. run "ps aux"
    * man ps -x => still d't underand why the process is not terminated
    * top command issue => process is not terminated
    * 1. Change it to System, there is issue with the function
    * TODO: fix it:o
    * There is still some issues, use 'system' instead, Tue Jan 28 11:46:22 2020
-}
run::String->IO [String]
run cmd = do
    (Nothing, Just hout, Nothing, ph) <- createProcess p
    -- some issue with waitForProcess
    -- it might be deadlock, e.g. run "ps aux"
    -- man ps -x => still d't underand why the process is not terminated
    -- top command issue => process is not terminated
    ec <- waitForProcess ph
    if (ec == ExitSuccess)
        then hGetContents hout >>= \x -> return $ lines x
        else do
            pp $ show ec
            exitFailure
            -- error $ "error" ++ show ec
    --mapM_ putStrLn $ lines out
    where
        p = (shell cmd)
            { std_in  = Inherit
            , std_out = CreatePipe
            , std_err = Inherit
            }

runCmd::String -> IO[String]
runCmd = run



{-|
    === Run shell command with Turtle lib, shell command (ExitCode, stdoutx, stderr)

    @
    (e2, so, si2) <- runShellcmd
    if e2 /= ExitSuccess then let rcode = ReplyCode{rcmd="", rerror = si2, stdoutx=si2}
                                  replyJson = toSBS $ DA.encode $ rcode
                              in response $ responseNothingBS replyJson
    else do
        pp so
        let replyCode = ReplyCode{rcmd="", rerror="", stdoutx= so}
        let replyJson = toSBS $ DA.encode $ replyCode
        response $ responseNothingBS replyJson
    @
    See 'runSh', 'runRawCmd', 'run'

    'runShell' is same as 'runSh', cmd is 'String'

    <http://hackage.haskell.org/package/turtle-1.5.21/docs/src/Turtle.Bytes.html#shellStrictWithErr Turtle shellStrictWithErr>

    @
    shellStrictWithErr
        :: MonadIO io
        => Text
        -- ^ Command line
        -> Shell ByteString
        -- ^ Chunks of bytes written to process input
        -> io (ExitCode, ByteString, ByteString)
        -- ^ (Exit code, stdoutx, stderr)
    shellStrictWithErr cmdline =
        systemStrictWithErr (Process.shell (Data.Text.unpack cmdline))
    @
-}
runShell :: String -> IO (ExitCode, TS.Text, TS.Text)  -- runShell String => IO (ExitCode, stdoutx, stderr)
runShell s = TUR.shellStrictWithErr (strToStrictText s) TUR.empty

{-|
    === Differentiate on \(f(x)\)
    === Find the first derivative at \( x \) on function \( (a \rightarrow a)) \)

    > x = 1.0
    > df (\x ->x^2) x
    > 2.0

    \( f(x) = x^2 \Rightarrow f'(x) = 2x \Rightarrow f'(1.0) = 2.0  \)

    First derivative on f or slop of f at point x

    \( f'(x) = \lim_{h \rightarrow 0} \frac{f(x + h) - f(x)}{h} \)

-}
df::(Fractional a)=>(a -> a) -> (a -> a)
df f x = ((f (x + h)) - f x)/h where h = 0.001

{-|
    === Compute the tangent equation at point \((x_0, f(x_0))\) for function \( f \)

    Find a tangent equation at \( x_0 = 1.0 \) in \( f(x) = x^2 \)

    >tanglent (\x -> x^2) 0 1

    Tangent equation:

    \( f'(x) = \frac{y - y_0}{x - x_0} \)

    \( y = f'(x_0)(x - x_0) + y_0 \)

    e.g.

    \( f(x) = x^2 \)

    \( \Rightarrow f'(x_0) = 2x_0 \)

    \( f (x) = x^2 \) where \( x = x_0 \)

    \( y = 2x_0(x - x_0) + y_0 \) where \( y_0 = f (x_0) \)

    \( f' (x_0) = \frac{y - y_0}{x - x0} \)

    \( y = f'(x_0)(x - x_0) + y_0 \)
-}
tangent::(Fractional a)=>(a -> a) -> a -> a -> a
tangent f x x0 = (df f x0)*(x - x0) + (f x0)

{-|
    Compute the tangent vector at point (x0, f x0)
-}
tangentVec::(Fractional a)=>(a -> a) -> a -> a -> (a, a)
tangentVec f x x0 = ve
            where
                f' = df f
                y' = f' x0
                ve = (x0, y')

{-|
    === Generate prime number with Sieve Algorithm
-}
prime = sieve [2..]
    where sieve (p:xs) = p:sieve[ x | x <- xs, x `mod` p /= 0]

{-|
    === Find root for any polynomial function

    * Example: <Users/cat/myfile/bitbucket/haskell/findRoot.hs FindRoot>

    * Partition the interval [x0, xn] into list = [x0, x1) [x1, x2) [x2, x3) ..[x(n-1), xn)
    * Concat [xn, xn] with the list since the left boundary is checked only
    * __Note__: f(xn) might be zero, we need to check the xn boundary
    * TODO: need to check f(xn), DONE, concat [xn, xn] to the list

    === Good test cases:

    \( f(x) = x^2 - 4 \quad x \in [-2, 2] \)

    \( f(x) = x^5 -4x^4 + 0.1x^3 + 4x^2 - 0.5 \quad x \in [-4, 4] \Rightarrow \) 5  solutions

    * limitation:
    * if each subinterval contains two or more values, then ONLY one value can be found
    * subinterval can be adjusted in arbitrary small

    > [0, 2] (2-0)/2 = [0, 1, 2] = [0, 1) ∪ [1, 2) ∪ [2, 2]
-}
oneRoot::(Double->Double)->Double->Double->Double->Maybe Double
oneRoot f x0 x1 ε   | abs(f x0)<= ε = Just x0
                    | abs(f m) <= ε = Just m
                    | f x0 < 0 && f x1 > 0 && f m < 0 = oneRoot f m x1 ε
                    | f x0 < 0 && f x1 > 0 && f m > 0 = oneRoot f x0 m ε
                    | f x0 > 0 && f x1 < 0 && f m < 0 = oneRoot f x0 m ε
                    | f x0 > 0 && f x1 < 0 && f m > 0 = oneRoot f m x1 ε
                    | otherwise                       = Nothing
                    where
                        m = (x0 + x1)/2

-- | Find all the roots for a given close interval: [1, 2], 1 or 2 might be the root
-- |
rootList::(Double->Double)->Double->Double->Double->Integer->[Maybe Double]
rootList f x0 x1 ε n = filter(isJust) $ map(\(t0, t1) -> oneRoot f t0 t1 ε) full
                where
                    n'   = fromInteger n
                    list = map(\k ->delt*k + x0) [0..n']
                    inte = zipWith(\x y -> (x, y)) (init list) (tail list)
                    full = inte ++ [(x1, x1)] -- make sure the left boundary is checked
                    delt = abs(x1 - x0)/ n'



---------------------------------------------------------------------------------
{-|
    === outer product = col ⊗ row
    * Sun O21 00:00:05 2018
-}
outer::(Num a)=>[a]->[a]->[[a]]
outer s1 s2 = map(\x1 -> map(\x2 -> x1*x2) s2) s1

{-|
    === generic outer product or sth, inspired by APL

    Update: Switch row and column to similar to 2d array or APL outer product

    @

    out (\x y -> x == y ? 1 $ 0) [1, 2, 3] [1 2]

       | 1 | 2 |
    -----------+
    1  | x | 0 |
    -----------+
    2  | 0 | x |
    -----------+
    3  | 0 | 0 |
    -----------+

    -- identity matrix
    > m = out (\a b -> a == b ? 1 $ 0) [1..4] [1..4]
    > pmat m
    1 0 0 0
    0 1 0 0
    0 0 1 0
    0 0 0 1
    @
-}
out::(a -> b -> c) -> [a] -> [b] -> [[c]]
out f cx cy = [[ f x y | y <- cy] | x <- cx]

{-|
    === outer sum
-}
outerSum::(Num a) => [a] -> [a] -> [[a]]
outerSum s1 s2 = map(\x1 -> map(\x2 -> x1 + x2) s2) s1


{-|
    === outer modulus = col
-}
outerMod::(Integral a) =>[a] -> [a] ->[[a]]
outerMod s1 s2 = map(\x1 -> map(\x2 -> mod x1 x2) s2) s1

{-|
  === KEY: outer product for string

  DATE: Wednesday, 28 February 2024 12:10 PST

  @
  > v0 = [
          ["x"],
          ["y"]
         ]
  > h0 = [["x", "y"]]
  > 
  > printMat $ outerStr (\a b -> a ++ b) v0 h0
  > "xx" "xy"
  > "yx" "yy"
  @

-}
outerStr::(a -> a-> a)->[[a]]->[[a]]->[[a]]
outerStr f v r = [ map(\rr -> f (head v') rr ) r' | v' <- v, r' <- r]

-- (⊗) x y = outer x y     -- \otimes, conflict with AronGraphic

{-|
    === KEY: symbol operator, unicode operator 
-}
(⦶) x y = outerMod x y  -- \circledvert
  
-- (⊕) = outerSum

-- (⧇) x y = outer x y     -- no idea
(‖) = mod                -- no idea

{-|
    === Get column

    Same as 'getColumn'

    @
    m
    [[1, 2]
     [3, 4]]

    tran m
    [[1, 3]
     [2, 4]]

    vrow [[1, 3]    n
          [2, 4]]
    @
-}
vcol::[[a]] -> Int -> [[a]]  -- vcol [[1], [2]] 1 =>  [1]
vcol m n = tran $ vrow (tran m) n

{-|
   === get column

   Same as 'vcol'
-}
getColumn::[[a]] -> Int -> [[a]]
getColumn = vcol

{-|
   === get row

   Same as 'getRow'
-}
vrow::[[a]] -> Int -> [[a]]
vrow m n = drop (n - 1) $ take n m

{-|
   === get row

   Same as 'vrow'
-}
getRow::[[a]] -> Int -> [[a]]
getRow = vrow

-- sum matrix
-- matSum::(Num a)=>[[a]]->[[a]]->[[a]]
-- matSum m1 m2 = zipWith(\x y -> zipWith(\x1 y1 -> x1+y1) x y) m1 m2

{-|
    === zipWith in two dimensions, zipWith matrix
    @
    zipWith2(Num a) => (a -> a -> a) ->[[a]] ->[[a]] -> [[a]]
    zipWith2 f a b = [ zipWith f ra rb | (ra, rb) <- zip a b]

    zipWith2::(Num a)=>(a ->a ->a)->[[a]]->[[a]]->[[a]]
    zipWith2 f m1 m2 = zipWith(\x y -> zipWith(\x1 y1 -> f x1 y1) x y) m1 m2
    @
-}
zipWith2::(Num a)=>(a ->a ->a)->[[a]]->[[a]]->[[a]]
zipWith2 f m1 m2 = zipWith(\x y -> zipWith(\x1 y1 -> f x1 y1) x y) m1 m2



data XNode = XNode (M.HashMap Char XNode)  Bool deriving(Eq, Show)
{-|
    === insert operation for Tries data structure

    >let xn = insertTries "a" (XNode M.empty False)
    >let xxn = insertTries"ab" xn
    >pp $ "containsTries=" <<< (containsTries "a" xxn == True)
    >pp $ "containsTries=" <<< (containsTries "ab" xxn == True)

    1. If String is empty return XNode is the end of a word
    2. If x points to Nothing(x is not on the Tries), then recur to cx
    3. If x points to an XNode, then recur to cx
-}
insertTries::String -> XNode -> XNode
insertTries [] (XNode m _) = XNode m True  -- it is a word
insertTries (x:cx) (XNode m b) = case xn of
                                 Nothing  -> XNode (M.insert x (insertTries cx (XNode M.empty False)) m) b
                                 Just xn' -> XNode (M.insert x (insertTries cx xn') m) b
                where
                    xn = M.lookup x m

{-|
    === Insert list of strings to Tries, see 'insertTries'
-}
insertTriesList::[String] -> XNode -> XNode
insertTriesList [] n = n
insertTriesList (x:cx) n = insertTriesList cx (insertTries x n)

{-|
    === contain operation for Tries data structure

    >let xn = insertTries "a" (XNode M.empty False)
    >let xxn = insertTries"ab" xn
    >pp $ "containsTries=" <<< (containsTries "a" xxn == True)
    >pp $ "containsTries=" <<< (containsTries "ab" xxn == True)
-}
containsTries::String -> XNode -> Bool
containsTries [] (XNode m b) = True
containsTries (x:cx) (XNode m b) = case xn of
                                   Nothing -> False
                                   Just xn' -> containsTries cx xn'
                where
                    xn = M.lookup x m


{-|
    === Most functions are related to Binary Search Tree
    * Thu Nov  8 21:24:37 2018
-}
data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving (Show, Eq)

insertNode::(Ord a)=>Tree a -> a -> Tree a
insertNode Empty a = Node a Empty Empty
insertNode (Node a left right) b = if b < a then (Node a (insertNode left b) right) else (Node a left (insertNode right b))

{-|
    === Insert a list of element into a 'Tree'
-}
insertFromList::(Ord a)=>Tree a ->[a]->Tree a
insertFromList Empty [] = Empty
insertFromList (Node a l r) [] = Node a l r
insertFromList Empty (x:xs) = insertFromList (insertNode Empty x) xs
insertFromList (Node a l r) (x:xs) = insertFromList (insertNode (Node a l r) x) xs


data LeafyTree a = Branch [LeafyTree a]
                   | Leaf a


{-|
    === foldMap combine foldable monoid type to a monoid

    <https://www.schoolofhaskell.com/user/mgsloan/monoids-tour Monoid and Foldable>

    @
    class Foldable t where
        foldMap ::(Monoid m) => (a -> m) -> t m -> m

    a -> m, a function converts any type to monoid type

    -- what is the identity?
    class Semigroup a => Monoid a where
        mempty :: a
        mconcat :: [a] -> a

    main = do
            let tree1 = Leaf "a"
            let s = foldMap id tree1
            pp s
            let tree2 = Branch [Leaf "a"]
            pp $ foldMap id tree2
            let tree3 = Branch [Leaf "a", Leaf "b", Branch [Leaf "a2", Leaf "b2"]]
            pp $ foldMap id tree3
            pp $ foldMap (Sum . length) tree3
    @
-}
instance Foldable LeafyTree where
    foldMap f (Branch xs) = mconcat $ map (foldMap f) xs
    foldMap f (Leaf a)    = f a

{-|
    === Inorder insection
-}
inorder::Tree a->[a]
inorder Empty = []
inorder (Node a l r) = (inorder l) ++ [a] ++ (inorder r)

{-|
    === Maximum number of levels of a 'Tree'
-}
maxlen::Tree a->Integer
maxlen Empty = 0
maxlen (Node a l r) = 1 + max (maxlen l) (maxlen r)

{-|
    === Better head
    * TODO: Use better error message
-}
head'::[a]->a
head' cx = if length cx > 0 then head cx else error "ERROR: head' => list is empty"

{-|
    === check whether a Tree is Binary tree

    == defintion of BST

    * Null is BST
    * Left subtree is BST
    * Right subtree is BST
    * minimum of left subtree is less than parent node
    * maximum of right subtree is greater than parent node
-}
isBST::(Ord a)=>Tree a -> Bool
isBST Empty = True
isBST (Node a l r) =   isBST l
                    && isBST r
                    && (if l /= Empty then max l < a else True)
                    && (if r /= Empty then min r > a else True)
            where
                min::(Ord a)=>Tree a -> a
                min Empty                = error "min error"
                min (Node a Empty Empty) = a
                min (Node a l _)         = min l
                max::(Ord a)=>Tree a -> a
                max Empty                = error "max error"
                max (Node a Empty Empty) = a
                max (Node a _ r)         = max r

{-|
    === Binary tree insection
-}
binsert::Tree Integer->Tree Integer->Tree Integer
binsert Empty (Node a Empty Empty) = (Node a Empty Empty)
binsert (Node a l r) (Node b Empty Empty) = if b < a
                                            then Node a (binsert l (Node b Empty Empty)) r
                                            else Node a l (binsert r (Node b Empty Empty))

{-|
    === Check whether a binary tree is symmetric or not

    Rename sym to 'isSym'
-}
isSym::Tree a ->Bool
isSym Empty = True
isSym (Node a Empty Empty) = True
isSym (Node a l Empty) = False
isSym (Node a Empty r) = False
isSym (Node a l r) = isSym l && isSym r
  
-- sym::Tree a ->Bool
-- sym Empty = True
-- sym (Node a Empty Empty) = True
-- sym (Node a l Empty) = False
-- sym (Node a Empty r) = False
-- sym (Node a l r) = sym l && sym r
  

{-|
    === Lease common ancestor

    * assume two nodes are in the tree
    * if two nodes are in the same path then the top node will be LCA
-}
lca::(Eq a)=>Tree a -> a -> a -> Maybe a
lca Empty _ _ = Nothing
lca (Node a l r) x y = if a == x || a == y then Just a else
                            let left = lca l x y; right = lca r x y
                                in if left /= Nothing && right /= Nothing then Just a
                                    else if left == Nothing then right else left



{-|
    === Build binary tree from preorder and inorder

    * Get the root from preorder , the first element is the header
    * Get the leftsubtree and rightsubtree from inorder with the root from previous step
    * Partition the preorder and inorder with previous two steps
    * Use smaller preorder and inorder on the left subtree and right subtree to do recursion calls

    @
        2
      1    3 => 1 2 3   inorder

        2
      1    3 => 2 1 3   preorder
    @

    <http://localhost/image/prepost.png Build Tree from preorder and inorder>

    <http://localhost/html/indexSerializeBinaryTreeTernaryTree.html Serialize and Deserialize Binary Tree with marker '#'>
-}
buildTree::[Char]->[Char]->Tree Char
buildTree _ [] = Empty
buildTree [] _ = Empty
buildTree preorder inorder = Node h (buildTree leftPre leftIn) (buildTree rightPre  rightIn)
                            where
                                h = head preorder
                                -- take left subtree from inorder
                                leftIn  = filter(< h) inorder
                                -- take right subtree from inorder
                                rightIn = filter(> h) inorder
                                -- left subtree from preorder
                                leftPre = take (length leftIn)  $ tail preorder
                                -- right subtree from preorder
                                rightPre = drop (length leftIn) $ tail preorder


{-|
    === Find all anagrams from a list of strings

    >anagram "dog" ["god", "cat", "ogd"]
    >["god", "ogd"]
-}
anagram::String->[String] -> [String]
anagram s cx = let mlist = M.lookup (sort s) map in if mlist == Nothing then [] else fromJust mlist
    where
        map = insertMap M.empty cx
        sort = L.sort
        insertMap::M.HashMap String [String] -> [String] -> M.HashMap String [String]
        insertMap m [] = m
        insertMap m (x:cx) = insertMap (insertStr m x) cx
                where
                    insertStr::M.HashMap String [String] -> String -> M.HashMap String [String]
                    insertStr m s = nmap
                        where
                            nmap = let s' = sort s; v = M.lookup s' m
                                   in if v == Nothing then M.insert s' [s] m
                                                      else let k = fromJust v in M.insert s' (s:k) m

{-|
    === Extract AronModule.hs functions out

    gx file:///Users/cat/myfile/bitbucket/stackproject/jupyterlab/readAronModule.html

    <file:///Users/cat/myfile/bitbucket/stackproject/jupyterlab/readAronModule.html Example>

    @
    [(
        [ "AronModule.f"
        , "AronModule.fu"
        , "AronModule.fun"
        , "AronModule.funx"
        , "AronModule.n"
        , "AronModule.nx"
        , "AronModule.u"
        , "AronModule.un"
        , "AronModule.unx"
        , "AronModule.x"
        ]
    , 30003
    , [ "funx::(Integral a, Num b) => a -> b" ]
    )]
    @
-}
redisExtractAronModule::String -> [String] -> [([String], Integer, [String])]
redisExtractAronModule package [] = []
redisExtractAronModule package cx = pMap
    where
       rstr = "(^[a-z]+[a-zA-Z0-9_]*)[[:space:]]*::"
       list = filter(\e -> matchTest (mkRegex rstr) e) cx
       lss = map(\e -> (matchAllText (mkRegex rstr) $ e, e)) list
       ln = map(\x -> (DR.elems $ (fst x) !! 0, snd x)) lss
       lns = map(\x -> (fst $ head $ tail $ fst x, snd x)) ln
       -- rMap = zipWith(\n x -> (prefix $ fst x, n, [snd x])) [30000..] lns
       -- rMap = zipWith(\n x -> (prefixSuffix $ fst x, n, [snd x])) [30000..] lns
       ixBound = redisBound Haskell
       rMap = zipWith(\n x -> (substr $ fst x, n,   [snd x])) [ixBound..] lns

       -- => [([AronModule.k0, AronModule.k1..], Integer, [String])]
       pMap = map (\ls -> (map(\x -> package ++ x) $ t1 ls, t2 ls, t3 ls)) rMap
       --                                         ↑           ↑       ↑
       --                                     ([String]     Integer [String])
       --
       -- package = "AronModule." -- append to each keys
       substr s = unique $ join $ allSubstr s

{-|

    KEY: Parse Java method name, extract method name and form a list
        [([String], Integer, [String])]


    File format:
    jname = "/Users/cat/myfile/bitbucket/javalib/Aron.java"

    The list can be used in Redis Server

    >["line1", "line2"] -> [([k0, k1], 1, ["line1"])]

    @
    ["line1", "line2"] -> [([k0, k1], 1, ["line1"])]

    Logger logInit(String className, String fName){}

    (
        [ "Aron.I"
        , "Aron.In"
        , "Aron.Ini"
        , "Aron.Init"
        , "Aron.g"
        , "Aron.gI"
        , "Aron.gIn"
        , "Aron.gIni"
        , "Aron.gInit"
        , "Aron.i"
        , "Aron.it"
        , "Aron.l"
        , "Aron.lo"
        , "Aron.log"
        , "Aron.logI"
        , "Aron.logIn"
        , "Aron.logIni"
        , "Aron.logInit"
        , "Aron.n"
        , "Aron.ni"
        , "Aron.nit"
        , "Aron.o"
        , "Aron.og"
        , "Aron.ogI"
        , "Aron.ogIn"
        , "Aron.ogIni"
        , "Aron.ogInit"
        , "Aron.t"
        ]
    , 10002
    , [ "Logger logInit(String className, String fName){" ]
    )
    @
-}
redisExtractJavaMethod::String -> [String] -> [([String], Integer, [String])]
redisExtractJavaMethod package [] = []
redisExtractJavaMethod package cx = pMap
    where
        list = filter(\e -> matchTest (mkRegex "public static") e) cx
        ixBound = redisBound Java 
        ls = zipWith(\n x -> (n, x)) [ixBound..] list
        m = map(\x -> (let may = matchRegex (mkRegex regexJavaMethod ) (snd x)
                       in case may of
                                Just e -> head e
                                _      -> []
                       , fst x, trim $ snd x)
               ) ls
        lss = map(\x -> (takeWhile(\e -> isLetter e || isDigit e) (t1 x), t2 x, t3 x)) m
        --
        -- => [([String], Integer, [String])]
        rMap = map(\x ->let la = splitStrChar "[[:space:]]" $ t3 x
                            lb = init $ foldr(\a b -> a ++ " " ++ b) [] $ drop 2 la
                        in (substr $ t1 x, t2 x, [lb])) lss
                        -- in (prefix $ t1 x, t2 x, [lb])) lss
                -- => [([Aron.k0, Aron.k1..], Integer, [String])]
        pMap = map (\ls -> (map(\x -> package ++ x) $ t1 ls, t2 ls, t3 ls)) rMap
        regexJavaMethod = "([a-zA-Z0-9_]+[[:space:]]*\\([^)]*\\))"
        substr s = unique $ join $ allSubstr s

{-|

    KEY: Parse Java method name, extract method name and form a list
        [([String], Integer, [String])]


    File format:
    jname = "/Users/cat/myfile/bitbucket/javalib/Aron.java"

    The list can be used in Redis Server

    >["line1", "line2"] -> [([k0, k1], 1, ["line1"])]
-}
redisExtractJavaMethodWithPackage::String -> [String] -> [([String], Integer, [String])]
redisExtractJavaMethodWithPackage _  [] = []
redisExtractJavaMethodWithPackage package cx = pMap
    where
        list = filter(\e -> matchTest (mkRegex "public static") e) cx
        ixBound = redisBound Java
        ls = zipWith(\n x -> (n, x)) [ixBound..] list
        m = map(\x -> (let may = matchRegex (mkRegex regexJavaMethod ) (snd x)
                       in case may of
                                Just e -> head e
                                _      -> []
                       , fst x, trim $ snd x)
               ) ls
        lss = map(\x -> (takeWhile(\e -> isLetter e || isDigit e) (t1 x), t2 x, t3 x)) m
        --
        -- => [([String], Integer, [String])]
        rMap = map(\x ->let la = splitStrChar "[[:space:]]" $ t3 x
                            lb = init $ foldr(\a b -> a ++ " " ++ b) [] $ drop 2 la
                        in (prefix $ t1 x, t2 x, [lb])) lss
        --
        -- => [([Aron.k0, Aron.k1..], Integer, [String])]
        pMap = map (\ls -> (map(\x -> package ++ x) $ t1 ls, t2 ls, t3 ls)) rMap
        regexJavaMethod = "([a-zA-Z0-9_]+[[:space:]]*\\([^)]*\\))"
        -- package = "Aron." -- append to each keys

{-|
    === KEY: extract code block from /Users/cat/myfile/bitbucket/snippets/snippet.hs

    gx file:///Users/cat/myfile/bitbucket/stackproject/jupyterlab/redisSnippet.html

    <file:///Users/cat/myfile/bitbucket/stackproject/jupyterlab/redisSnippet.html snippet_tmp.hs>

    @
    cab
    c      -> cab dog
    ca     -> cab dog
    cab    -> cab dog

    c   -> 100
    ca  -> 100
    cab -> 100

    100 -> cab dog
    @
-}
redisExtractSnippet::[([String], [String])] -> [([String], Integer, [String])]
redisExtractSnippet cs = s5
        where
            s3 = map(\x -> (lenFun $ unique $ join $ map prefix $ fst x, snd x) )  cs -- => [("a", "b")]
            s4 = map(\(a, b) -> ( (package ++) <$> a, b)) s3
            ixBound = redisBound Snippet
            s5 = zipWith(\x n -> (fst x, toInteger n, snd x)) s4 [ixBound..]               -- => [("a", 1, "b"), ("c", 2, "fun")]
            -- /Users/cat/myfile/bitbucket/snippets/snippet.hs
            package = "snippet."

            lenFun::[String] -> [String]
            lenFun cs = filter(\x -> (len . trimStart $ x) > 1) cs

            t1 (a, b, c) = a
            t2 (a, b, c) = b
            t3 (a, b, c) = c


{-|
   Html textarea
   textArea row col string
   textArea 4 5 "dog"
-}
textArea::Integer -> Integer -> String-> String
textArea r c s = topen ++ row ++ col ++ bclose ++ s ++ tclose
    where
        topen = "<textarea"
        bclose = ">"
        row = sp ++ "rows=\"" ++ (show r) ++ "\""
        col = sp ++ "cols=\"" ++ (show c) ++ "\""
        tclose = "</textarea>"
        sp = " "

{-|
     === Generate html table from list of string

     * gx file:///Users/cat/myfile/bitbucket/stackproject/jupyterlab/htmlTable.html

    @
    <table>
    <tr><td> a1 </td> <td> a2 </td> </tr>
    <tr><td> b1 </td>  <td> b2 </td></tr>
    </table>

    [
     "<table>"
     "<tr><td> a1 </td> <td> a2 </td> </tr>"
     "<tr><td> b1 </td>  <td> b2 </td></tr>"
     "</table>"
    ]

    @
 -}
htmlTable::[[String]] -> [String] -- htmlTable [["dog"],["cat"]]  => ["<table>", "<tr><td>dog</td></tr>", "<tr><td>cat</td></tr></table>"]
htmlTable cs = table (join rows)
    where
        rows = map(\r -> tr $ map(\x -> td x) r) cs
        td x = "<td>" + x + "</td>"
        tr x = ["<tr>"] + x + ["</tr>"]
        table x = ["<table>"] + x + ["</table>"]
        (+) = (++)

{-|
    === Generate r row and c col table
-}
htmlTableRowCol::Int -> Int -> [String]
htmlTableRowCol r c = htmlTable ls
  where
    ls = [[ show (r' + c')| c' <- [1..c]] | r' <- [1..r]]


{-|
    === Generate r row and c col table
-}
htmlTableRowColSText::Integer -> Integer -> [TS.Text]
htmlTableRowColSText r c = strToStrictText <$> htmlTable ls
  where
    ls = [[ show (r' + c')| c' <- [1..c]] | r' <- [1..r]]


{-|
    === Find the name of OS from __environment_variable__ __OSTYPE__

    * Some OS might not set the environment variable name: __OSTYPE__
    * OSTYPE might be set manually in file such as __.profile__
    * Currently it supports MacOS and FreeBSD
    * MacOS = "darwin"
    * FreeBSD = "freebsd"
-}
--getOS::IO String
--getOS = do
--        osv <- getEnv osType
--        let os = if | containStr macOS osv   -> macOS
--                    | containStr freeBSD osv -> freeBSD
--                    | otherwise              -> error unknown
--        return os
--    where
--        macOS = "darwin"
--        freeBSD = "freebsd"
--        osType = "OSTYPE"
--        unknown = "unknown"

getOS::IO String
getOS = run "uname"  >>= return . head

--
-- project_beg:
{-|
    === Project HOME/myfile/bitbucket/stackproject/InsectHistoryToSqlite3
-}
data ShellHistory = ShellHistory
  { shId :: Int64
  , shcmd :: TS.Text
  } deriving (Eq,Read,Show)

-- | import Database.SQLite.Simple.FromRow
-- | two fields: shId, shcmd
instance FromRow ShellHistory where
   fromRow = ShellHistory <$> field <*> field

-- | import Database.SQLite.Simple.ToRow
instance ToRow ShellHistory where
   toRow (ShellHistory _shId shcmd) = toRow (Only shcmd)
-- project_beg:

{-|
    === Insert dot_bash_history to sqlite3

    KEY: insert history, history sqlite3, insert .bash_history to sqlite3

    * Need to create symlink to ~/.bash_history

    home <- getEnv "HOME"
    hisFile = home </> "/myfile/bitbucket/shell/dot_bash_history"

    1. drop ShellHistory table
    2. create table ShellHistory
    3. read dot_bash_history
    4. insert all cmds to ShellHistory

    @
    home <- getEnv "HOME"
    let dbfile = home </> "myfile/bitbucket/database/ShellHistory.db"
    >sqlite3 dbfile
    >.table
    >SELECT * FROM ShellHistory
    @

    * Sqlite3 Table: 'ShellHistory'
    * Construct sql query in 'Query' type as following
    * Use 'toSText' to convert 'String' to 'TS.Text'

    * See <https://hackage.haskell.org/package/sqlite-simple-0.4.18.0/docs/Database-SQLite-Simple.html#t:Query Query>

    @
    data Query = Query{fromQuery::TS.Text}

    insert_table = Query {fromQuery = toSText "INSERT INTO ShellHistory (shcmd) VALUES (?)"}
    @

    UPDATE: Wednesday, 17 November 2021 15:41 PST
    @
    hisdb  = "myfile/bitbucket/database/ShellHistory.db"
    bashHis = "myfile/bitbucket/shell/dot_bash_history"
    @
-}
insertShellHistory::String -> String -> IO()
insertShellHistory bashHis hisdb = do
        home <- getEnv "HOME"
        -- conn <- open $ dbfile home
        conn <- open hisdb
        execute_ conn sql_drop_table
        execute_ conn sql_create_table
        lsText <- readFileSTextList bashHis
        mapM_ (\x -> execute conn insert_table (ShellHistory 0 x) ) lsText
        -- insert data to table
        cmdsql <- query_ conn sql_select ::IO [ShellHistory]
        let cmdList = let ls = map (shcmd) cmdsql::[TS.Text] in map toStr ls::[String]
        pp $ "len=" ++ (show $ len cmdList)
        pp "done"
    where
        insert_table = Query { fromQuery = strToStrictText "INSERT INTO ShellHistory (shcmd) VALUES (?)"}
        sql_select = Query { fromQuery =  strToStrictText "SELECT id, shcmd FROM ShellHistory" }
        sql_drop_table = Query{ fromQuery = strToStrictText "DROP TABLE IF EXISTS ShellHistory" }
        sql_create_table = Query{ fromQuery = strToStrictText "CREATE TABLE IF NOT EXISTS ShellHistory (id INTEGER PRIMARY KEY AUTOINCREMENT, shcmd TEXT)" }

{-|
    === filter out lines in history file

   @
   hisdb  = "myfile/bitbucket/database/ShellHistory.db"
   bashHis = "myfile/bitbucket/shell/dot_bash_history"
   @
-}
queryShellHistory::String -> IO[String]
queryShellHistory hisdb = do
          home <- getEnv "HOME"
          conn <- open hisdb
          cmdsql <- query_ conn sql_select ::IO [ShellHistory]
          let cmdList = let ls = map (shcmd) cmdsql::[TS.Text] in map toStr ls::[String]
          return cmdList
      where
          sql_select = Query {fromQuery = strToStrictText "SELECT id, shcmd FROM ShellHistory"}
          -- sql_select = Query {fromQuery = toSText "SELECT id, shcmd FROM ShellHistory"}

{-|
    === Apple notify shell script, macos script, 
-}
appNotify::String -> IO()
appNotify msg = sys ("notify.sh " ++ msg) >> return ()


{-|
    === Redis get value

    * Redis default Connection Info

    @
    defaultConnectInfo

    connectHost           = "localhost"
    connectPort           = PortNumber 6379 -- Redis default port
    connectAuth           = Nothing         -- No password
    connectDatabase       = 0               -- SELECT database 0
    connectMaxConnections = 50              -- Up to 50 connections
    connectMaxIdleTime    = 30              -- Keep open for 30 seconds
    @

    <https://hackage.haskell.org/package/hedis-0.6.5/docs/Database-Redis.html  Redis_Database>

    Get Value from a key

    It uses Default Connection

    See 'redisSet'

    @ 
    GHCi:
    redisSet "key1" "value1"
    redisGet "key1"
    @
-}
redisGet::String -> IO (Maybe String)  -- redisGet "k1" => IO (Maybe String)
redisGet key = redisConnectDefault >>= \conn -> redisGetConn conn key

{-|
    === Get value from Redis db 

    * Redis default Connection Info

    @
    defaultConnectInfo

    connectHost           = "localhost"
    connectPort           = PortNumber 6379 -- Redis default port
    connectAuth           = Nothing         -- No password
    connectDatabase       = 0               -- SELECT database 0
    connectMaxConnections = 50              -- Up to 50 connections
    connectMaxIdleTime    = 30              -- Keep open for 30 seconds
    @

    See: 'redisGet' and 'redisSet'
-}
redisGetConn::RED.Connection -> String -> IO (Maybe String)
redisGetConn conn key = do
             mValue <- RED.runRedis conn $ do
                        value <- RED.get $ toSBS key
                        let mayValue = case value of
                                    Left _ -> Nothing
                                    -- Right v -> fromMaybe (BSU.fromString "Could not find key") v
                                    Right v -> v
                        return mayValue
                        -- liftIO::(MonadIO m) => IO a => m a
             return $ toStr <$> mValue

{-|
    === Redis set key value

    Set key and value in Redis

    It uses Default Connection

    See 'redisGet'

    @ 
    GHCi:
    redisSet "key1" "value1"
    redisGet "key1"
    @
-}
redisSet::String -> String -> IO ()  -- redisSet "k1" "v1"
redisSet k v = redisConnectDefault >>= \conn -> redisSetConn conn k v

redisSetConn::RED.Connection -> String -> String -> IO ()
redisSetConn conn k v = do
                conn <- redisConnectDefault
                let k' = toSBS k
                let v' = toSBS v
                RED.runRedis conn $ do
                        RED.set k' v'
                        -- liftIO $ print $ "Set k=" ++ k ++ " v=" ++ v
                return ()

redisSetConnSByteString::RED.Connection -> BS.ByteString -> BS.ByteString -> IO ()
redisSetConnSByteString conn k v = do
                conn <- redisConnectDefault
                let k' = k
                let v' = v
                RED.runRedis conn $ do
                        RED.set k' v'
                        CM.liftIO $ putStrLn $ "Set k=" <> toStr(k) <> " v=" <> toStr(v)
                return ()

redisConnectDefault::IO RED.Connection
redisConnectDefault = RED.connect connInfo >>= return
  where
    connInfo = RED.defaultConnectInfo
    -- connInfo' = connInfo{RED.connectMaxConnections = 2000}
    -- RED.connect RED.defaultConnectInfo >>= return
{-|
   === KEY: redis disconnect, destroy resource, destroy connection, destroy redis

   @
   conn <- redisConnectDefault
   RED.disconnect conn
   @
-}
redisDisconnect :: RED.Connection -> IO()
redisDisconnect = RED.disconnect
  
{-|
    === check CSS color format,

    >"#334455"  => valid
    >"#333"     => valid
    >"#1234567" => not valid
-}
checkCSSColorFormat::TS.Text -> Bool
checkCSSColorFormat s = TS.length s <= 7 && TS.head s == '#' && (isHexStr (toStr s'))
     where
         s' = TS.drop 1 s


cssToStr::(String, String) -> String
cssToStr (a, b) = a <> ":" <> b <> ";"

{-|
    === concat style fields: ("color", "red") => "color:red;"
-}
concatStyle::[(String, String)] -> String
concatStyle cs = concatStr (map cssToStr cs) []



fun444::Int ->Int
fun444 x = x + 1

fun555::Int ->Int
fun555 x = x + 1

{-|
    === C style printf, string format

    * import Text.Prinf

    @
     printf::PrintfType => r -> s

     printf "%.2f" 3.0
     3.00
     let n = 3::Int
     let f = rf n
     printf "%.2f, %.2f" f f

     let name = "dog"
     let weight = 20
     printf "(%s)=(%d)"
     "(dog)=(20)"

     let s = pf "%.2f" 3.1415
     pp s

     c      character               Integral
     d      decimal                 Integral
     o      octal                   Integral
     x      hexadecimal             Integral
     X      hexadecimal             Integral
     b      binary                  Integral
     u      unsigned decimal        Integral
     f      floating point          RealFloat
     F      floating point          RealFloat
     g      general format float    RealFloat
     G      general format float    RealFloat
     e      exponent format float   RealFloat
     E      exponent format float   RealFloat
     s      string                  String
     v      default format          any type
    @

    <https://hackage.haskell.org/package/base-4.16.0.0/docs/Text-Printf.html  Printf>
-}
-- pf s d = printf s d
pf::PrintfType r => String -> r
pf s = printf s

{-|
    print output, similar Print.java
-}
p::(Show s)=>s->IO()
p s = print s


pp2 s = TPS.pPrint s

fl::IO()
fl = do 
     (termH, termW) <- getTerminalSize
     let width = 80 >= termW ? termW $ 80
     putStrLn $ foldr (++) "" $ replicate width "-"

{-|
    === line with color
    psTab 5 (color Green   (replicate 80 '-'))
-}
-- lineColor::IO()
-- lineColor = psTab 5 (color Green   (replicate 80 '-'))


pw::(Show s)=>String->s->IO()
pw w s = fpp w s

fpp::(Show s)=>String->s->IO()
fpp msg s = fw msg >> pp s

fw::String->IO()
fw msg = do 
        istty <- queryTerminal stdInput
        -- print istty
        if istty then do
            (termH, termW) <- getTerminalSize
            output termH termW
        else do
            output 80 80
        where
            output termH termW = do 
                let line = foldr(++) "" $ replicate 80 "-"
                let diff   = (80 >= termW ? termW $ 80) - (length msg)
                let half   = div diff 2
                let isEven = mod diff 2
                let left   = foldr(++) "" $ replicate (half + isEven)  "-"
                let right  = foldr(++) "" $ replicate half  "-"
                putStr (left ++ msg ++ right ++ "\n")

ff::(Show var)=>String->var->IO()
ff msg var = do 
                (termH, termW) <- getTerminalSize
                let width = (80 >= termW ? termW $ 80)
                let line = foldr(++) "" $ replicate width "-"
                let diff   = width - (length msg)
                let half   = div diff 2
                let isEven = mod diff 2
                let left   = foldr(++) "" $ replicate (half + isEven)  "-"
                let right  = foldr(++) "" $ replicate half  "-"
                putStr (left ++ msg ++ right ++ "\n" ++ show(var) ++ "\n")

{-|
    === KEY: ternary operator like c++, if else

    @
    let x = 3 in x > 3 ? "> 3" $ " < 3"
    @
-}
infix 1 ?
(?)::Bool -> a -> a -> a
(?) pred a b = if pred then a else b


infixl 1 <<<
(<<<)::(Show a)=>String -> a -> String
(<<<) s a = s ++ (show a)

{-|
    === Parse a text file and return HashMap String (HashMap String String)

    * OS specific config file

    Config file could have the following format.
    @

    > os = macOS
      ↑
      + → os has to be in the file.

    > host = localhost
    > path = /usr/home/user

    > os = linux
      ↑
      + → os has to be in the file.

    > host = myhost
    > path = /usr/home/user


    return

    > HashMap String (HashMap String String) = [("macOS", (fromList [("host", "localhost"), ("path", "/usr/home/user")))]
    @

    * Update Fri Mar 13 21:52:36 2020
    * Add skip comment code
    * Saturday, 24 September 2022 21:13 PDT
    * Update String with double quotes

    @
        -- comment
        -- gf $b/haskellwebapp2/config.txt
        host = localhost
    @

    Use 'createConfigMap' instead

-}
readConfig::FilePath -> IO (M.HashMap String (M.HashMap String String))
readConfig p = do
                ls <- readFileLatin1ToList p
                let list = filter (not . containStr "^[ ]*--") ls
                let blocks = splitWhen (\x -> (length $ trim x) == 0) list
                let ls = filter (\x -> len x > 0) blocks
                let lm = map(\x -> (snd $ splitStrTuple "=" $ head x, M.fromList $ (map (splitStrTuple "=") $ tail x))) ls
                return $ M.fromList lm
    where
        pat = "="


{-|
    === concat two files to a thrid file
-}
concatFile::FilePath -> FilePath -> FilePath -> IO()
concatFile f1 f2 f3 = liftA2 (++) (readFileLatin1ToList f1) (readFileLatin1ToList f2) >>= writeFileList f3

{-|
    === KEY: extract a first number from a string.
-}
extractNumFromStr::String -> Integer  -- extractNumFromStr " 123ab44" => 123,  "ab" => 0
extractNumFromStr s = strToInteger $ case maybe of
                                       Just x -> let n1 = fst x; n2 = snd x in take n2 $ drop n1 s
                                       Nothing -> "0"
    where
        maybe = matchAnyRegex (mkRegex "[0-9]+") s


{-|
    === KEY: convert 'String' to 'Maybe' 'Integer'

    > strToStrictByteString "12" => Just 12
    > strToStrictByteString "01" => Just 1
    > strToStrictByteString "00" => Just 0
    > strToStrictByteString " 1" => Nothing
    > strToStrictByteString "1a" => Nothing
-}
strToIntegerMaybe::String -> Maybe Integer
strToIntegerMaybe s = if isValid then Just (read s' :: Integer) else Nothing
  where
    s' = trim s
    isValid = len (filter isDigit s') == len s'

gcdList::[Integer] -> Integer
gcdList [] = 0
gcdList (x:cx) = gcd x (gcdList cx)


commentLine::String -> String -> String
commentLine lang s = if | lang == "java"   || lang == "cpp"  -> f java
                        | lang == "python" || lang == "bash" -> f python
                        | lang == "haskell"                  -> f haskell
                        | lang == "elisp"                    -> f elisp
                        | otherwise                          -> s
  where
    f pre = let n = len pre in
            if (len . trim) b /= 0 then
              if len b >= n && take n b == pre then s else a + (pre + " ") + b
            else s
    (a, b) = splitWhileStr(\x -> x == ' ') s
    (+) = (++)
    java    = "//"
    python  = "#"
    haskell = "--"
    elisp   = ";;"

commentCode::String -> [String] -> [String]
commentCode lang cx = map(\x -> commentLine lang x) cx

{-|
    === KEY: take strings between beg and end

    NOTE: take the First pair of ONLY
    @
    takeBetweenExc "beg" "end" ["a", "BEG", "c", "END" "d"]
    => ["c"]

    takeBetweenExc "beg" "end" ["a", "BEG", "c", "END" "d", "BEG", "k", "END"]
    => ["c"]
    @
-}
takeBetweenExc::String -> String -> [String] -> [String]  -- takeBetweenExc "beg" "end" ["a", "beg", "c", "end" "d"]
takeBetweenExc _   _   [] = []
takeBetweenExc beg end cx = ss
  where
    ls' = filterNonEmpty cx
    ss = let s = dropWhile(\x -> x /= beg) $ takeWhile(\x -> x /= end) ls' in if len s > 0 then tail s else s



-- BEG123


{-|
    === KEY: take elements between both indices

    @
    takeIndexBetweenInc (3, 4) [0..10]
    => [3, 4]
    @
-}
takeIndexBetweenInc::(Int, Int) -> [a] -> [a]
takeIndexBetweenInc (x0, x1) cx = take (x1 - x0 + 1) $ drop x0 cx

maxList::[Int] -> Int
maxList cx = foldl(\acc b -> max acc b) (minBound::Int) cx

{-|
    === KEY: console ansi box, console textbox, ansi textbox, escape code, ascii code

    See <http://localhost/pdf/unicode_box_drawing.pdf Ansi_Box_Drawing>

    @
    -- BEG
    -- KEY: ansi color, console color
    import Rainbow
    import System.Console.Pretty (Color (..), Style (..), bgColor, color, style, supportsPretty)
    import qualified System.Console.ANSI as AN
    -- END

    -- move 4 tabs to the right
    printBox 4 ["dog", "cat"]

         ⌜-------⌝
         |dog    |
         |cat    |
         ⌞-------⌟
    @
-}
printBox:: Integer -> [String] -> IO()  -- printBox 2 ["dog cat pig"]
printBox n cx = do
    psTab n (color Green top)
    mapM_ (\x -> do
              psTab n ((vl + (color Yellow x) + (replicate (m - len x) ' ') + vr))
          ) cx
    psTab n (color Green bot)
  where
    m = maxList $ map len cx
    (+) = (++)
    top = "+" + (concat $ replicate m "-") + "+"
    bot = "+" + (concat $ replicate m "-") + "+"
    -- rep' n c = concat $ replicate n c
    vl = color Green "|"
    vr = color Green "|"
{-|
    === KEY: console ansi box, console textbox, ansi textbox

    See <http://localhost/pdf/unicode_box_drawing.pdf Ansi_Box_Drawing>

    'printBox' with 'Color'

    @
    data System.Console.Pretty.Color
      = Black
      | Red
      | Green
      | Yellow
      | Blue
      | Magenta
      | Cyan
      | White
      | Default

    -- Defined in ‘System.Console.Pretty’
    instance [safe] Enum System.Console.Pretty.Color
    -- Defined in ‘System.Console.Pretty’


    -- BEG
    -- KEY: ansi color, console color
    import Rainbow
    import System.Console.Pretty (Color (..), Style (..), bgColor, color, style, supportsPretty)
    import qualified System.Console.ANSI as AN
    -- END

    -- move 4 tabs to the right
    printBox 4 ["dog", "cat"]

         ⌜-------⌝
         |dog    |
         |cat    |
         ⌞-------⌟
    @
-}
printBoxColor:: Color -> Integer -> [String] -> IO()  -- printBoxColor Green 2 ["dog cat fox"]
printBoxColor c n cx = do
    psTab n (color colo top)
    mapM_ (\x -> do
              psTab n ((vl + (color c x) + (replicate (m - len x) ' ') + vr))
          ) cx
    psTab n (color colo bot)
  where
    m = maxList $ map len cx
    (+) = (++)
    top = "┌" + (concat $ replicate m "─") + "┐"
    bot = "└" + (concat $ replicate m "─") + "┘"
    -- rep' n c = concat $ replicate n c
    vl = color colo "│"
    vr = color colo "│"
    colo = White

printBoxColor2::Integer -> Integer -> [String] -> IO()
printBoxColor2 colorCode ntab cx = do
    psTab ntab (color colo top)
    mapM_ (\x -> do
              psTab ntab ((vl + (colorfgStr colorCode x) + (replicate (m - len x) ' ') + vr))
          ) cx
    psTab ntab (color colo bot)
  where
    -- len' = BS.length . BSU.fromString . toSBS
    m = maxList $ map len cx
    (+) = (++)
    top = "┌" + (concat $ replicate m "─") + "┐"
    bot = "└" + (concat $ replicate m "─") + "┘"
    -- rep' n c = concat $ replicate n c
    vl = color colo "│"
    vr = color colo "│"
    colo = White


splitWhileStr::(Char -> Bool) -> String -> (String, String)
splitWhileStr f s =(s1, s2)
  where
    s1 = takeWhile f s
    s2 = dropWhile f s

getLineFlush::IO String
getLineFlush = hFlush stdout >> getLine

{-|
    === KEY: replace line with NO Regex

    NOTE: "a & b \\" has issue use regex replace

    @
    file: x.x
    line 1
    latexcode_replace123
    line 2

    replaceLineNoRegex "/tmp/x.x" "latexcode_replace123" "a & b \\"
    @
-}
replaceFileLineNoRegex::FilePath -> String -> String -> IO ()
replaceFileLineNoRegex fp pat sub = do
  ls <- readFileListStrict fp
  let twoList = splitListWhen (\x -> trim x == pat) ls
  let lss = (head twoList) ++ [sub] ++ (last twoList)
  writeFileList fp lss

replaceLineNoRegexListTuple::[String] -> [(String, String)] -> [String]
replaceLineNoRegexListTuple cy [] = cy
replaceLineNoRegexListTuple cy (x:cx) = replaceLineNoRegexListTuple lss cx
  where
    pat = fst x
    sub = snd x
    ls = splitListWhen (\e -> trim e == pat) cy
    lss = (head ls) ++ [sub] ++ (last ls)



{-|
    === Run shell command with Turtle lib

    See 'runShell'

    @
    (e2, so, si2) <- runSh $ toSText cmd
    if e2 /= ExitSuccess then let rcode = ReplyCode{rcmd="", rerror = si2, stdout=si2}
                                  replyJson = toSBS $ DA.encode $ rcode
                              in response $ responseNothingBS replyJson
    else do
        pp so
        let replyCode = ReplyCode{rcmd="", rerror="", stdout= so}
        let replyJson = toSBS $ DA.encode $ replyCode
        response $ responseNothingBS replyJson
    @

    <http://hackage.haskell.org/package/turtle-1.5.21/docs/src/Turtle.Bytes.html#shellStrictWithErr Turtle shellStrictWithErr>

    @
    shellStrictWithErr
        :: MonadIO io
        => Text
        -- ^ Command line
        -> Shell ByteString
        -- ^ Chunks of bytes written to process input
        -> io (ExitCode, ByteString, ByteString)
        -- ^ (Exit code, stdout, stderr)
    shellStrictWithErr cmdline =
        systemStrictWithErr (Process.shell (Data.Text.unpack cmdline))
    @

    SEE: 'runShStr'
-}
runSh :: TS.Text -> IO (ExitCode, TS.Text, TS.Text)  -- runSh TS.Text
runSh s = TUR.shellStrictWithErr s TUR.empty

{-|
   KEY: run String command

   @
   (ExitCode, out, err) <- runShStr "grep error /tmp/a.x"
   if ExitCode == ExitSuccess then putStrLn out else putStrLn err
   @

   SEE: 'runSh'
-}
runShStr :: String -> IO (ExitCode, String, String)
runShStr cmd = do
  let fc = \(a, b, c) -> (a, toStr b, toStr c)
  let cmd' = toSText cmd
  runSh cmd' >>= return . fc


{-|
    === KEY: clear screen, clear terminal
-}
clear::IO()  -- putStr "\ESC[2J"
clear = putStr "\ESC[2J"


pp::(Show s)=>s->IO()
pp s = print s


{-|
    === Prevent shell expand argument variable, e.g '$1', '$2'

    * KEY: shell not expand, shell literal, shell expand, shell escape variable, shell raw variable

    @
     runRawCmd "write_to_shell" ["cat $1 $2"] --  cat $1 $2
    @

    * Usage
    @
     runRawCmd "ls" []
     runRawCmd "ls" ["-la"]
    @

    * 'waitForProcess' to finish or 'exitFailure'

-}
--runRawCmd::String->[String]->IO()
--runRawCmd s cx = createProcess(proc s cx) >> return ()
runRawCmd::String ->[String] ->IO[String]
runRawCmd cmd cx = do
            (Nothing, Just hout, Nothing, ph) <- createProcess p
            -- some issue with waitForProcess
            -- it might be deadlock, e.g. run "ps aux"
            -- man ps -x => still d't underand why the process is not terminated
            -- top command issue => process is not terminated
            ec <- waitForProcess ph
            if (ec == ExitSuccess)
                then hGetContents hout >>= \x -> return $ lines x
                else do
                    pp $ show ec
                    exitFailure
                    -- error $ "error" ++ show ec
            --mapM_ putStrLn $ lines out
            where
                -- https://hackage.haskell.org/package/process-1.6.13.2/docs/src/System.Process.Common.html#CreateProcess
                -- p = CreateProcess
                p = (proc cmd cx)
                    { std_in  = Inherit
                    , std_out = CreatePipe
                    , std_err = Inherit
                    }


{-|
    === Try to replace as many as shell command as possible

    * shell ls command

    * See how far I can go
    * write many shell commands as possible
    * try to emulate shell commands

    * Sat Feb  1 23:40:55 2020
-}
ls::IO()
ls = runRawCmd "ls" [] >>= pre
--ls::IO()
--ls = createProcess(proc "ls"  [] ) >> return ()

{-|
    === get current dir
-}
getPwd::IO FilePath
getPwd = getCurrentDirectory

{-|
   === get current dir

   'getPwd'
   'getCurrentDirectory'
-}
getpwd::IO FilePath
getpwd = getCurrentDirectory

-- setCurrentDir::IO ()

{-|
    === KEY: list file

    * Sat Feb  1 22:25:09 2020
    * FOUND ERROR: don't use the function, it creates zombie process
    * Sun Feb  2 13:12:01 2020
    * Fixed ERROR with 'runRawCmd'

    * NOTE: Not a full path
    * SEE: 'lsFileFull' => list full path
    * SEE: 'lsStr'
-}
lsFile::String->IO [String]  -- list file, not a full path
lsFile p = runRawCmd "ls" [p]

{-|
    === KEY: list file

    * Wed 28 Sep 21:24:18 2022  

    * SEE: 'lsFileFull' => list full path
    * SEE: 'ls'
-}
lsStr::String -> IO [String]
lsStr = lsFile

{-|
    === KEY: random Integer

    * Generate Int from x to y random number

    @
    System.Random randomR :: (Random a, RandomGen g) => (a, a) -> g -> (a, g)
                                                                  ↑        ↑ 
                                                  + -  - - - - -  +        |
                                                  |             + - - -  - + 
                                                  |             | 
    System.Random getStdRandom :: MonadIO m => (StdGen -> (a, StdGen)) -> m a
    @
-}
drawInt::Int -> Int -> IO Int
drawInt x y = getStdRandom(randomR(x, y))

{-|
    === KEY: list of FULL path files

    * 'lsFileFull' is NON recursive
    * See 'lsFile' or 'runRawCmd'
    * return full path files list

    > lsFileFull "."
    > lsFileFull "/dog/cat"

    * Sat Feb  1 22:25:09 2020
    * FOUND ERROR: don't use the function, it creates zombie process
    * Sat Feb  1 23:33:12 2020
    * Fixed ERROR with 'runRawCmd'

    * NOTE: list of full path
    * See 'lsFileFull' => list full path
-}
lsFileFull::String->IO [String]  -- lsFileFull "/tmp"
lsFileFull s =  do
            cur <- getPwd
            let path = if s == "." then cur else s
            l <- lsFile path
            return $ map(\x -> path </> x) l

type RegexStr = String

{-|
    === KEY: list full path with regex match, see 'lsRegex', list file with filter, file filter, file path with regex

    > lsRegexFull "." "\\.hs$"
    > lsRegexFull "/tmp" "\\.hs$"
-}
lsRegexFull::String-> RegexStr ->IO [String] -- lsRegexFull "/tmp" "\\.tex$"
lsRegexFull s r = lsFileFull s >>= \f -> return $ filter(matchTest reg) f
            where
                reg = mkRegexWithOpts r True False

{-|
    === Deprecated, use 'lsRegexFull'
    === list full path with regex match

    > lsRegexFull "." "\\.hs"
    > lsRegexFull "/tmp" "\\.hs"

-}
lsFullRegex::String-> RegexStr ->IO [String]  -- lsFullRegex str regexStr  lsFullRegex "/tmp" "\\.hs$"
lsFullRegex s r = lsFileFull s >>= \f -> return $ filter(matchTest reg) f
            where
                reg = mkRegexWithOpts r True False


{-|
    === KEY: random Int 
-}
randomInt::Int -> Int -> IO Int
randomInt = drawInt

{-|
    === KEY: write a string to file

    Use 'writeFile'

    >writeFile::FilePath -> String -> IO()
    >writeFileStr = writeFile
-}
writeFileStr::FilePath -> String -> IO()
writeFileStr = writeFile

  
{-|
   === Overwrite a file  
-}
writeFileOver :: FilePath -> String -> IO()  -- overwrite a file, same as writeFile "/tmp/a.x" "ok"
writeFileOver = writeFile
  
{-|
    === Better name: write a list to file

    * write string to file
    <https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:writeFile writeFile>

    * Append string to file
    <https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:appendFile appendFile>

-}
writeFileList::FilePath->[String]->IO()
writeFileList p list = writeFile p $ unlines list

{-|
    === Char to word8, char to int

    * readFile to ByteString

    >ls <- BS.readFile "/tmp/b.x"
    >lsBS = BS.split (c2w_ '\n') ls
-}
c2w_::Char -> DW.Word8
c2w_ = BSI.c2w

{-|
    === Convert Integer to string, int to str

    >>> intToString 123
    "123"
-}
intToString::Integer->String
intToString n = show n

{-|
    === Convert Integer to string, int to str
    >>> integerToString 123
    "123"
-}
integerToString::Integer -> String
integerToString n = show n

{-|
    === Convert Integer to Int

    Use 'fromInteger'
-}
integerToInt::Integer -> Int
integerToInt = fromInteger




{-|
    === Int to digit char

    @
      intToCharDigit 3  => '3'
      intToCharDigit 31 => Exception
    @

    Use  'intToDigit'
-}
intToCharDigit::Int -> Char  -- 3 => '3',  31 => Error, See intToDigit
intToCharDigit = intToDigit


{-|
    === Extract a string from a string

    See 'extract' or 'Extract'

    > class Extract source where
    >   before :: Int -> source -> source
    >   after  :: Int -> source -> source
    >   extract:: (Int, Int) -> source -> source
    >
    > instance Extract String where
    >   extract :: (Int, Int) -> String -> String
    >
    > extractStr::(index, len) -> String  -> String

-}
extractStr::(Integer, Integer) -> String -> String -- extractStr (1, 2) "abc" == "bc", extractStr (1, 0) "abc" == ""
extractStr (a, b) s = extract (integerToInt a, integerToInt b) s

integerToBinary::Integer -> String
integerToBinary 0 = "0"
integerToBinary n =  reverse $ init $ g n
    where
        g::Integer -> String
        g 0 = "0"
        g n = let (q, r) = divMod n 2 in (integerToCharDigit r):(g q)



charToDecimal::Char -> Int
charToDecimal = ord

charToDecimalInteger :: Char -> Integer 
charToDecimalInteger = fi . ord

{-|
   === deprecated
   NOTE: confusing naming
   Monday, 24 July 2023 12:09 PDT
   NOTE: DO NOT USE any more
-}  
-- charToInt::Char -> Int  -- deprecated, USE: charToDecimal
-- charToInt c = fromEnum $ (c2w_ c) - (c2w_ '0')
  
{-|
   === deprecated
   NOTE: confusing naming
   Monday, 24 July 2023 12:09 PDT
   NOTE: DO NOT USE any more
-} 
-- charToInteger::Char -> Integer  -- deprecated, USE: charToDecimal
-- charToInteger c = toInteger $ fromEnum $ (c2w_ c) - (c2w_ '0')

charToIntX :: Char -> Int
charToIntX = ord

  
{-|
   KEY: char digit to Int
-} 
char0to9ToInt :: Char -> Int  -- char0to9ToInt '3' => 3 :: Int
char0to9ToInt c = case elem c ['0' .. '9'] of
                       True -> fromEnum $ (c2w_ c) - (c2w_ '0')
                       _    -> error $ "ERROR: literal Char is out of bound: c=" ++ [c]

  
{-|
   === deprecated
-}
integerToCharDigit::Integer -> Char  -- deprecated
integerToCharDigit n = intToCharDigit $ fromInteger n



{-|
    === Convert string to Integer, str to int, str to num, string to num

    * 'strToInteger'

    >stringToInteger "123"
    >123
    >
    >stringToInteger "a"
    >error

    * The Read class
    <https://hackage.haskell.org/package/base-4.12.0.0/docs/Text-Read.html#v:readPrec Text.Read>
-}
stringToInteger::String->Integer
stringToInteger s = read s::Integer

{-|
    === String to Integer

    alias of 'stringToInteger'
-}
strToInteger::String -> Integer
strToInteger = stringToInteger

{-|
    === KEY: Convert string to Integer, str to int, str to num, string to num, string to int

    * 'strToInt'

    >stringToInt "123"
    >123
    >
    >stringToInt "a"
    >error

    * 'Read' is a typeclass all the instance need to implement following method
    @
        strToInteger :: String -> Integer
        strToInteger s = foldr (+) zipWith(\x y -> (digitToInt x) * 10^10) (reverse s) [0..]

        class Read a where
            read :: String -> a

        instance Read Integer where
            read s = strToInteger s
    @

    > read "123"::Int

    <https://hackage.haskell.org/package/base-4.12.0.0/docs/Text-Read.html#v:readPrec Text.Read>
-}
stringToInt::String->Int
stringToInt s = read s::Int

{-|
    === String to Int, str to Int

    Same as 'stringToInt'
-}
strToInt::String -> Int
strToInt = stringToInt

{-|
    === KEY: pretty print

    @
        print tuple, print list, print two dimensions list
        print record
        print list of tuple
        use pPrint
    @

    > [("a", "b"), ("c", "d")]

    'Text.Pretty.Simple'
-}
pre::(CM.MonadIO m, Show a) => a -> m ()
pre s = TPS.pPrint s

prex::(CM.MonadIO m, Show a) => a -> m ()
prex s = TPS.pPrint s

-- Mon 13 Mar 12:28:03 2023 
-- Move to AronDevLib.hs due to current older resolver, see stack.yaml
-- SEE: /Library/WebServer/Documents/xfido/randomNote.txt
-- prex::(CM.MonadIO m, Show a) => a -> m ()
-- prex s = TPS.pPrintOpt TPS.CheckColorTty TPS.defaultOutputOptionsDarkBg {TPS.outputOptionsCompact = True} s

{-|
   === print 2d list
-}
prel::(Show a)=>[[a]] -> IO()
prel xxs  | length (L.nub [length xs | xs <- xxs]) /= 1 = error "ERROR: Not symmetric, Different length"
          | otherwise = mapM_ printRow xxs
              where
                printRow xs =  (mapM_ print_) xs >> putStrLn ""
                print_ x =  putStr $ (show x) ++ "\t"

{-|
    === use 'latin1' encoding to avoid error when reading non ASCII characters

    URL: https://hackage.haskell.org/package/base-4.18.0.0/docs/System-IO.html#t:TextEncoding

    FIXED: bug, read file contains unicode does not work properly.
    @
      -- Monday, 24 July 2023 15:47 PDT
      -- DO NOT use latin1

      latin1 :: TextEncoding
      utf8   :: TextEncoding

      -- SEE: https://hackage.haskell.org/package/base-4.18.0.0/docs/System-IO.html#t:TextEncoding

      Change:
      hSetEncoding latin1
      To
      hSetEncoding utf8
    @
-}
readFileLatin1ToList::FilePath -> IO [String] -- use 'latin1' encoding to avoid error when reading non ASCII characters
readFileLatin1ToList p = do
            h <- openFile p ReadMode
            hSetEncoding h utf8
            contents <- hGetContents h
            return $ lines contents

{-|
    === Read a file and return a String
-}
readFileList::FilePath -> IO [String] -- readFileList "/tmp"  , save as readFileLatin1ToList
readFileList = readFileLatin1ToList

readFileStr::FilePath -> IO String
readFileStr = readFile

{-|
    === read file data, covert each line to ['Double'] => [['Double']]

    @
    /tmp/x.x
    3.14 2.0
    1.0  2.0

    ls <- readFileDouble "/tmp/x.x"

    [[3.14, 2.0], 
     [1.0,  2.0]]
    @
-}
readFileDouble::FilePath -> IO [[Double]] -- "3.14 2.0" => [[3.14, 2.0]]
readFileDouble fp = do
  ls <- readFileList fp
  let lss = map(\x -> splitStr " " x) ls
  pre lss
  let dls = (map . map)(\x -> (read x)::Double) $  filter (\x -> len x > 0) $ (map . filter) (\x -> (len . trim) x > 0) lss
  return dls

{-|
    === read file data, covert each line to ['Float'] => [['Float']]

    @
    /tmp/x.x
    3.14 2.0
    1.0  2.0

    ls <- readFileFloat "/tmp/x.x"

    [[3.14, 2.0], 
     [1.0,  2.0]]
    @
-}
readFileFloat::FilePath -> IO [[Float]] -- "3.14 2.0" => [[3.14, 2.0]]
readFileFloat fp = do
  ls <- readFileList fp
  let lss = map(\x -> splitStr " " x) ls
  pre lss
  let dls = (map . map)(\x -> (read x)::Float) $  filter (\x -> len x > 0) $ (map . filter) (\x -> (len . trim) x > 0) lss
  return dls

{-|
    === read file data, covert each line to ['Int'] => [['Int']]

    @
    /tmp/x.x
    3  2
    1  2

    ls <- readFileInt "/tmp/x.x"

    [[3, 2], 
     [1, 2]]
    @
-}
readFileInt::FilePath -> IO [[Int]] -- "3 2" => [[3, 2]]
readFileInt fp = do
  ls <- readFileList fp
  let lss = map(\x -> splitStr " " x) ls
  pre lss
  let dls = (map . map)(\x -> (read x)::Int) $  filter (\x -> len x > 0) $ (map . filter) (\x -> (len . trim) x > 0) lss
  return dls

{-|
    >(round . (* 10^12)) <$> getPOSIXTime
-}
timeNowPico::IO Integer
timeNowPico = (round . (* 10^12)) <$> getPOSIXTime

{-|
    >(round . (* 10^9)) <$> getPOSIXTime
-}
timeNowNano::IO Integer
timeNowNano = (round . (* 10^9)) <$> getPOSIXTime

{-|
    >(round . (* 10^3)) <$> getPOSIXTime
-}
timeNowMicro::IO Integer
timeNowMicro = (round . (* 10^3)) <$> getPOSIXTime

{-|
    >(round . (* 10^6)) <$> getPOSIXTime
-}
timeNowMilli::IO Integer
timeNowMilli = (round . (* 10^6)) <$> getPOSIXTime

{-|
    Same as 'timeNowSec'

    >(round . (* 1)) <$> getPOSIXTime
-}
timeNowSecond::IO Integer
timeNowSecond = (round . (* 1)) <$> getPOSIXTime
  
{-|
    Same as 'timeNowSecond'
-}
timeNowSec::IO Integer
timeNowSec = timeNowSecond
  
  

{-|
    === get local time with TimeZone

    > getLocalTime
    2020-07-08 12:14:46.10179

    'LocalTime'

    >utcTime <- getCurrentTime
    >z <- getCurrentTimeZone
    >let utc = utcToLocalTime z utcTime
    >return utc
-}
getLocalTime::IO LocalTime -- 2020-07-08 12:14:46.10179
getLocalTime = do
   utcTime <- getCurrentTime
   z <- getCurrentTimeZone
   let utc = utcToLocalTime z utcTime
   return utc

{-|
    === KEY: get local date, get current time

    >"2019-05-27 12:57:41.520758 PDT"
-}
getLocalDate::IO String
getLocalDate = do
               ct <- getCurrentTime
               tz <- getTimeZone ct
               let localDate = utcToZonedTime tz ct
               return $ show localDate

{-|
    === KEY: get local date, get current time

    > s <- getLocalDate
    > putStr s
    > 2019-05-27 12:57:41.520758 PDT
-}
getDate::IO String
getDate = getLocalDate

{-|
    === KEY: get local date, get current time

    > s <- dateStr 
    > putStr s
    > 2019-05-27 12:57:41.520758 PDT
-}
dateStr::IO String
dateStr = getLocalDate

{-|
    === KEY: print local date, print current time

    Use 'getLocalDate'

    >"2019-05-27 12:57:41.520758 PDT"
-}
date::IO()
date = getLocalDate >>= \x -> putStrLn x


{-|
    === KEY: get local current time, local time, time zone
    __NOTE__ 'getCurrentTime' is UTC timezone only,

    'getTimeDay' time with day

    > return $ (show hour) ++ ":" ++ (show minute) ++ ":" ++ (show second)
-}
getTime::IO String
getTime = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let (TimeOfDay hour minute second) = localTimeOfDay $ utcToLocalTime timezone now
    -- Note: Second is of type @Pico@: It contains a fractional part.
    -- Use @fromIntegral@ to convert it to a plain integer.
    return $ (show hour) ++ ":" ++ (show minute) ++ ":" ++ (show second)

showTime::IO()
showTime = getTime >>= \x -> putStrLn $ (color Yellow) x



{-|
    === Trim, remove whitespace characters from either side of string.

    see 'trimWS' all whitespace
-}
trim::String->String -- trim ws from both sides
trim s  = TS.unpack $ TS.strip $ TS.pack s

{-|
    === KEY: split string, split str

    NOTE: DOES NOT Support String contains unicode

    @
    > splitStr "\25151" "abc\25151def"

    > splitStr "::" "dog::cat" => ["dog", "cat"]

    > let s = "\25151 abc \25151"
    "房 abc 房"
    > splitStr "\25151" s
    *** Exception: user error (Text.Regex.Posix.String died: (ReturnCode 13,"repetition-operator operand invalid"))
    > splitWhen

    > splitWhenFirstNoRegex "\25151" s
    Just (""," abc \25151")
    @

-}
splitStr::RegexStr -> String -> [String]  -- splitStr "::" "dog::cat" => ["dog", "cat"], splitStr "[ \t]" "a b c"
splitStr r s = splitRegex (mkRegex r) s

{-|
    === Partition string to [String] according to character class []

    @
    splitStrChar "[,.]" "dog,cat,cow.fox" => ["dog", "cat", "cow", "fox"]y
    splitStrChar::String->String->[String]
    splitStrChar r s = splitWhen(\x -> matchTest rex (x:[])) s
                where
                    rex = mkRegex r
    @

    * See 'splitStrCharTrim'

    >splitStrRegex => splitStrChar
-}
splitStrChar::RegexStr -> String -> [String]  -- splitStrChar "[,.]" "dog,cat,cow.fox" => ["dog", "cat", "cow", "fox"]
splitStrChar r s = splitWhen(\x -> matchTest rex (x:[])) s
                where
                    rex = mkRegex r

{-|
    === Split String. 'trim' and Remove empty String
    @
    splitStrCharTrim "[,.]" " dog,fox. " => ["dog", "fox"]
    @

    * See 'splitStrChar'
-}
splitStrCharTrim::RegexStr -> String ->[String]
splitStrCharTrim r s = filter (\x -> len x > 0) $ map trim $ splitWhen(\x -> matchTest rex (x:[])) s
                where
                  rex = mkRegex r

{-|
    === Match all pat from a given str
-}
matchAllBS::BS.ByteString -> BS.ByteString -> [(MatchOffset, MatchLength)]
matchAllBS pat str = join $ fmap DR.elems $ matchAll (makeRegex pat :: Regex) str

{-|
    === Better length function

    * Convert Int to polymorphic values
    * Convert Int to Num
    * fromIntegral::(Integral a, Num b)=> a -> b
 -}
len::(Foldable t, Num b)=>t a -> b
len a = fromIntegral $ length a

{-|
    === split key and value

    >splitStrTuple "="  "host = localhost" => (host, localhost)
    * TODO: fix the function if  host = dog = cat => ("host", "dogcat")
-}
splitStrTuple::String -> String -> (String, String)
splitStrTuple p s = (trim $ head $ splitStr p s, trim $ last $ splitStr p s)


logFile::FilePath -> [String] -> IO()  -- logFile "/tmp/f.x" ["Error"]
logFile = writeFileListAppend

logFile2::FilePath -> [String] -> IO()
logFile2 fp cs = do
    t <- getLocalDate
    let ls = map (\x -> x ++ "\t -- " ++ t) cs
    let h = replicate 80 '-'
    writeFileListAppend fp ([h] ++ ls)

logFileTmp::[String] -> IO()
logFileTmp cx = logFile2 fn cx
  where
    fn = "/tmp/f.x"

logFileNoName::[String] -> IO()  -- logFileNoName ["a"]  => $b/publicfile/global/globallog.txt
logFileNoName cs = do
    t <- getLocalDate
    fp <- getEnv "glog"
    let ls = map (\x -> x ++ "\t -- " ++ t) cs
    let h = replicate 80 '-'
    writeFileListAppend fp ([h] ++ ls)

logFileGEx::Bool -> String -> [String] -> IO()
logFileGEx b ks cs = do
  t <- getLocalDate
  let tabt = "\t -- " ++ t
  let ls = map (\x -> b ? x ++ tabt $ x) cs
  let h = replicate 80 '-'
  fp <- getEnv "glog"
  size <- getFileSize fp
  when (size > 2^24) $ do
    writeFile fp "reset"
  writeFileListAppend fp ([ks ++ tabt] ++ ls ++ [h])
  
logFileG::[String] -> IO()  -- log file to $glog  => $b/publicfile/global/globallog.txt
logFileG cs = logFileGEx True "" cs

logFileGT::String -> [String] -> IO()
logFileGT = logFileGEx True

logFileMat :: (Show a) => String -> [[a]] -> IO()
logFileMat s m = (cap . printMat) m >>= \x -> logFileGT s [x]

{--
logFileGEx::String -> [String] -> IO()
logFileGEx ks cs = do
  t <- getLocalDate
  let ls = map (\x -> x ++ "\t -- " ++ t) cs
  let h = replicate 80 '-'
  fp <- getEnv "glog"
  size <- getFileSize fp
  when (size > 2^24) $ do
    writeFile fp "reset"
  writeFileListAppend fp ([ks ++ "\n"] ++ ls ++ h)  
--}
  
logFileSBS2::FilePath -> [BS.ByteString] -> IO()  -- logFileSBS2 "/tmp/f.x" [toStr "Error"]
logFileSBS2 fp cs = do
    t <- getLocalDate
    let cs' = fmap toStr cs
    let ls = map (\x -> x ++ "\t -- " ++ t) cs'
    let h = replicate 80 '-'
    writeFileListAppend fp ([h] ++ ls)



{-|
    === KEY: binary, octedecimal, hexadecimal, int to hexadecimal

    See 'showIntAtBase' from 'Numeric'

    @
    showIntAtBaseX 2 intToDigit 10 ""  => binary
    showIntAtBaseX 8 intToDigit 10 ""  => octal
    showIntAtBaseX 10 intToDigit 10 "" => decimal
    showIntAtBaseX 16 intToDigit 10 "" => hexadecimal

    -- hexadecimal to decimal
    showIntAtBaseX 10 intToDigit (hexToInt "ff") ""
    @
-}
showIntAtBaseX::Integral a
                => a              -- base
                -> (Int -> Char)  -- digit to char
                -> a              -- number to show
                -> ShowS          -- String -> String
showIntAtBaseX base toDigit n rest
          | n < 0 = error "n can not be negative"
          | n' == 0 = rest'
          | otherwise = showIntAtBaseX base toDigit n' rest'
          where
            (n', d) = quotRem n base
            rest' = intToDigit (fromIntegral d) : rest


{-|
   === KEY: integer to hexadecimal
-}
integerToHex::Integer -> String
integerToHex 0 = "0"
integerToHex n =  reverse $ init $ g n
    where
        g::Integer -> String
        g 0 = "0"
        g n = let (q, r) = divMod n 16 in case (M.lookup r m3) of
                                                    Just x  -> x:(g q)
                                                    Nothing -> error "error"
            where
                ls = [(10, 'A'), (11, 'B'), (12, 'C'), (13, 'D'), (14, 'E'), (15, 'F')]
                m1 = map(\x -> (x, integerToCharDigit x)) [0..9]
                m2 = m1 ++ ls
                m3 = M.fromList m2


intToHex::Integer -> String  -- Use Numeric, showInt
intToHex n = showIntAtBaseX 16 intToDigit n ""

{-|
    === KEY: Integer to hexadecimal

integerToHex::Integer -> String  -- Use Numeric showInt
integerToHex n = showInt n ""
-}

{-|
    === KEY: hexadecimal to Integer
-}
hexToInt::String -> Integer  -- hexToInt "2206" => 8710
hexToInt s = sum ls
  where
    n = len s
    px = reverse $ take n [0..]
    hx = take n [16, 16..]
    co = map charToDecimal s
    ls = map fi $ zipWith3(\x y z -> x * y^z) co hx px


{-|
    === KEY: escape char to unescape char, int to unicode

    @
    "\\x2206" => "\x2206"
    @
-}
unescape :: String -> String
unescape str = map f (tail $ splitOn "\\x" str) where
  f s = chr (read ("0x"++s))

{-|
    === KEY: int to unicode

    @
      hex: \x2206 => ∆
    @
-}
intToUnicode::Int -> Char  -- putStrLn [chr . fi . intToUnicode $ 8710] => ∆
intToUnicode n = chr n

{-|
    === KEY: hexadecimal to unicode
-}
hexToUnicode::String -> Char  -- hexToUnicode "2206" => ∆
hexToUnicode s = intToUnicode . fi . hexToInt $ s

{-|
    === int to unicode, int to char

    See 'hexToUnicode' 'intToUnicode'
-}
intToChar::Int -> Char  -- putStrLn [chr . fi . intToChar $ 8710] => ∆
intToChar = intToUnicode

{-|
    === hexadecimal to Char or Unicode

    See 'hexToUnicode' 'intToChar'
-}
hexToChar::String -> Char  -- hexToChar "2206" => ∆
hexToChar = hexToUnicode

ρ::[a]-> Int
ρ cx = len cx

data FrameCount = FrameCount{
  frameTime::Integer,
  frameCount::Integer,
  frameNewCount::Integer,
  frameIndex::Int
  } deriving (Show, Eq)


resetRefFrame::IORef FrameCount -> IO()
resetRefFrame refFrame = do
    currTime <- timeNowMilli
    oldFrame <- readIORef refFrame
    let oldTime = frameTime oldFrame
    let oldCount = frameCount oldFrame
    let oldIndex = frameIndex oldFrame
    writeIORef refFrame FrameCount{frameTime = currTime, frameCount = 0, frameNewCount = 0, frameIndex = 0}

  


{-|
    === KEY: animate counter

  @

     (count, isNext, currRef) <- readRefFrame2 refCount 200

          refFrame <- (timeNowMilli >>= \x -> newIORef FrameCount{frameTime = x, frameCount = 1})

          (count, isNext, currRef) <- readRefFrame refFrame

          -- count   => start from 1 to 100, 1 <= count <= 100 and stop at 100
          -- isNext  => Bool, the delay is <= 20::Integer then True else False
          -- currRef => old Ref or updated Ref dependen on whether isNext is True or False

          when isNext $ do
            draw1
            draw2
  @
-}
readRefFrame2::IORef FrameCount -> Integer -> IO(Int, Bool, FrameCount)
readRefFrame2 refFrame timeDiff = do
  currTime <- timeNowMilli
  oldFrame <- readIORef refFrame
  let oldTime = frameTime oldFrame
  let oldCount = frameCount oldFrame
  let newCount = frameNewCount oldFrame
  let oldIndex = frameIndex oldFrame
  let isNext = (currTime - oldTime) > timeDiff
  if isNext then do
    let newIndex = oldIndex + 1
    let newFrame = FrameCount{frameTime = currTime, frameCount = newCount, frameNewCount = newCount, frameIndex = newIndex}
    return (newIndex, isNext, newFrame)
  else do
    return (oldIndex, isNext, oldFrame)

{-|
    === KEY: file size unit, Kilobyte or Magebyte NOT Gigabyte, Terabyte or Petabyte

    @
      fileSizeStrToNum 31M => 31 * 1024
      fileSizeStrToNum 20K => 20
    @
-}
fileSizeStrToNum::String -> Float  --  fileSizeStrToNum 31M => 31 * 1024
fileSizeStrToNum s = suffix == 'M' ? n * unit $ n
  where
    unit = 1024
    suffix = last s
    n = strToF $ init s


{-|
    === KEY: which in shell, which pdflatex
-}
whichGetPath::String -> IO FilePath
whichGetPath name = do
  (e, so, si) <- runShell $ "which " ++ name
  return $ (trim . toStr) $ if e /= ExitSuccess then error $ "ERROR:" ++ "which " ++ name else so


{-|
   === KEY: open file with UTF-8 encoding, read file with UTF-8 encoding

   <https://hackage.haskell.org/package/with-utf8-1.0.2.2/docs/src/Data.Text.IO.Utf8.html#readFile with-utf8>
   <https://hackage.haskell.org/package/with-utf8-1.0.2.2/docs/src/System.IO.Utf8.html#openFile with-utf8>
-}
openFileUtf8::CM.MonadIO m => FilePath -> IOMode -> m Handle
openFileUtf8 fp mode = CM.liftIO $ do
  h <- openFile fp mode
  hSetEncoding h utf8
  pure h

{-|
   === KEY: read file with UTF-8 encoding, readfile utf8

   <https://hackage.haskell.org/package/with-utf8-1.0.2.2/docs/src/Data.Text.IO.Utf8.html#readFile with-utf8>
   <https://hackage.haskell.org/package/with-utf8-1.0.2.2/docs/src/System.IO.Utf8.html#openFile with-utf8>
-}
readFileUtf8::CM.MonadIO m => FilePath -> m TS.Text
readFileUtf8 fp = openFileUtf8 fp ReadMode >>= CM.liftIO . TSO.hGetContents

{-|
    === scientific notation to normal notation

    * package: 'Numeric'
    * 'showFFloat' 'scientificToFloat'

    @
      scientificToFloat 3 3.1415e-2  -- 0.031
      sciToFloat 3 3.1415e-2  -- 0.031

      showFFloat (Just 3) 3.1415e-2 ""  -- 0.031
    @
-}
scientificToFloat::(RealFloat a) => Int -> a -> String  -- sciToFloat 3 3.1415e-2 ⇒ 0.031
scientificToFloat n v = showFFloat (Just n) v ""

{-|
    === scientific notation to normal notation

    * package: 'Numeric'
    * 'showFFloat' 'scientificToFloat'

    @
      scientificToFloat 3 3.1415e-2  -- 0.031
      sciToFloat 3 3.1415e-2  -- 0.031

      showFFloat (Just 3) 3.1415e-2 ""  -- 0.031
    @
-}
sciToFloat::(RealFloat a) => Int -> a -> String  -- sciToFloat  3  3.1415e-2 ⇒ 0.031
sciToFloat = scientificToFloat

{-|
    === KEY: Terminal color, term color, term background color, shell color, escape code sequence

    http://localhost/html/indexTerminalColorandEscapeCodeSequence.html

    @ 
    colorfgStr::Integer -> String -> String -- putStrLn $ colorfgStr 200 "Hello"
    colorfgStr n s = fg ++ color ++ s ++ reset
           where
             fg = "\x1b[38;5;"
             color = (show n) ++ "m"
             reset = "\x1b[0m"
    @

    @
      Use 256 Colors in terminal

      Set foreground color: \x1b[38;5; {color code}m \x1b[0m
      Set background color: \x1b[48;5; {color code}m \x1b[0m
                               |           |            |
                               |           |            |-> reset color
                               |           |-> RGB color (0-255)
                               |
                               |->  38 => foreground
                               |->  48 => background


      24 bit RGB (Truecolor)
      {r}    {g}       {b}
      8bits  8bits     8bits = 24bits

      32 bit RGBA
      {r}    {g}       {b}    {a}
      8bits  8bits     8bits  8bits

      2^10 = 1024
      2^5 = 32 x 32 = 940 + 64 = 1024
      2^24 = (2^10)^2 * 2^2 = 1024 x 1024 = 1M*2^2 = 4M

      Set foreground color: \x1b[38;2;{r};{g};{b}m\x1b[0m
      Set background color: \x1b[48;2;{r};{g};{b}m\x1b[0m
                               |           |            |
                               |           |            |-> reset color
                               |           |-> RGB color (0-255)
                               |
                               |->  38 => foreground
                               |->  48 => background


      putStrLn $ colorfgStr 200 "Hello"
    @

    SEE: 'color' 'System.Console.Pretty'
-}
colorfgStr::Integer -> String -> String -- putStrLn $ colorfgStr 200 "Hello"
colorfgStr n s = fg ++ color ++ s ++ reset
       where
         fg = "\x1b[38;5;"
         color = (show n) ++ "m"
         reset = "\x1b[0m"

{-|
    === KEY: Terminal color, term color, term background color, shell color, color background string

    >putStrLn $ colorbgStr 200 "Hello"

    @
    colorbgStr::Int -> String -> String -- putStrLn $ colorbgStr 200 "Hello"
    colorbgStr n s = bg ++ color ++ s ++ reset
           where
             bg = "\x1b[48;5;"
             color = (show n) ++ "m"
             reset = "\x1b[0m"

    -- Use AronToken
    let s = "( 1 + 2 = 3 )"
    putStr $ (concat . colorToken) $ tokenize s
    @
    SEE: 'AronToken' 
    SEE: 'tokenize' 'colorToken'
    SEE: 'colorfgStr'
    SEE: 'color' 'System.Console.Pretty'
-}
colorbgStr::Int -> String -> String -- putStrLn $ colorbgStr 200 "Hello"
colorbgStr n s = bg ++ color ++ s ++ reset
       where
         bg = "\x1b[48;5;"
         color = (show n) ++ "m"
         reset = "\x1b[0m"


{-|
   === KEY: pipe data to external process
  @
  ls = ["dog\n", "cat\n"]
             |
             |
             ↓
            pipe | createPipe
             |
             ↓
          External Process
          "alignmentStr -p kk"
             |
             ↓
          Handle = hout
             |
             ↓
          hGetContents hout
             |
             ↓
           putStr


    let ls = ["\"12\"", "\"34  \"", "\"5 6     \""]
    mayStOut <- createProcessPipeData "alignmentStr" ["-p", "kk"] ls
    case mayStOut of
         Just hout -> hGetContents hout >>= \x -> length x `seq` putStr x
         _        -> putStrLn "nothing"

 @
-}
createProcessPipeData::String -> [String] -> String -> IO (Maybe Handle, ExitCode) -- mayStOut <- createProcessPipeData "alignmentStr" ["-p", "kk"] ls
createProcessPipeData cmd opts str = do
    -- createPipe :: IO (Handle, Handle)
    (h_read, h_write) <- createPipe
    BS.hPut h_write $ toSBS str
    hFlush h_write
    hClose h_write
    -- createProcess :: CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
    (Nothing, Just hout, Nothing, ph) <- createProcess (proc cmd opts ){std_in  = UseHandle h_read
                                                                       ,std_out = CreatePipe
                                                                       }
    ec <- waitForProcess ph
    if ec == ExitSuccess
    then do
         -- hSetBuffering hout NoBuffering
         -- hSetBuffering hout NoBuffering
         -- hGetContents hout >>= \x -> length x `seq` putStr x
         return (Just hout, ec)
    else do
         print $ "ERROR:" ++ show ec
         exitFailure


{-|
    === KEY: write Strict Text to file
-}
writeFileSText::FilePath -> TS.Text  -> IO()  -- write Strict Text to file
writeFileSText fp strictText = do
  TSO.writeFile fp strictText

{-|
    === KEY: write Lazy Text to file
-}
writeFileLText::FilePath -> TL.Text -> IO()  -- write Lazy Text to file
writeFileLText fp lazyText = do
  TSO.writeFile fp $ TL.toStrict lazyText

{-|
    === KEY: padding zero to a list

    Thu  2 Nov 12:06:19 2023 
    NOTE: disable it because Arrow use the same operator
-}
{--
(>>>)::(Num a)=>Int -> a -> [a] -> [a]
(>>>) n c cx = let m = abs n in if n > 0 then (take n $ repeat 0) ++ cx else cx ++ (take m $ repeat 0)
--}


{-|
    === KEY: padding zero to a list

   padding left
   >pad 2 0 [1, 2]

   padding right
   >pad (-2) 0 [1, 2]
-}
pad::(Num a)=> Int -> a -> [a] -> [a]  -- pad 2 0 [1, 2] => [0,0,1,2] , pad (-2) 0 [1, 2] => [1,2,0,0]
pad n c cx = let m = abs n in if n > 0 then (take n $ repeat 0) ++ cx else cx ++ (take m $ repeat c)

{-|
   === KEY: list of zero
-}
nzero::(Num a)=> Int -> [a]  -- nzero 2 => [0, 0]
nzero n = take n $ repeat 0

{-|
   === KEY: shift each row of matrix to left and padding with zero

  @
   let m = [[1, 2, 3]
           ,[3, 4, 5]
           ,[6, 7, 8]]
   let shiftMat = shiftMatrixLeft 3 m
  @
-}
shiftMatrixLeft::(Num a) => Int -> [[a]] -> [[a]]
shiftMatrixLeft n cx = zipWith(\x r ->  (nzero $ n - 1 - x) ++ r ++ (nzero x) ) [0..n-1] cx

{-|
    --    data System.Console.Pretty.Color
    --      = Black
    --      | Red
    --      | Green
    --      | Yellow
    --      | Blue
    --      | Magenta
    --      | Cyan
    --      | White
    --      | Default
    --
-}
shellHighlight::String -> [String]
shellHighlight s = s19
    where
        repList = [
                    ("sed",      (color Red "\\0")    ),
                    ("grep",     (color White "\\0")  ),
                    ("awk",      (color Red "\\0")    ),

                    ("Int",      (color Green "\\0")  ),
                    ("Integer",  (color Green "\\0")  ),
                    ("String",   (color Green "\\0")  ),
                    ("Char",     (color Green "\\0")  ),
                    ("Bool",     (color Green "\\0")  ),

                    ("void",     (color Blue "\\0")   ),
                    ("int",      (color Blue "\\0")   ),
                    ("string",   (color Blue "\\0")   ),
                    ("char",     (color Blue "\\0")  ),
                    ("bool",     (color Blue "\\0")  )
                   ]

        repWord = searchReplaceWord  -- word only
        repAny  = searchReplaceAnyTup  -- any String
        s00 = lines s
        s01 = map (\x -> repAny x ("\\[",      (color White "-LLLL-") )) s00 
        s02 = map (\x -> repAny x ("\\]",      (color White "-RRRR-") )) s01
        s03 = map (\x -> repAny x  ("->",       (color White "\\0") )) s02
        s04 = map (\x -> repAny x  ("=>",       (color Red "\\0")   )) s03
        s05 = map (\x -> repAny x  ("::",       (color Red " \\0 ") )) s04

        s050 = lines $ searchReplaceListWord repList (unlines s05)

        s14 = map (\x -> repAny x ("\\(",      (color Cyan "\\0")   )) s050 
        s15 = map (\x -> repAny x ("\\)",      (color Cyan "\\0")   )) s14
        s16 = map (\x -> repAny x ("<",        (color Yellow "\\0") )) s15
        s17 = map (\x -> repAny x (">",        (color Yellow "\\0") )) s16
        s18 = map (\x -> repAny x ("-LLLL-",   (color White "[")      )) s17
        s19 = map (\x -> repAny x ("-RRRR-",   (color White "]")      )) s18


{-|
    === KEY: extra BEG_xxx and END_xxx block from a file, extra code block, begin block, end block

    @

    let f = containStr "BEG_"
    let g = containStr "END_"

    b <- getEnv "scr"
    let p = b </> "AronLib.sh"
    ls <- readFileList p
    pre $ blockBegEnd f g ls

    @
 -}
blockBegEnd::(String -> Bool) -> (String -> Bool) -> [String] -> [[String]]
blockBegEnd f g cx = filter (\x -> len x > 0) $ blockPair f g cx
    where
        blockPair::(String -> Bool) -> (String -> Bool) -> [String] -> [[String]] 
        blockPair f g [] = []
        blockPair f g cx = s3 : (blockPair f g s5) 
            where
                b = "BEG_"
                e = "END_"
                s1 = dropWhile (\x -> not $ containStr b x) cx  -- "BEG_" "a" "END_" => "BEG_" "a" "END_"
                s2 = dropWhile (\x -> containStr b x) s1        -- "a" "END_" 
                s3 = if hasInList g s2 then takeWhile (\x -> not $ containStr e x) s2 else []  -- "a"
                s4 = dropWhile (\x -> not $ containStr e x) s2  -- "END_"
                s5 = dropWhile (\x -> containStr e x) s4
        
        hasInList  :: (String -> Bool) -> [String] -> Bool
        hasInList f cx = (len $ filter f cx) > 0


{-|
    KEY:  pick from a list

    @
    let (optls, numls) = pick (hasStr "-") ["-n", "-f", "10", "1", "5"] 
    optls = ["-n", "-f"
    numls = ["10", "1", "5"]
    @

-}
pick::(a -> Bool) -> [a] -> ([a], [a])
pick f [] = ([], [])
pick f cx = (s2, s3) 
    where
        s1 = map f cx  -- [True, False]
        s2 = map snd $ filter (\(a, _) -> a == True) $ zip s1 cx  -- [(True, a), (True, a)]
        s3 = map snd $ filter (\(a, _) -> a /= True) $ zip s1 cx  -- [(True, a), (True, a)]
        
{-|
    KEY: Check cx within cy

    @ 
    containAll [1, 2], [1, 2, 3] => True
    @
-}
containAll::(Ord a)=>[a] -> [a] -> Bool
containAll [] [] = False
containAll cx cy = t 
    where
        s = map (\x -> elem x cy) cx
        t = foldl (\x y -> x && y) True s

{-|
    KEY: m choose n, combination

    URL: https://wiki.haskell.org/99_questions/Solutions/26

    L.tails = mytails

    @

    mytails :: [a] -> [[a]]
    mytails [] = [[]]
    mytails (x:cx) = (x:cx):(mytails cx)

    mytails [1, ll2, 3]
    [[1, 2, 3], [2, 3], [3]]
   
    combin 2 [1, 2, 3]
    [[1,2],[1,3],[2,3]]

    tails [1, 2, 3]
    [1, 2, 3], [2, 3], [3], []


    Explanation:
    1 2 3 4

    1 2 3 4      2 3 4        3 4         4           []
    1 (c(n-1))   2 (c(n-1))   3(c(n-1))   4(c(n-1))   
      2 3 4        3 4          4
        3 4          4
          4
      
    @
-}
combin::Int -> [a] -> [[a]]
combin 0 _  = [[]]
combin n cx = [ x:cy | x:cx' <- mytails cx
                      ,cy <- combin (n-1) cx']
  where  
   mytails :: [a] -> [[a]]
   mytails [] = [[]]
   mytails (x:cx) = (x:cx):(mytails cx)
  
  
drawRectBar::(Pixel px, PrimMonad m) => MutableImage(PrimState m) px
    ->(Int, Int) -> (Int, Int) -> Int ->[Int] -> px -> m()
drawRectBar m (x1, y1) (x2, y2) h cs px = do
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

  
{-|
    === KEY: draw histogram from a file

    @
    /tmp/x.x
    1
    3 
    9
    20
    14

    histogram /tmp/x.x
    @
-}
histogram::FilePath -> IO()
histogram fp = do
        nls <- readFileList fp >>= \cx -> return $ map (\x -> read x ::Int) cx
        let w = 2000
            h = 1000

        img <- withMutableImage w h (PixelRGB8 10 10 10) $ \m -> do
            ls <- randIntList 100 (1, 100)
            drawRectBar m (10, 20) (20, 30) 100 nls (PixelRGB8 0 255 0)
            -- A dark green filled triangle
            -- fillTriangle m 50 200 250 300 70 350 (PixelRGB8 0 150 50)
        writePng "/tmp/histogram.png" img
        print "gene png file => /tmp/histogram.png"


{-|
   KEY: insert to the next index 

   SEE 'removeIndex'
-}
insertIndexAt::Int -> a -> [a] -> [a]  -- insertIndexAt 0 "dog" ["a", "b"] => ["a", "dog", "b"]
insertIndexAt x e cx = left ++ [e] ++ right 
    where
        left = take (x + 1) cx            
        right = drop (x + 1) cx


{-|
    KEY: Check str is in the list
-}
isInList :: String -> [String] -> Bool  -- isInList "a" ["a ", "b"] ⟹   True
isInList s cx = bo 
    where
        ls = filter (\x -> len x > 0) $ map (lowerStr . trim) cx 
        set = DS.fromList ls
        bo = DS.member s set


{-|
    KEY: remove the FIRST match from a list
    NOTE: Better name
-}
removeFirstList::(Ord a) => a -> [a] -> [a]
removeFirstList s cx = left ++ right 
    where
        left = takeWhile (\x -> x /= s) cx
        ls = dropWhile (\x -> x /= s) cx
        right = null ls ? [] $ tail ls 

{-|
    NOTE: Deprecated, bad name
    USE: 'removeFirstList'
-}
removeFromList::(Ord a) => a -> [a] -> [a]
removeFromList s cx = left ++ right 
    where
        left = takeWhile (\x -> x /= s) cx
        ls = dropWhile (\x -> x /= s) cx
        right = null ls ? [] $ tail ls 


{-|
    KEY: read file 2d string to integer
    
    Write APL data to file and read it from it

    @
    m ← 3 3 3 ⍴ 10 ? 10
          m
     8 10  5
     2  3  1
     7  4  9
            
     6  8 10
     5  2  3
     1  7  4
            
     9  6  8
    10  5  2
     3  1  7

    (⊂↓⎕FMT m) ⎕NPUT '/tmp/x'
    ⎕cmd 'cat /tmp/x'

    File:
    1 2 3
    3 4 5

    3 4 5
    3 3 9 

    => [[Integer]]
    @
-}
readFileInteger2d::FilePath -> IO[[Integer]]  -- Read APL data
readFileInteger2d fp = do
    s <- readFileList fp >>= \str -> return $ map (splitSPC) str 
    let s' = filter (\e -> len e > 0) $ map (\ls -> filter (\a -> len a > 0) ls) $ (map . map) (trim) s
    let ns = (map . map) (\x -> read x :: Integer) s'
    pre ns
    return ns 
  

{-|
    KEY: read file 2d string to Float 
    
    Write APL data to file and read it from it

    @

    mf ← 3 3 3 ⍴ ? 10 ⍴ 0
          mf
    0.875658736   0.4130345348  0.7661493221 
    0.05092898876 0.7344701699  0.5883291622 
    0.08133906822 0.6499611783  0.203398563  
                                             
    0.7316870476  0.875658736   0.4130345348 
    0.7661493221  0.05092898876 0.7344701699 
    0.5883291622  0.08133906822 0.6499611783 
                                             
    0.203398563   0.7316870476  0.875658736  
    0.4130345348  0.7661493221  0.05092898876
    0.7344701699  0.5883291622  0.08133906822

    (⊂↓⎕FMT mf)⎕NPUT'/tmp/y'

    File:
    1.1 2.0 3.1
    2.1 3.3 3.3

    2.1 3.3 3.3

    => [[Float]]
    @ 
-}
readFileFloat2d::FilePath -> IO[[Float]]  -- Read APL data
readFileFloat2d fp = do
    s <- readFileList fp >>= \str -> return $ map (splitSPC) str 
    let s' = filter (\e -> len e > 0) $ map (\ls -> filter (\a -> len a > 0) ls) $ (map . map) (trim) s
    let ns = (map . map) (\x -> read x :: Float) s'
    pre ns
    return ns 

{-|
    === Read table like string from a file 
-}
readTable::FilePath -> IO [[String]]
readTable fp = readFileList fp >>= return . alignTable 


{-|
    === KEY: alignment, column table, align column, format table

    @
    table 1:
    "a  b   c"
    "e f  g"
      ↓ 

    table 1:
    "a" "b" "c"
    "e" "f" "g"

    @
-}
alignTable::[String] -> [[String]]
alignTable cx = if isSameCol then 
                    let ls' = tran lu 
                        ms = map (\r -> ml r ) $ (map . map) len ls'
                        pad n s = let d = n - len s in s ++ replicate d ' '
                        m = map (\r -> let n = ml $ len <$> r in map(\s -> pad (n + 1) s) r) ls'
                        ml s = foldr (\a b -> max a b) 0 s
                    in tran m
                else error "Invalid table format: Row or Column has different items"
    where
        lu = filter (\u -> len u > 0) $ map (\t -> filter (\x -> (len . trim) x > 0) $ splitSPC t) cx
        lr = map len lu
        isSameCol = foldr (\a b -> a && b) True $ map (\e -> e == x) lr where x = head lr 

{-|
    === KEY: alignment, column table, align column, format table

    @
    table 1:
    "a  b   c"
    "e f  g"
      ↓ 

    table 1:
    "a" "b" "c"
    "e" "f" "g"
    @
-}
columnTable :: [String] -> [[String]]
columnTable = alignTable


{-|
    === trim list of String

    @
    ["", "", "a", " b ", " "] => ["a", "b"]
    @
-}
trimList :: [String] -> [String] 
trimList ls = map strictTextToStr $ trimListST $ map strToStrictText ls 

--trimList::[String] -> [String]  -- ["", "", "a", " b ", " "] => ["a", "b"]
--trimList lss = let f x =  dropWhile null $ map trim x 
--                   rev = reverse
--               in  (rev . f . rev . f) lss 

{-|
    === trim list of strict Text 

    'Data.Text'

    @
    ["", "", "a", " b ", " "] => ["a", "b"]
    @
-}
trimListST :: [TS.Text] -> [TS.Text]  -- ["", "", "a", " b ", " "] => ["a", "b"]
trimListST lss = (rev . g . rev . g) lss
    where
        g ::[TS.Text] -> [TS.Text]
        g cx = dropWhile TS.null $ map TS.strip cx 
        rev = reverse

{-|
    === KEY: Generate a step list 

    SEE: 'splitWhenFirstNoRegex' and 'splitWhenLastNoRegex'

    @
        stepList "abcd" 2 ⟹  
        [
            ( "ab"
            ,
                [ 0
                , 1
                ]
            )
        ,
            ( "bc"
            ,
                [ 1
                , 2
                ]
            )
        ,
            ( "cd"
            ,
                [ 2
                , 3
                ]
            )
        ]
    @
-}
stepList:: [a] -> Int -> [([a], [Int])]
stepList cx n = step cx n
        where
            step cx n = let bt = zip cx [0..] 
                            lv = take (len cx - (n - 1)) $ f n bt 
                            s  = (map . map) fst lv
                            r  = (map . map) snd lv
                            -- g m x = take m x : (f m $ rotateLeft 1 x)
                            f k x = take k x : (f k $ rotateLeft 1 x)
                        in zip s r 

{-|
    === KEY: split a string without Regex, String can contain unicode 

    * Use 'stepList' 'splitStr' DOES NOT support String contains unicode
    
    @
    > splitWhenFirstNoRegex "@=" "\25151 abc@=123@=456"
    Just ("\25151 abc","123@=456")


    > splitStr "\25151" "abc\25151def"

    > let s = "\25151 abc \25151"
    "房 abc 房"

    > splitStr "\25151" s
    *** Exception: user error (Text.Regex.Posix.String died: (ReturnCode 13,"repetition-operator operand invalid"))
    > splitWhen

    > let s = "\25151 abc \25151"
    > splitWhenFirstNoRegex "\25151" s
    Just (""," abc \25151")

    @
-}
splitWhenFirstNoRegex :: String -> String -> Maybe (String, String)  -- splitWhenFirstNoRegex "@=" "\25151 abc@=123@=456"
splitWhenFirstNoRegex pat cx = null ls ? Nothing $ Just (take n1 cx, drop (n2 + 1) cx)
        where
            ls = filter (\x -> fst x == pat) $ stepList cx (len pat)
            h = head ls 
            n1 = head $ snd h
            n2 = last $ snd h

{-|
    === KEY: split a string without Regex, String can contain unicode

    * Use 'stepList'
    
    @
    > splitWhenLastNoRegex "@=" "\25151 abc@=123@=456"
    Just ("\25151 abc@=123","456")
    @
-}
splitWhenLastNoRegex :: String -> String -> Maybe (String, String)  -- splitWhenLastNoRegex "@=" "\25151 abc@=123@=456"
splitWhenLastNoRegex pat cx = null ls ? Nothing $ Just (take n1 cx, drop (n2 + 1) cx)
        where
            ls = filter (\x -> fst x == pat) $ stepList cx (len pat)
            h = last ls 
            n1 = head $ snd h
            n2 = last $ snd h

{-|
    === KEY: Split String to a list

    @
        pre $ splitStrCharNoRegex "\NUL" "123\NULabcd\0efgh\NUL"

        [ "123"
        , "abcd"
        , "efgh"
        ]

        pre $ splitStrCharNoRegex "\0" "123\NULabcd\0efgh\NUL"

        [ "123"
        , "abcd"
        , "efgh"
        ]
    @ 
-}
splitStrCharNoRegex :: String -> String -> [String]  -- splitStrCharNoRegex "\0" "123\NULabc\0def\NUL"
splitStrCharNoRegex _ [] = []
splitStrCharNoRegex pa s = case splitWhenFirstNoRegex pa s of
                                    Just (a, rest) -> a : splitStrCharNoRegex pa rest 
                                    Nothing -> s : splitStrCharNoRegex pa [] 

{-|
    === withFile, print file
-}
printFile :: FilePath -> IO()
printFile p = do
        _ <-  withFile "/tmp/ab.x" ReadMode $ \h -> do
                   ls <- hGetContents h >>= return . lines 
                   mapM_ putStrLn ls 
        return ()

{-|
    === KEY: count the frequence of items 

    SEE: 'L.groupBy'
    SEE: 'L.groupBy . sort'
    @
        frequenceCount ["a", "b", "a", "b", "c"]
        [("a",2),
         ("b",2),
         ("c",1)]

        frequenceCount [1, 2, 1, 3] 
        [(1, 2),
         (2, 1),
         (3, 1)]
    @
-}
frequenceCount::(Ord a) => [a] -> [(a, Int)]  -- frequenceCount ["a", "b", "b"] => [("a", 1), ("b", 2)]
frequenceCount cx = map (\x -> (head x, len x)) $ L.groupBy (\a b -> a == b)  $ sort cx 

-- KEY: fileblock  
-- NOTE: $sp/fileBlock
data FileBlock =  DropHead
             | DropLast
             | DropIndex Int
             | PrintIndex Int
             | PrintHead
             | PrintLast
             | PrintAll
             | Append String
             | AppendList [String]
             | Prepend String
             | NumberOfBlock deriving (Show, Eq)

{-|
  KEY:
  Monday, 17 July 2023 14:55 PDT
  FIXED: Make delimiter and separator are the same.
-}
fileBlock :: FilePath -> String -> FileBlock -> IO()
fileBlock path de opt = do
  fs <- fileExist path >>= \x -> not x ? (createFile path >> return []) $ readFileListStrict path
  let fs' = rev $ dropWhile (\x -> (len . trim) x == 0) $ rev fs
  let separator = concat $ take 10 $ repeat de
  -- let delimiter = len fs' > 0 ? last fs' $ separator
  let delimiter = separator
  let ls = filter (\x -> len x > 0 ) $ splitBlock fs' separator
  -- IF the last block:[Str] of ls:[[Str]] is empty THEN remove it ELSE do nothing
  -- [[]]    => []
  -- [["a"]] => [["a"]]
  let ls' =  len ls == 0 ? ls $ let s = concat $ map trim $ last ls in (len s == 0 ? init ls $ ls) 

  case opt of
    DropHead -> do
      let ss = map (\x -> trimList x ++ [delimiter] ) ls'
      let s = len ss > 0 ? tail (map (trim . unlines) ss) $ [] 
      wfl path s
    DropLast -> do
      let ss = map (\x -> trimList x ++ [delimiter] ) ls'
      let s = init $ map (trim . unlines) ss 
      wfl path s
    DropIndex inx -> do
      let ss = map (\x -> trimList x ++ [delimiter] ) ls'
      let st = removeIndex inx $ map (trim . unlines) ss
      wfl path st
    PrintIndex inx -> do
      let ss = map trimList ls'
      let st = 0 <= inx && inx < len ss ? ss !! inx $ error ("ERROR: len =" ++ (show . len) ss ++ " index=" ++ show inx)
      putStr $ unlines st
    PrintHead -> do
      let ss = map trimList ls'
      let st = len ss > 0 ? head ss $ error ("ERROR: len =" ++ (show . len) ss)
      putStr $ unlines st
    PrintLast -> do
      let ss = map trimList ls'
      let st = len ss > 0 ? last ss $ error ("ERROR: len =" ++ (show . len) ss)
      putStr $ unlines st
    PrintAll -> do
      let ss = map trimList ls'
      mapM_ (putStrLn . unlines) ss

    Append str -> do
      -- Remove the first and last empty lines str contains them 
      -- ss = [String]
      let ss = let s = map trimList $ [lines str] in if len s == 0 then [] else head s
      -- Remove the str first if there is str in the list
      let lu = removeFirstList ss $ filter (not . null) $ map trimList ls' 
      -- let la = unique $ filter (not . null) $ map trimList $ ls' ++ [lines str] 
      let la = lu ++ [ss]
      let tt = map (trim . unlines) $ map (++[delimiter]) la 
      wfl path tt 

    AppendList ls -> do
      let la = uniqueOrder $ map trimList $ ls' ++ [ls]
      let tt = map (trim . unlines) $ map (++[delimiter]) la 
      wfl path tt 

    Prepend str -> do
      let ss = let s = map trimList $ [lines str] in if len s == 0 then [] else head s
      -- Remove the str first if there is str in the list
      let lu = removeFirstList ss $ filter (not . null) $ map trimList ls' 
      -- let la = unique $ filter (not . null) $ map trimList $ ls' ++ [lines str] 
      let la = [ss] ++ lu
      let tt = map (trim . unlines) $ map (++[delimiter]) la 
      wfl path tt 
    NumberOfBlock -> do
      putStr $ (sw . len) ls'
  where
    wfl = writeFileList
    rev = reverse
    sw = show

{-|
    KEY: split prefix
    @ 
       let t@(h, t) = splitPrefix (\x -> x /= ' ') ":abcd efgh ijkl mnop qrst uvwx yz" 
       (":abcd"," efgh ijkl mnop qrst uvwx yz")

    NOTE:break == splitPrefix
         break (== ' ') ":ab c" => (":ab", " c")
    @
 -}
splitPrefix :: (a -> Bool) -> [a]  -> ([a], [a])  -- splitPrefix (x -> x /= ' ') ":ab cd" => (":ab", " cd")
splitPrefix f s = (h, t) 
    where
        h = takeWhile f s 
        t = dropWhile f s

breakFirst :: (a -> Bool) -> [a] -> ([a], [a])
breakFirst = break

{-|
  === KEY: grep, iostream
  NOTE: does not work inside GHCi
  URL: https://www.reddit.com/r/haskellquestions/comments/154y4x7/comment/jsyv1e9/?context=3
-}
grepx :: BS.ByteString -> FilePath -> IO ()
grepx pattern file = withFile file ReadMode $ \h -> do
    is <- Streams.handleToInputStream h >>=
          Streams.lines                 >>=
          Streams.filter (S8.isInfixOf pattern)
    os <- Streams.unlines Streams.stdout
    Streams.connect is os
  
{-|
  
 === KEY: it is based on stream library
 https://hackage.haskell.org/package/io-streams-1.5.2.2/docs/System-IO-Streams-Tutorial.html
 NOTE: does not work so far
 URL: https://www.reddit.com/r/haskellquestions/comments/154y4x7/comment/jsyv1e9/?context=3
-}
grepLine :: FilePath -> (String -> Bool) -> IO()  -- grepLine (hasStr "abc") "/tmp/a.x"
grepLine fp f = withFile fp ReadMode $ \h -> do
                 is <- Streams.handleToInputStream h >>=
                   Streams.lines >>=
                   Streams.filter f'
                 os <- Streams.unlines Streams.stdout
                 Streams.connect is os
    where
      f' x = f $ toStr x

catx :: FilePath -> IO ()
catx file = withFile file ReadMode $ \h -> do
    is <- Streams.handleToInputStream h
    Streams.connect is Streams.stdout

{-|
    === KEY: clipboard, pbcopy, pbpaste, copy and paste
-}
clipBoardcp :: String -> IO ()
clipBoardcp s = do
    let cmd = [r| echo '|] <> s <> [r|' | pbcopy|]
    _ <- system cmd 
    return () 

pbcopy :: String -> IO () 
pbcopy = clipBoardcp

clipBoardpa :: IO [String] 
clipBoardpa = runCmd [r|pbpaste|] 

pbpaste :: IO [String] 
pbpaste = clipBoardpa

{-|
   === KEY: ghci read line

   * read line inside GHCi

   NOTE: CTRL-D => GHCi => \EOT

   SEE: https://stackoverflow.com/questions/56219779/how-can-i-read-mutiple-lines-from-stdin-within-ghci-more-than-once
-}
getContentsCTRLD :: IO String;
getContentsCTRLD = do
                   getChar >>= \c -> 
                     if c == '\EOT'   -- CTRL D
                         then return "" 
                         else getContentsCTRLD >>= \s -> 
                         return (c:s)
    
{-|
 
   @
       print $ hasSubstr "ab" "xxabb" == True
       print $ hasSubstr "axb" "xxabb" == False 
       print $ hasSubstr "" "xxabb" == True 
       print $ hasSubstr "xab" "xxabb" == True
   @
 -}
hasSubstr :: String -> String -> Bool
hasSubstr a s = DS.member a set 
    where
        set = DS.fromList $ (let x = substr (len a) s in x == [] ? [] $ head x)


{-|
    @
    hasStrBlock "abc" ["abc", "kk", "--", "aaa"]
    [["abc", "kk"] ["aaa"]]
    @
 -}
hasStrBlock :: String -> [String] -> [[String]]
hasStrBlock a s = filter (\x -> f a x) s1 
            where
              low = toLowerStr 
              hasSubstrL x cx = let x' = low x
                                    cx' = low cx
                                in hasSubstr x' cx'
              s1 = splitListWhen(\x -> trim x == "--" || trim x == "---") s
              f x ss = foldr (\e1 e2 -> e1 || e2) False $ map (\e -> hasSubstrL x e) ss
  
{-|
     === KEY: power set, powerset

     @
     [] => [[]]
     [1] => (x:[]) [[1]] ++ [[]] => [[], [1]]
     [1,2] => (1:[2]) => map (1:) $ [[],[2]) ++ [[], [2]]
                  => [[1],[1,2]] ++ [[], [2]]
                  => [[], [1], [2], [1,2]]
     @
-}
powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x:cx) = map (x:) (powerSet cx) ++ powerSet cx

{-|
    KEY: swap list 
-}
swap :: (Int, Int) -> [a] -> [a]
swap  t@(x, y) cs = b1 && b2 ? cs2 $ error $ "Invalid index=" ++ (show t)
    where
      b1 = x >= 0 && x < len cs 
      b2 = y >= 0 && y < len cs 
      l = cs !! x
      r = cs !! y
      cs1 = replace1d x (\_ -> r) cs
      cs2 = replace1d y (\_ -> l) cs1
  
{-|
  KEY: print array, print 2d array

  @
  let a = 1; b = 2; c = 3
  ls <- DAO.newListArray ((0, 0, 0), (a, b, c)) [1..((a + 1) * (b + 1)) * (c + 1)] :: IO(IOArray (Int, Int, Int) Int)
  printMat3 ls
  @
-}
printMat3 :: (Show a) => IOArray (Int, Int, Int) a -> IO()
printMat3 arr = do
  ls <- DAO.getAssocs arr
  ((z0, y0, x0), (z1, y1, x1)) <- getBounds arr
  let lz = z1 - z0 + 1
  let ly = y1 - y0 + 1
  let lx = x1 - x0 + 1
  mapM_ (\s -> do printMat s; fw "") $ partList ly $ partList lx ls
  
{-|

  KEY: print array, print 3d array

  @
  let a = 2; b = 3
  ls <- DAO.newListArray ((0, 0), (a, b)) [1..((a + 1) * (b + 1))] :: IO(IOArray (Int, Int) Int)
  printMat2 ls
  @
-}  
printMat2 :: (Show a) => IOArray (Int, Int) a -> IO()
printMat2 arr = do
  ls <- DAO.getAssocs arr
  ((y0, x0), (y1, x1)) <- getBounds arr
  let ly = y1 - y0 + 1
  let lx = x1 - x0 + 1
  printMat $ partList lx ls

{-|

  KEY: capture stdout from print

  @
  `System.IO.Silently` from 'silently' package

  fun :: IO()
  fun = print "capture string"

  str <- capture_ $ fun
  print str
  @
  
-}
cap :: IO a -> IO String
cap = capture_

{-|
   KEY: read file and convert to Haskell type

   @
     (x, y) <- readAndParse "/tmp/a" :: IO (Float, Float)
   @
-}
readAndParse :: (Read a) => FilePath -> IO a
readAndParse fn = read <$> readFile fn

readMaybeParse :: (Read a) => FilePath -> IO (Maybe a)
readMaybeParse fn = readMaybe <$> readFile fn


{-|


  NOTE: The runtime is very slow
  SEE: <http://localhost/html/indexMatrixmultiplicationruntime.html#org6cb17c3 RunTime>

  @
   -- https://www.haskell.org/tutorial/arrays.html

   let ln = 500
   m1 <- geneRandMat (ln, ln) <&> join
   let ar1 = array ((0, 0),(ln - 1, ln - 1)) [((i, j), let ix = i * ln + j in m1 !! ix)
                                              | i <- range (0, ln - 1),
                                                j <- range (0, ln - 1)]

   let a0 = array ((0, 0), (1, 1)) [((i, j), i + j) | i <- range (0, 1), j <- range (0, 1)]
   let a1 = array ((0, 0), (1, 1)) [((i, j), i + j) | i <- range (0, 1), j <- range (0, 1)]
   let m = multiMatArr a0 a1
   print a0
   fw ""
   print a1
   fw "m"
   print m
   fw "ar1 x ar1"
   old <- timeNowMicro
   let m2 = multiMatArr ar1 ar1
   wfl "/tmp/x.x" $ map show $ elems m2
   new <- timeNowMicro
   let diff = new - old
   fw "diff="
   print diff
  @

-}
multiMatArr :: (Ix a, Ix b, Ix c, Num n) => DR.Array (a, b) n -> DR.Array (b, c) n -> DR.Array (a, c) n
multiMatArr a1 a2 = DR.array resultBnd [ ((i, k), sum [a1 DR.! (i, j) * a2 DR.! (j, k) | j <- range (x1, y1)]) 
                                       | i <- range (x0, y0), 
                                         k <- range (x1', y1')
                                    ]
  where
    ((x0, x1),  (y0,  y1) ) = DR.bounds a1
    ((x0',x1'), (y0', y1')) = DR.bounds a2
    resultBnd | (x1, y1) == (x0', y0') = ((x0, x1'),(y0, y1'))
              | otherwise = error "ERROR: dim of matrices are NOT compatible"


compareArray :: [Int] -> [Int] -> Int
compareArray xx@(x:y:_) yy@(x':y':_) | x == 1 && y == 0 = error "ERROR: Invalid format 1"
                            | x' == 1 && y' == 0 = error "ERROR: Invalid format 2" 
                            | x == x' && x == 0 && y == y' && y == 0 = 0
                            | x == 0 && x' == 1 = 1
                            | x == 1 && x' == 0 = -1
                            | x == 0 && x' == 0 = f xx yy
                            | x == 1 && x' == 1 = negate $ f xx yy
                            | otherwise = error "ERROR: compareArray"
  where
    dropWith cx cy = dropWhile (== 0) $ zipWith (\a b -> a == b ? 0 $ (a > b ? 1 $ -1)) cx cy
    f cx cy = let ls = dropWith (tail cx) (tail cy) in len ls == 0 ? 0 $ head ls

    

{-|
    === KEY: file permission, change file permission, chmod file

    * From unix package
     
    <https://hackage.haskell.org/package/unix-2.8.0.0/docs/System-Posix-Files.html#v:ownerExecuteMode ownerExecuteMode>
    <https://hackage.haskell.org/package/unix-2.8.0.0/docs/System-Posix-Files.html#g:2 setFileMode>
    <https://hackage.haskell.org/package/unix-2.8.0.0/docs/System-Posix-Files.html System.Posix.Files>

    'setFileMode'

    @
    chmodExec::FilePath -> IO()
    chmodExec fp = do
    setAccessPermissions fp (Permissions True True True False)
    @
-}

-- BEG_splitFileBlock
-- END_splitFileBlock
  
-- END123

-- CPP if else Macro
-- ghc -Dxxx => ghc -DTEST
--
-- change Cabal file to following
--      ghc-options:  -O2
--      cpp-options:  -DTEST  -- -Dxxx
--
-- See AronModuleLib.cabal file
--
-- following will cause many warning double slashs..

-- TEST is defined
-- IGNORE is not defined
-- See AronModuleLib.cabal file
-- #if defined(TEST)
#if defined(IGNORE)
{-|
    === split key and value

    CPP if else Macro
    ghc -Dxxx => ghc -DTEST

    change Cabal file to following
    ghc-options:  -O2
    cpp-options:  -DTEST  -- -Dxxx

    See AronModuleLib.cabal file

    following will cause many warning double slashs..
-}
fun10::Int -> Int
fun10 x = x + 1

#endif

