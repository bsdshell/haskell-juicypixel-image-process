{-# OPTIONS_GHC -Wmissing-fields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
-- empty the map (ref HMap) 
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- {-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE StrictData #-}

-- https://github.com/ndmitchell/record-dot-preprocessor#readme
-- dot operator for record
-- {-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
-- {-# LANGUAGE TypeApplications, FlexibleContexts, DataKinds, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

{-| 
    The Module contains all the functions for __haskellwebapp2__

    * Use Aeson to serialize record to Json
    * Record: Person
    * Insert data to MySqlit-simple file-based database
    * Upload file to server.
    * Use Redis(memcached) to store snippet and query snippet.
    * *src/aronlib.js* is symbollink to *$b/jslib/aronjs.js*
    * All Javascript functions are in *src/aronlib.js*
    * Use 'responseJavascript' to send *src/aronlib.js* to client side.
-} 
module WaiLib where

import Data.Default
import Data.Typeable (typeOf)
import Data.Typeable 
import Network.Wai
import Network.HTTP.Types
import Network.HTTP.Types.Header
import Network.Wai.Handler.Warp (run)
import Control.Monad
import Data.Char
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Time
import Data.IORef 
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
import Text.RE.TDFA.String
import Control.Lens ((^.))

-- Wednesday, 13 October 2021 14:39 PDT
-- Try to handle APL symbol code,
-- Other Regex crash:)
import qualified Text.Regex.Applicative as TRA


import Network.Wai.Parse
import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Data.ByteString.Builder (byteString, Builder)

import qualified Text.Email.Validate            as EM 
import qualified Data.Word8                     as DW
import qualified Data.Text                      as TS               -- strict Text         
import qualified Data.Text.Lazy                 as DL
import qualified Data.Text.Lazy.IO              as LIO
import qualified Data.Text.IO                   as TIO 

import qualified Control.Concurrent             as Concurrent
import qualified Data.List                      as L
import qualified Data.HashMap.Strict            as M 
import qualified Control.Exception              as Exception
import qualified Safe

import qualified Data.ByteString.UTF8          as BU
import qualified Data.ByteString.Lazy.Internal as IN (ByteString)
import qualified Data.ByteString.Char8      as S8 (unpack,pack, putStrLn)   -- strict ?
import qualified Data.ByteString.Lazy       as LA (writeFile, fromChunks, fromStrict)
import qualified Data.ByteString.Lazy.Char8 as LC 
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Internal   as BI (c2w, w2c)


import qualified Turtle as TUR -- (empty, shellStrictWithErr, ExitCode)
-- import Data.Text.Lazy -- lazy Text

import Network.HTTP.Types (status200)
import Network.Wai
-- import Network.Wai.Handler.Warp (run)
import qualified Network.Wai.Handler.Warp as WARP
import Network.Wai.Util
import Network.URI
import Network.HTTP.Types.Status
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets             as WS
import qualified Control.Exception              as EXP
       
import Language.Haskell.Ghcid

-- import qualified Network.Wai.Handler.WebSockets as WS

-- {-# LANGUAGE QuasiQuotes       #-}
import Text.RawString.QQ (r)         -- Need QuasiQuotes too 

-- http://hackage.haskell.org/package/neat-interpolation-0.3.2.4/docs/NeatInterpolation.html
import qualified NeatInterpolation as NI -- variable interpolation

-- remove it since there is issue to build in stack
-- copy the source code and create a module called PortableLines
-- import qualified Text.PortableLines as POR   -- (lines replace window newline '\r\n' with '\n')

import           Data.Int (Int64)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToRow
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok
import  qualified Database.SQLite.Simple.Types as DQ

import           GHC.Generics
import qualified Data.Aeson as DA
import Data.Aeson.Text (encodeToLazyText)
-- import Data.Aeson (ToJSON, decode, encode)

import qualified Data.ByteString.Lazy      as BL
import qualified Data.ByteString.UTF8      as BSU
import qualified Data.ByteString           as BS
import qualified Data.Text                 as TS    -- strict Text

import qualified Data.Text.Lazy            as TL    -- lazy Text
import qualified Data.Text.Encoding        as TSE
import qualified Data.Text.Lazy.Encoding   as TLE

import qualified Data.Bifunctor            as DB


-- BEG_993 concurrency
import System.Timeout
-- import Criterion.Measurement
import System.IO.Unsafe
import System.Process
import Control.Exception
import System.IO
import System.IO.Error
import GHC.IO.Exception
import System.Exit
import Control.Concurrent.MVar
import Control.Concurrent
-- END_993 concurrency
-- import Control.Concurrent (forkIO, threadDelay)



-- import PortableLines
-- import AronModule                hiding(run, cmd)
import AronModule hiding(run, cmd)
-- import HtmlForm                
import AronHtml                             as H1
import AronHtml2                            as H2 
import qualified AronModule                 as A
import qualified GenePDFHtmlLib             as PDF
import AronAlias
-- import qualified WaiConstant                as WC 



{-| 
    KEY: Say something
    
    M-x openurl
    help: file:///Users/aaa/myfile/bitbucket/stackproject/jupyterlab/jupyterlab.html
    gx /Library/WebServer/Documents/xfido/image/foldlistimage.jpg 
-} 

-- query_redis = "/Users/aaa/myfile/symbin/RedisQuery "
query_redis = "RedisQuery "
eleIdCodeBlock="t"
pdfdir = "pdf"

{-|
    KEY: Last snippet command is stored in Redis database, last redis cmd

-}
keyLastCmd::String
keyLastCmd = "keyLastCmd"

indexEditorHTML = "src/datadir/latex/indexEditorACE/indexEditorACE.html"
indexEditorJSON = "src/datadir/latex/indexEditorACE/indexEditorACE.json"

s2Text = strToStrictText

data Block = Block{bblock::[DL.Text]} deriving (Generic, Show)
data MBlock = MBlock{mblock::[Integer]} deriving (Generic, Show)
data GeneMatrix = GeneMatrix{
                             cmd :: TS.Text,
                             ncol :: Integer,
                             nrow :: Integer 
                            } deriving (Generic, Show)

instance DA.FromJSON GeneMatrix 
instance DA.ToJSON GeneMatrix where
    toEncoding = DA.genericToEncoding DA.defaultOptions

-- create instance of FromJSon an ToJSon
data Bgcolor = Bgcolor{ colorname :: TS.Text } deriving (Generic, Show)
instance DA.FromJSON Bgcolor
instance DA.ToJSON Bgcolor where
    toEncoding = DA.genericToEncoding DA.defaultOptions

data Textcolor = Textcolor{ textcolor :: TS.Text } deriving (Generic, Show)
instance DA.FromJSON Textcolor
instance DA.ToJSON Textcolor where
    toEncoding = DA.genericToEncoding DA.defaultOptions

data User = User   {uid::Int64, name::TS.Text, email::TS.Text, password::TS.Text, task::TS.Text, money::Integer} deriving (Show, Eq, Read)
data Image = Image {iid::Int64, imagename::TS.Text, uid::Int64} deriving (Show, Eq, Read)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

instance FromRow Image where
  fromRow = Image <$> field <*> field <*> field

instance ToRow User where
  toRow (User _uid name email password task money) = toRow (name, email, password, task, money)

instance ToRow Image where
  toRow (Image _iid imagename uid) = toRow (imagename, uid)

data TodoItem =
  TodoItem
    {
      todoId :: Int64,
      keyItem :: TS.Text,
      todoItem :: TS.Text
    } deriving (Generic, Eq, Read, Show)

    
instance DA.FromJSON TodoItem
instance DA.ToJSON TodoItem where
    toEncoding = DA.genericToEncoding DA.defaultOptions
    
  {--
instance ToRow TodoItem where
  toRow (TodoItem _todoId key_item todo_item) = toRow (todoId, keyItem, todoItem)
  --}
    
    {--
data TodoJSON = TodoJSON{
  keyItem :: String,
  todoX::String
  } deriving (GEN.Generic, Show)
instance DA.FromJSON TodoJSON
instance DA.ToJSON TodoJSON where
    toEncoding = DA.genericToEncoding DA.defaultOptions
    --}
    
data TodoReply = TodoReply{
                            cmdReply :: TS.Text
                          } deriving (Generic, Show)

instance DA.FromJSON TodoReply
instance DA.ToJSON TodoReply where
    toEncoding = DA.genericToEncoding DA.defaultOptions

{--
instance FromRow TodoItem where
   fromRow = TodoItem <$> field <*> field
--}
  
data CompileCode = CompileCode{
                                compiler :: TS.Text,
                                option :: TS.Text,
                                code :: TS.Text 
                              } deriving (Generic, Show)

instance DA.FromJSON CompileCode 
instance DA.ToJSON CompileCode where
    toEncoding = DA.genericToEncoding DA.defaultOptions


-- Send to client => JSON [[Integer]]
data MatInt = MatInt{name::TS.Text, matrix::[[Integer]]} deriving (Generic, Show)
instance DA.FromJSON MatInt 
instance DA.ToJSON MatInt where
    toEncoding = DA.genericToEncoding DA.defaultOptions

data SnippetJSON = SnippetJSON{pidls::[Integer], name::TS.Text, snippet::[[String]]} deriving (Generic, Show)
instance DA.FromJSON SnippetJSON
instance DA.ToJSON SnippetJSON where
    toEncoding = DA.genericToEncoding DA.defaultOptions

    
-- Generate HTML table in Server side
-- Send to client in JSON format [[TS.Text]]
-- Client can display it on Browser
--
-- Send to client => JSON [[TS.Text]]
data HTMLTable = HTMLTable{name::TS.Text, matrix::[TS.Text]} deriving (Generic, Show)
instance DA.FromJSON HTMLTable
instance DA.ToJSON HTMLTable where
    toEncoding = DA.genericToEncoding DA.defaultOptions

data PreColor = PreColor {color::TS.Text, background::TS.Text} deriving (Generic, Show)
instance DA.FromJSON PreColor
instance DA.ToJSON PreColor where
    toEncoding = DA.genericToEncoding DA.defaultOptions

 
updateRetcmd::String -> CodeBlockReply -> CodeBlockReply
updateRetcmd s u = u { retcmd = s}

updateOk::String -> CodeBlockReply -> CodeBlockReply
updateOk s u = u { ok = s }
               
instance DA.FromJSON Block 
instance DA.ToJSON Block where
    -- No need to provide a toJSON implementation.

    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = DA.genericToEncoding DA.defaultOptions

instance DA.FromJSON MBlock 
instance DA.ToJSON MBlock where
    -- No need to provide a toJSON implementation.

    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = DA.genericToEncoding DA.defaultOptions

-- | Person to Json object
data Person =
  Person 
    { personId   :: Int64
    , personName :: TS.Text
    , personAge  :: TS.Text
    } deriving (Eq,Read,Show)

-- | define record for all the code blocks
--  can not define [TS.Text] => sqlite3 does not support [TS.Text]
-- data CodeBlock = 
--    CodeBlock 
--    { codeblockId        :: Int64
--    , header    :: TS.Text
--    , codeblock :: TS.Text
--    } deriving (Eq, Read, Show)




-- instance FromRow CodeBlock where
--  fromRow = CodeBlock <$> field <*> field <*> field

-- What is 'Only'
-- https://hackage.haskell.org/package/postgresql-simple-0.4.9.0/docs/Database-PostgreSQL-Simple.html#t:ToRow
-- instance ToRow CodeBlock where
--   toRow (CodeBlock _pId pHeader pCode) = toRow (pHeader, pCode)


{-| 
    === Create UserInput table in Sqlite
    * login database
    * sqite3 /Users/aaa/myfile/bitbucket/testfile/userinput.db
    * cmdId = pid
    * xcmd = input command, e.g. "c ls"
-} 
data UserInput =
  UserInput 
    { cmdId :: Int64
    , xcmd :: TS.Text
    } deriving (Eq,Read,Show)

instance FromRow Person where
  fromRow = Person <$> field <*> field <*> field

instance FromRow UserInput where
  fromRow = UserInput <$> field <*> field

-- when inserting a new Person, ignore personId. SQLite will provide it for us.
instance ToRow Person where
  toRow (Person _pId pName pAge) = toRow (pAge, pName)

-- http://hackage.haskell.org/package/sqlite-simple-0.4.16.0/docs/Database-SQLite-Simple.html#v:toRow
instance ToRow UserInput where
  toRow (UserInput _cmdId md) = toRow (Only md)

updir = "/Users/aaa/myfile/bitbucket/haskellwebapp2/uploaddir/"

hiddenLATEXCODE = "latexcode_replace314"
hiddenCOMPILESAVE = "hidden_compile_save"



dbname = "webappdb"
configFile = "./config.txt"

lookupJust s m = fromJust $ M.lookup s m

confMap::FilePath -> IO (M.HashMap String String)
confMap fp = do
  os <- getOS
  configMap <- readConfig fp
  return $ lookupJust os configMap
 where
   lookupJust s m = fromJust $ M.lookup s m

getHostName::IO String
getHostName = do
  osMap <- confMap configFile
  let host = lookupJust "host" osMap
  let portStr = lookupJust "port" osMap
  return $ host ++ ":" ++ portStr

{-|
    === KEY: get the full rootdir

    @
      "/Users/aaa/myfile/bitbucket/haskellwebapp2"
      "/Users/aaa/myfile/mybin/haskellwebapp2Bin"
    @
-}
getRootDirFull::IO String
getRootDirFull = do
  osMap <- confMap configFile
  home <- getEnv "HOME"
  let rootdir = lookupJust "rootdir" osMap
  return $ home </> rootdir
      
styleChar::String->String->Char->Char->String->String
styleChar l r a b s = foldr(\x' y' -> x' ++ y') [] $ map(\x -> if x == a || x == b then l ++ (x:[]) ++ r else (x:[])) s

-- | -------------------------------------------------------------------------------- 
-- | Thu Nov 15 13:18:04 2018 
-- | Simple web server for request and response
-- | Handle search code snippet
-- | -------------------------------------------------------------------------------- 
-- run.sh => ./wai 
-- ghc -i/$b/haskelllib --make wai.hs -o wai
                      
-- [file path] [desc] [image src]
href::String->String->String->String
href p n m = [r|<a href='|] <> p <> [r|'>|] <> (baseName p) <> 
             [r|<img src='|] <> m <> [r|' width="10%"  height="10%" /></a>|] 

changeSymbol::String -> String
changeSymbol str = s1 
    where
        s = splitStr "<-" str  -- ["dog", "cat"]
        ss = if takeEnd 1 s  == [""] then init s else init s  -- ["dog", "cat"]
        s1 = (concat (map(\x -> x ++ "<span style='color:red;'>&lt;-</span>") ss)) ++ la
        la = if takeEnd 1 s == [""] then "" else last s

-- | Convert [[String]] to ByteString
listToByteStr::[[String]]->BS.ByteString
listToByteStr s = toSBS $ L.foldr(\x y-> x ++ "<br>" ++ y) [] $ L.foldr(\x y -> x ++ ["<br>"] ++ y) [] s

cssStrong::String->String
cssStrong s = if len > 2 then "<strong>" ++ s ++ "</strong>" else s
            where 
                list = splitRegex(mkRegex ":") s
                len = length list


cssHead::[[String]]->[[String]]
cssHead = map(\x -> let 
                        len = length $ splitRegex(mkRegex ":") (head x) 
                        in if len > 2 then ("<span style=\"color:gray;\">" ++ head x ++ "</span>"): drop 1 x else x)

htmlLess::[[String]]->[[String]]
htmlLess = (map . map)(\x -> (subRegex r x) "&lt;")
        where
            r = mkRegex "<"

htmlGreater::[[String]]->[[String]]
htmlGreater = (map . map)(\x -> (subRegex r x) "&gt;")
        where
            r = mkRegex ">"

keyWord::[[String]]->[[String]]
keyWord = (map . map)(\x -> (subRegex r x) "<span style=\"color:green;\">\\0</span>")
        where
            r = mkRegex "CTRL[a-zA-Z_-]*"

redisGetLastCmd::String -> IO String
redisGetLastCmd k = redisGet k >>= \c -> case c of
                                           Just x  -> return x 
                                           Nothing -> return ""

{-|
    === KEY: apl symbol

    DATE: Monday, 24 July 2023 16:42 PDT

    APL symbol
    SEE: /Users/aaa/myfile/bitbucket/publicfile/aplSymbol.txt
    -- FIXED: Remove '/' from apl code in order to fix URL format
    -- Unicode code point: 9017  0x2339 ⌹
    -- https://unicodeplus.com/U+2339
    -- putStrLn "\9017"
-}
aplSymbol::[[String]]->[[String]]
aplSymbol = (map . map) (\x -> (subRegex rx x) rp)
  where
    -- rp = "<span style=\"color:yellow;font-size:18pt;\">\\0</span>"
    rs = [r|
           ÷ × , / ? \ ¨ ì {} ← ↑ → ↓ ∆ ∇ ∈ − ∘ ∣ ∧ ∨ ∩ ∪ ∵ ∼ ≠ ≡ ≢ ≤ ≥ ⊂ ⊃ ⊖ ⊢ ⊣ ⊤ ⊥ ⊼ ⊽ ⋄ ⋆ ⌈ ⌊ ⌶ ⌷ ⌸ ⌹ ⌺ ⌻ ⌼ ⌽ ⌾ ⌿ ⍀ ⍁ ⍂ ⍃ ⍄ ⍅ ⍆ ⍇ ⍈ ⍉ ⍊ ⍋ ⍌ ⍍ ⍎ ⍏ ⍐ ⍑ ⍒ ⍓ ⍔ ⍕ ⍖ ⍗ ⍘ ⍙ ⍚ ⍛ ⍜ ⍝ ⍞ ⍟ ⍠ ⍡ ⍢ ⍣ ⍤ ⍥ ⍦ ⍧ ⍨ ⍩ ⍪ ⍫ ⍬ ⍭ ⍮ ⍯ ⍰ ⍱ ⍲ ⍳ ⍴ ⍵ ⍶ ⍷ ⍸ ⍹ ⍺ ⎕ ○
           |]
    -- Remove '/' from apl code in order to fix URL format
    pl = let s = filter (\x -> x /= ' ' && x /= '/') rs in "[" + s + "]"
    rx = mkRegex pl
    (+) = (++)
    rp = "<span style=\"color:yellow;\">\\0</span>"
    -- rx = mkRegex "[\9017\9018\9071\9037\9025\9056\9027\9016\9028\9072\9020\9044\9026\9043\9019\9036\247\9050\8900\9048\9066\8594\8867\8722\8802\8800\9049\9035\8710\8802\9068\9070\8592\8804\8801\8805\9065\9053\9014\9014\9055\8854\9061\9033\9052\9675\8868\168\9067\8739\9078\9078\9082\8892\8743\8592\9024\8869\9109\8594\8743\9066\9066\8968\9033\8854\9061\9022\9055\9021\9052\9052\9675\9066\9053\9023\8869\9058\9042\9067\9035\9049\9049\8710\8711\8801\8867\168\9050\9050\8900\8757\8835\247\9017\8595\8744\8746\8868\9046\9073\9062\9057\9045\9041\9046\8595\168\8834\8868\8712\9079\9079\8712\236\9024\8902\9079\8593\8970\9045\9058\8592\8594\9042\9035\8968\9065\8805\9061\9060\9075\9015\9075\8745\8745\8744\9080\9080\9075\8592\9060\9051\9051\8728\9066\9053\8592\8834\8867\9029\9063\9029\8804\8970\8866\9055\9055\9017\9017\8801\8968\8712\8712\8722\8970\8711\9074\8728\8722\9073\8800\8802\8764\9060\9051\9081\9081\9077\8893\8744\8728\8834\8757\9675\8835\8902\9032\9026\9020\9056\9037\9044\9018\9017\9047\9036\9016\9028\9019\9031\9027\9044\9071\9072\9032\9025\9040\9043\9031\9027\9028\9109\9054\9048\9048\9060\247\9023\9023\8868\9076\8739\9021\9076\8594\8835\8866\9030}\9030\8728?\8854\9021\9024\9070\9076\215\9023\9023\9024\9064\9057\9059\9015\9015\9059\8902\9069\8739\8593\9064\8764\215\9033\8712\8746\8593\8743\8745\8869\9039\9074\9053\9038\9034\9039\9077\215\9068\8866\9069\9064\9074\9073\8764]"

  
-- latex: \begin{document} \end{document}
keyWord1::[[String]]->[[String]]
keyWord1 = (map . map)(\x -> (subRegex r x) "<span style=\"color:green;\">\\0</span>")
        where
            r = mkRegex "\\\\[a-zA-Z0-9]+{[^}]+}"

keyDash::[[String]]->[[String]]
keyDash = (map . map)(\x -> (subRegex r x) "<span style=\"color:red;\">\\0</span>")
        where
            r = mkRegex "[-+]{10,}"

--keySymbol1::[[String]]->[[String]]
--keySymbol1 s = (map . map)(\x -> (subRegex r x) "<span style=\"color:blue;\">\\0</span>")  s
--        where
--            r = mkRegex "=>|=="

keySymbol1::[[String]]->[[String]]
keySymbol1 = (map . map)(\x -> changeSymbol x)

--keyName::[[String]]->[[String]]
--keyName s = (map . map)(\x -> (subRegex r x) "<span style=\"color:pink; background:#CCF7F7;\">\\0</span>")  s
--        where
--            r = mkRegex "buffer|while|if|endif|Emacs|split|goto"

keyName::[[String]]->[[String]]
keyName s = (map . map)(\x -> x *=~/ 
    [ed|${adr}(\<where\>|\<let\>):?///<span style="color:blue;">${adr}</span>|]) s

specialName::[[String]]->[[String]]
specialName = (map . map)(\x -> x *=~/ 
    [ed|${adr}(\<new\>|::|\<sizeof\>):?///<span style="color:red;">${adr}</span>|])

javaClassName::[[String]]->[[String]]
javaClassName = (map . map)(\x -> x *=~/ [ed|${adr}(\<interface\>|\<abstract\>|\<implements\>|\<class\>|\< = \>):?///<span style="color:#ef82ee;">${adr}</span>|])

        -- let s1 = "mydog dog dog (dog)" ?=~/ [ed|${adr}(\<dog\>):?///< div class="dog">${adr}< /div> |]
-------------------------------------------------------------------------------- 
-- Use following package lang extension and package for word boundary 
-- search and replacement
-- {-# LANGUAGE QuasiQuotes       #-}
-- import Text.RE.TDFA.String

-- add more ClassName here
javaFunClass::[[String]]->[[String]]
javaFunClass = (map . map)(\x -> x *=~/ 
    [ed|${adr}(\< Vector \>|\< List \>|\< Set \>|\< HashSet \>|\< HashMap \>|\< ArrayList \>|\< Integer \>|\< String \>):?///<span style="color:#218e2b;">${adr}</span>|])
    -- it is too slow [ed|${adr}(\<[A-Z][a-z_0-9]*\>):?///<span style="color:#218e2b;">${adr}</span>|]) s
-------------------------------------------------------------------------------- 
javaKeyWords::[[String]]->[[String]]
javaKeyWords = (map . map)(\x -> x *=~/ 
    [ed|${adr}(\< abstract \>|\< assert \>|\< boolean \>|\< break \>|\< byte \>|\< case \>|\< catch \>|\< char \>|\< class \>|\< const \>|\< continue \>|\< default \>|\< do \>|\< double \>|\< else \>|\< enum \>|\< extends \>|\< final \>|\< finally \>|\< float \>|\< for \>|\< goto \>|\< if \>|\< implements \>|\< import \>|\< instanceof \>|\< int \>|\< interface \>|\< long \>|\< native \>|\< new \>|\< package \>|\< private \>|\< protected \>|\< public \>|\< return \>|\< short \>|\< static \>|\< strictfp \>|\< super \>|\< switch \>|\< synchronized \>|\< this \>|\< throw \>|\< throws \>|\< transient \>|\< try \>|\< void \>|\< volatile \>|\< while \>):?///<span style="color:#f50a93;">${adr}</span>|])

-------------------------------------------------------------------------------- 
javaCmdKeyWords::[[String]]->[[String]]
javaCmdKeyWords = (map . map)(\x -> x *=~/ 
    [ed|${adr}(\< java \>|\< javac \>|\< javadoc \>|\< jar \>):?///<span style="color:#35A993;">${adr}</span>|])

-------------------------------------------------------------------------------- 

mysqlKeyWords::[[String]]->[[String]]
mysqlKeyWords = (map . map)(\x -> x *=~/ 
    [ed|${adr}(\< insert \>|\< create \>|\< from \>|\< select \>|\< table \>|\< into \>):?///<span style="color:#FF69B4;">${adr}</span>|])
-------------------------------------------------------------------------------- 

-- [[:graph:]] - ASCII char excluding space
-- match URL
keyURL::[[String]]->[[String]]
keyURL = (map . map)(\x -> (subRegex r x) "<a href=\"\\1\">\\1</a>")
        where
            r = mkRegex "(https?://[[:graph:]]+)"

spChar::[[String]]->[[String]]
spChar = (map . map)(\x -> styleChar l r a b x)
        where
            l = "<span style=\"color:red;\">"
            r = "</span>"
            a = '{' 
            b = '}' 

bracketChar::[[String]]->[[String]]
bracketChar = (map . map)(\x -> styleChar l r a b x)
        where
            l = "<span style=\"color:blue;\">"
            r = "</span>"
            a = '(' 
            b = ')' 

sbChar::[[String]]->[[String]]
sbChar = (map . map)(\x -> styleChar l r a b x)
        where 
            l = "<span style=\"color:#e012cd;\">"
            r = "</span>"
            a = '[' 
            b = ']' 

-- compose all Regex subRegex
-- transform = id
transformX = 
             cssHead.
             spChar.
             bracketChar.
             sbChar.
             specialName.
             javaClassName.
             javaFunClass.
             javaKeyWords.
             javaCmdKeyWords.
             mysqlKeyWords.

             keyURL.
             keyDash.
             keyName.
             aplSymbol.
             (htmlLess.htmlGreater)



wsApp :: WS.ServerApp
wsApp pending = do
  forever $ do
    conn <- WS.acceptRequest pending
    msg <- WS.receiveData conn :: IO TS.Text
    putStrLn $ show msg
    putStrLn ("weApp"::String)
    WS.sendTextData conn msg
    
type Client = (Int, WS.Connection)

broadcast :: TS.Text -> [Client] -> IO ()
broadcast msg = mapM_ (flip WS.sendTextData msg) . map snd

addClient :: WS.Connection -> [Client] -> ([Client], Int)
addClient conn cs = let i = if null cs then 0 else maximum (map fst cs) + 1
                    in  ((i, conn):cs, i)

removeClient :: Int -> [Client] -> ([Client], ())
removeClient i cs = (filter (\c -> fst c /= i) cs, ())

chat :: IORef [Client] -> WS.ServerApp
chat ref pending = do
    conn <- WS.acceptRequest pending
    identifier <- atomicModifyIORef ref (addClient conn)
    flip EXP.finally (disconnect identifier) $ forever $ do
        msg <- WS.receiveData conn
        -- putStrLn msg
        putStrLn $ show msg
        conns <- readIORef ref
        broadcast msg conns
    where
    disconnect identifier = atomicModifyIORef ref (removeClient identifier)
       
{-|
     === Fake optional parameter

     > alternateLineColor2 []
     > alternateLineColor2 [("background", "green"), ("background", "cyan")]
-}                 
alternateLineColor2::[A.CSSPro] -> [[String]] -> [[String]]
alternateLineColor2 cs cx = case len cs of
                    0 -> let style  = H2.style_ [("color", "#AAAAAA")] 
                             style' = H2.style_ [("color", "white")]
                         -- in map(\row -> map(\(x, n) -> if (mod n 2) == 0 then H2.span_ style x else H2.span_ style' x ) $ zip row [0..]) cx
                         in map(\row -> zipWith (\x n -> if even 2 then H2.span_ style x else H2.span_ style' x) row [0..]) cx
                    2 -> let style1 = H2.style_ $ init cs
                             style2 = H2.style_ $ tail cs
                         -- in map(\row -> map(\(x, n) -> if (mod n 2) == 0 then H2.span_ style1 x else H2.span_ style2 x ) $ zip row [0..]) cx
                         in map(\row -> zipWith(\x n -> if even 2 then H2.span_ style1 x else H2.span_ style2 x) row [0..]) cx
                    _ -> error [r|Invalid input: => alternateLineColor[("background", "green"), ("background", "cyan")]|]
                 where
                   (+) = (++)

blockId::Integer -> String
blockId n = "t" ++ show n

svgIconUpdate::String
svgIconUpdate = [r| 
      <svg xmlns='http://www.w3.org/2000/svg' class='ionicon' width="24" height="24" viewBox='0 0 512 512'>
      <title>Arrow Up</title>
      <path fill='none' stroke='currentColor' stroke-linecap='round' stroke-linejoin='round' stroke-width='48' d='M112 244l144-144 144 144M256 120v292'/>
      </svg>|]

svgIconInsert::String
svgIconInsert = [r|
      <svg xmlns='http://www.w3.org/2000/svg' class='ionicon' width="24" height="24" viewBox='0 0 512 512'>
	<title>Add Circle</title>
	<path d='M448 256c0-106-86-192-192-192S64 150 64 256s86 192 192 192 192-86 192-192z' fill='none' stroke='currentColor' stroke-miterlimit='10' stroke-width='32'/>
	<path fill='none' stroke='currentColor' stroke-linecap='round' stroke-linejoin='round' stroke-width='32' d='M256 176v160M336 256H176'/>
      </svg> |]

svgIconDelete::String
svgIconDelete = [r| 
   <svg version="1.1" xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 512 512">
   <title></title>
   <g id="icomoon-ignore"></g>
   <path d="M507.331 411.33c-0.002-0.002-0.004-0.004-0.006-0.005l-155.322-155.325 155.322-155.325c0.002-0.002 0.004-0.003 0.006-0.005 1.672-1.673 2.881-3.627 3.656-5.708 2.123-5.688 0.912-12.341-3.662-16.915l-73.373-73.373c-4.574-4.573-11.225-5.783-16.914-3.66-2.080 0.775-4.035 1.984-5.709 3.655 0 0.002-0.002 0.003-0.004 0.005l-155.324 155.326-155.324-155.325c-0.002-0.002-0.003-0.003-0.005-0.005-1.673-1.671-3.627-2.88-5.707-3.655-5.69-2.124-12.341-0.913-16.915 3.66l-73.374 73.374c-4.574 4.574-5.784 11.226-3.661 16.914 0.776 2.080 1.985 4.036 3.656 5.708 0.002 0.001 0.003 0.003 0.005 0.005l155.325 155.324-155.325 155.326c-0.001 0.002-0.003 0.003-0.004 0.005-1.671 1.673-2.88 3.627-3.657 5.707-2.124 5.688-0.913 12.341 3.661 16.915l73.374 73.373c4.575 4.574 11.226 5.784 16.915 3.661 2.080-0.776 4.035-1.985 5.708-3.656 0.001-0.002 0.003-0.003 0.005-0.005l155.324-155.325 155.324 155.325c0.002 0.001 0.004 0.003 0.006 0.004 1.674 1.672 3.627 2.881 5.707 3.657 5.689 2.123 12.342 0.913 16.914-3.661l73.373-73.374c4.574-4.574 5.785-11.227 3.662-16.915-0.776-2.080-1.985-4.034-3.657-5.707z"></path>
   </svg> |]

svgIconSubtract::String
svgIconSubtract = [r|
    <svg version="1.1" xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 512 512">
    <title></title>
    <g id="icomoon-ignore">
    </g>
    <path d="M0 208v96c0 8.836 7.164 16 16 16h480c8.836 0 16-7.164 16-16v-96c0-8.836-7.164-16-16-16h-480c-8.836 0-16 7.164-16 16z"></path>
    </svg> |]

svgIconAdd::String
svgIconAdd = [r|
    <svg version="1.1" xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 512 512">
    <title></title>
    <g id="icomoon-ignore"></g>
    <path d="M496 192h-176v-176c0-8.836-7.164-16-16-16h-96c-8.836 0-16 7.164-16 16v176h-176c-8.836 0-16 7.164-16 16v96c0 8.836 7.164 16 16 16h176v176c0 8.836 7.164 16 16 16h96c8.836 0 16-7.164 16-16v-176h176c8.836 0 16-7.164 16-16v-96c0-8.836-7.164-16-16-16z"></path>
    </svg> |]

{-| 
    === Hide all the data in TextArea

    * IN USE

    @
    <form action="serverFun.hs" name="someName" method="POST">
    <textarea id="ttId" class="text" cols="86" rows ="20" name="textName"></textarea>

    <input type="submit" value="Email" class="submitButton">
    </form>

    <textarea cols="20" rows="20" id="textArea" style="display:none;font-size:18px;" class="hide"></textarea>

    <textarea autofocus="true" onfocus="textAreaAdjust(this);"></textarea>
    @
    
    We update the codeblock according to "header"(use ID?)
    

    TODO1: use Ajax to update codeblock in database
    See aronlib.js requestPreFromRedis

    @
    data CodeBlock = 
    CodeBlock 
    { id        :: Int64
    , header    :: TS.Text
    , codeblock :: TS.Text
    } deriving (Eq, Read, Show)
    @
    pid => id => from CodeBlock table from sqlite3

    :DATE: 26-10-2020
    :NOTE: USE IT NOW ✅ 
    :FIXME: Click hide/show DOES NOT WORK, the click location is off
    :IMG: file:///Users/aaa/myfile/bitbucket/image/clickhide.png
-} 
hiddenForm2::Integer -> String -> String  
hiddenForm2 pid s = [r|
        <form action="/update" name="Update" class="hf" id='|] <> cid "f" pid <>[r|'|] <>
    [r| method="POST"><textarea name="header" id='|] <> pidStr <> [r|' rows="20" class="hide">|] <> pidStr <> [r|</textarea>|]<> 
    [r|<textarea  name="myblock" spellcheck="false" style="car et-color:red;" autofocus="true" onfocus="textAreaAdjust(this);" id= '|] <> eleIdCodeBlock <> pidStr <> [r|' |] <> 
    [r|class="hide">|]<>s<>[r|</textarea><button type="button" |] <> onClickUpdate <> [r| value="update" id="search-button">|]
    <>svgIconUpdate <>
    [r|</button><button type="button" |] <> onClickInsert <> [r| value="add" id="search-button">|]
    <> svgIconInsert <>
    [r|</button><button type="button" |] <> onClickDelete <> [r| value="delete" id="search-button">|]
    <> svgIconDelete <>
    [r|</button><button type="button" |] <> onClickAdd <> [r| value="addscore" id="search-button">|]
    <> svgIconAdd <>
    [r|</button><button type="button" |] <> onClickSubtract <> [r| value="subtractscore" id="search-button"> |]
    <> svgIconSubtract <>
    [r|</button></div></form>
    |]
      where
        pidStr = show pid 
        cid s n = s ++ show n
        onClickUpdate   = [r| onclick="updateCodeBlock('|] <> pidStr <> [r|');" |]
        onClickInsert   = [r| onclick="insertCodeBlock('|] <> pidStr <> [r|');" |]
        onClickDelete   = [r| onclick="deleteCodeBlock('|] <> pidStr <> [r|');" |]
        onClickAdd      = [r| onclick="addScoreCodeBlock('|] <> pidStr <> [r|');" |]
        onClickSubtract = [r| onclick="subtractScoreCodeBlock('|] <> pidStr <> [r|');" |]
{-|
    === KEY:  

    @
     In Java
     Function f = x -> x + 1
     BiFunction f = (x, y) -> x + y
     
     
     foldr(\x y -> [div] ++ x ++ [cdiv] ++ brr + y) (0, []) zhtml
     
     The id can be used to for TextArea editor
     e.g.
      <TextArea onclick="editfun()" ></TextArea>
     
     <script>
     function editfun(){
     
     }
     
     </script>
     
     See file gf: /Users/aaa/myfile/bitbucket/html/showTextAreaOnClick.html
     
     <div id=\"3\" style=\"kk\"> code1 </div> 
     <div id=\"4\" style=\"kk\"> code2 </div> 
     
     ([[String]] -> [[String]]) 
     stylish allBlock

     Mon Dec  2 12:55:08 2019 
     Fixex issue inside 'stylish allBlock', apply <br> to stylish allBlock instead of zhtml

     TODO1
     foldListList ::([([String], Integer)]->[([String], Integer)])->[([String], Integer)]->String

    DATE: Mon 28 Nov 18:50:38 2022 
    NOTE: USE IT NOW
    @
-}
foldListList::([[String]]->[[String]])->[[String]]->String
foldListList stylish allBlock = L.foldr(+)[] $ map (concatStr' []) zhtml 
               where
                -- flip does not?
                concatStr' x y  = concatStr y x
                -- Add PID in allBlock?
                code = zip ((map . map)(+ br) $ stylish allBlock) allBlock -- code => stylish code
                -- n    => [1..] => Primary Key in CodeBlock
                -- code => [(x, b)] => [([String], [String])]
                -- zhtml = [[String]]
                zhtml = zipWith(\n (x, b) ->[hiddenForm2 n (unlines b)] +
                                 [preT $ (H1.ondblclick_ $ fun "showandhide" (ts n)) + (H1.class_ $ "co" +| n) + (H1.id_ $ "c" +| n)] +
                                 [div_ ac] + x + [cdiv] + [cpre] + [toClipboard n]) [1..] code
                
                br          =  "<br>"
                cdiv        =  "</div>"
                cpre        =  "</pre>"
                preT s      =  "<pre " <> s <> " >"
                ao          =  "<"
                ac          =  ">"
                divo        =  "<div "
                div_ s      =  "<div " + s
                ts          =  intToString
                (+)         =  (++)
                (+|) s n    =  s + (ts n)
                fun s arg   =  s + "(" + arg + ")"
                -- toClipboard n    = [r|<div class="butcen"><input type="button" class="butcopy" onClick="clip(document.getElementById('|] <> "c" <> (show n) <> [r|'));" name="cp" value="copy" ></div>|]
                toClipboard n    = [r|<div class="butcen"><input type="button" class="butcopy" onClick="copyToClipboardFromTextArea('|] <> eleIdCodeBlock <> (show n) <> [r|');" name="cp" value="copy" ></div>|]
                inputNum n  = [NI.text|<div class="butcen"><input type="button" onClick="clip(document.getElementById('c${n}'));" name="cp" value="copy" ></div>|] 


                    

{-|
   @
   [([String], Integer, Integer,  Integer)]
   [(codeBody, pid,      addedtime, score)]

   [
     ( [ "line2" ]         -- [String]
     , 4                   -- pid
     , 8                   -- addedtime
     , 3                   -- score
     )
   ]

   [
        ( "23423"
        ,
            [
                (
                    [ "23423:*:what0000"   -- [String]
                    , "line 4"             
                    ]
                , 117                      -- table pid
                , 1635398867               -- addedtime
                , 33                       -- score
                )
            ,
                (
                    [ "23423:*:test"
                    , "line1000"
                    ]
                , 113
                , 1635380323
                , 0
                )
            ]
        )
    ,
        ( "updat"
        ,
            [
                (
                    [ "test:*: test10, ok, update444"
                    , " line 'dog'cat"
                    , "fac ← {⍵ > 1 : ⍵×fac ⍵ - 1 ⋄ 1}"
                    , "nice"
                    , "update"
                    , "line444"
                    , "line555"
                    ]
                , 35
                , 1635399200
                , 8
                )
            ]
        )
    ]


   @


-}
foldListList2::([[String]]->[[String]])->[([String], Integer, Integer, Integer)]->String
foldListList2 stylish allBlock = L.foldr(+)[]  zhtml3
               where
                -- flip does not?
                concatStr' x y  = concatStr y x
                -- Add PID in allBlock?
                -- show the newest added code first
  
                -- TODO: Add diffe rent sortings flag from user input
                sortAllBlock = qqsort(\(_ , _ , time , score) (_ , _ , time' , score') -> score > score') allBlock
                
                -- allBlock' = map ft1 allBlock
                sortAllBlock' = map ft1 sortAllBlock
                -- code = zip ((map . map)(\x -> x + br) $ stylish allBlock') allBlock -- code => stylish code
                code1 = zipWith (\s1 s2 -> (s1, ft1 s2, ft2 s2)) ((map . map) (+ br) $ stylish sortAllBlock') sortAllBlock
                -- Wed 27 May 01:28:32 2020 
                -- TODO1: Add id here to delete it

                -- zhtml3 =>
                -- <pre  ondblclick='showandhide(128)'   class='co0'   id='c128'  >
                -- <div>
                -- <span style="color:gray;">firstone:*: tt4, update, ok, newme, update</span>
                -- <br> test1 test2
                -- <br> test3 test4<br>
                -- </div>
                -- </pre>
                --
                zhtml3 = map(\(x, b, n) -> div_ "" (hiddenForm2 n (unlines b) +
                                           (
                                             H2.pre_ (concatStr [H2.ondblclick_ [fun "showandhide" (ts n)],
                                                                 H2.class_ ["co" ++> 0],
                                                                 H2.id_ ["c" ++> n]   -- HTML element CAN NOT have multiple id =>  <pre id='s1' id='s2'>
                                                                ] " "
                                                     )
                                                    (H2.div_ [] (concatStr x [])) + 
                                                    toClipboard n
                                           ))
                            ) code1
                ft1 (a, b, c, d) = a
                ft2 (a, b, c, d) = b
                br          =  "<br>"
                ts          =  intToString
                (+)         =  (++)
                (++>) s n     =  s + (show n)
                (+|) s n    =  s + ts n
                fun s arg   =  s + "(" + arg + ")"
                -- toClipboard n    = [r|<div class="butcen"><input type="button" class="butcopy" onClick="clip(document.getElementById('|] <> "c" <> (show n) <> [r|'));" name="cp" value="copy" ></div>|]
                toClipboard n    = [r|<div class="butcen"><input type="button" class="butcopy" onClick="copyToClipboardFromTextArea('|] <> eleIdCodeBlock <> show n <> [r|');" name="cp" value="copy" ></div>|]
                inputNum n  = [NI.text|<div class="butcen"><input type="button" onClick="clip(document.getElementById('c${n}'));" name="cp" value="copy" ></div>|] 
                    



-- myfun  var  = [text|dog_cat_${var}_pig|] 

myfun name = [NI.text|this_could_be_'${name}'_long_identifier|]
fun4 name = toStr [NI.text|${name}|]

foldListListTxt::[[String]]->String
foldListListTxt allBlock = L.foldr(\x y -> x ++ "\n" ++ y) []  
                           $ L.foldr(\x y -> x ++ ["\n"] ++ y) [] allBlock    -- f s => [[String]]
                           
foldListListTxt2::[([String], Integer, Integer, Integer)] -> String
foldListListTxt2 allBlock = L.foldr(\x y -> x ++ "\n" ++ y) [] $
                              L.foldr(\x y -> x ++ ["\n"] ++ y) [] allBlock'
                         where
                            allBlock' = map x1 allBlock
                            x1 (a, b, c, d) = a




-- /Library/WebServer/Documents/zsurface/pdf
pdfname   = "Very Important File"
img     = "img.png"
pdfPath = "/Library/WebServer/Documents/zsurface/pdf"
docRoot = "/Library/WebServer/Documents/zsurface"
doc     = ""
cmdLog  = "/Users/aaa/myfile/bitbucket/testfile/waiCmdLog.txt"

currCmdFile = "/Users/aaa/myfile/bitbucket/testfile/currCmd.txt"

logCurrCmd::[String] -> IO()
logCurrCmd = writeToFile currCmdFile

readCurrCmd::IO String                               
readCurrCmd = readFileLatin1 currCmdFile

              
{-|
                              [([String], Integer, Integer,  Integer)]

                              [(codeBody,   pid,   addedtime,  score)]
                                   ↓         ↓        ↓          ↓ 
-}
type HMap2 = M.HashMap String [([String], Integer, Integer,   Integer)]
          
type PDFMap = M.HashMap String String

-- Response html, css, js, pdf from Server
type RespMap = M.HashMap String String

genePDF::String->IO() 
genePDF p = do 
    f <- A.lsFile p 
    -- mapM print f
    A.fl
    let list = map(\x -> href (doc </> x) pdfname img ++ "<br>")  f
    -- mapM print list
    A.writeToFile "./pdf.html" list 


{-| 
    === Main Application entry

    @
    type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
    @

    * Add *src/aronlib.js* as *Javascript* library which includes all javascript functions
    * Copy to clipboard still not working so far.

    <http://localhost/html/indexWhatIdidtoday.html#orgc0b84d7 Here_is_Why>

    :NOTE: USE IT
-} 
-- app2::Ghci -> Connection -> IORef HMap2->Application
app2::Connection -> IORef HMap2 -> IORef PDFMap -> RespMap -> Application
app2 conn1 ref pdfMapRef rmap request respond = do
  let x = 100
  let s = "a"
  logFileG ["pathInfo_request=>" ++ show (pathInfo request)]
  case pathInfo request of 
   ("test":_)       -> respond $ responseNothing "test nothing"
   ("raw":_)        -> respond plainIndex
   ("up":_)         -> respond uploadPage
   ("insertinfo":_) -> respond insertinfo
   ("listPage":_)   -> listPage conn1 request respond
   ("insertUser":_) -> respond insertUser
   ("login":_)      -> respond loginHtml
   ("genepdf":_)    -> respond $ responseGenePDFHtml conn1
   ("pdfimage":fn:_)-> do
                         let fname = let p = rawPathInfo request 
                                     in toStr . last $ filter(\x -> BS.length x > 0 ) $ A.splitBS (c2w_ '/') p  
                         logFileG ["pdfimage fname =>" ++ fname]
                         respond $ sendImage "pdfimage" fname

   ("pdf":fn:_)     -> do
                         let fname = let p = rawPathInfo request 
                                     in toStr . last $ filter(\x -> BS.length x > 0 ) $ A.splitBS (c2w_ '/') p  
                         print fname
                         logFileG ["pdf fname =>" ++ fname]
                         respond $ sendPDF "pdf" fname 
                         -- respond $ pdfSentX $ strictTextToStrictByteString fn 
   ("loginCheck":_)      -> loginCheck conn1 request respond
   ("insertUserDB":_)    -> insertUserDB conn1 request respond
   ("insert":_)          -> insertDatabase conn1 request respond
   ("upload":_)          -> upload updir request respond
   ("getjson":_)         -> upload updir request respond

   ("snippet":_)         -> do
                            -- mayCmd = Just "s grep"
                            let mayCmd = toStr <$> getQueryString "id" request
                            logFileG ["mayCmd=>" ++ show mayCmd]
                            case mayCmd of
                                Just cmd -> redisSet keyLastCmd cmd
                                Nothing  -> redisSet keyLastCmd []
                            respond $ responseFromCmd conn1 ref mayCmd 
                            -- respond $ anyRoute2 conn1 ref request   -- anyRoute => Response, respond (Response) => IO ResponseReceived

   -- See $b/jslib/aronlib.js, send Ajax to server
   -- url = "http://localhost:8080/json"; in postMatrix.html
   -- geneRectMat::Application
   ("json":_)            -> geneRectMat request respond
   -- test json
   ("testjson":_)         -> do
                               -- jsonFile src/datadir/latex/indexEditorACE/indexEditorACE.json
                               jsonFile <- datadirFull "indexEditorACE" EJSON
                               mjson <- jsonToRecord jsonFile :: IO (Maybe EditorCode)
                               pre mjson
                               case mjson of
                                 Just record -> respond $ responseJSON record
                                 Nothing -> respond $ responseHelp
   ("htmlfetchjson":_)         -> respond htmlfetchjson

  
   ("htmltable":_)       -> geneHTMLTable request respond
   ("updatebackground":_) -> updateBackground request respond  -- change background color
   -- ("updatetextcolor":_) -> updateTextColor request respond    -- change text color

   ("editor":_)          -> respond replyEditor
   ("search":_)          -> respond searchUI
   ("wordcount":_)       -> respond replyCssButton
   ("wordcount_reply":_) -> respond wordcountReply
   -- ("matrix":_)          -> respond matrixReply
   ("matrix":_)          -> respondMatrix conn1 ref request respond
   ("htmltablecmd":_)    -> respond sendHTMLTableCmd
   ("compiler":_)        -> receiveCode request respond
   ("editcode":_)        -> respond $ responseHtml "compileCode.html"      -- haskellwebapp2/compileCode.html
   ("getcolor":_)        -> getPreFromRedis request respond -- Send JSON PreColor{color::TS.Text, background::TS.Text} to client side, in aronlib.js
   ("updatecode":_)      -> updateCodeBlock conn1 ref request respond -- CodeBlockReply{ok::String, retcmd::String, retbegt::Integer, retendt::Integer} deriving (Generic, Show) 
                                                                      -- SEE: aronlib.js updateCodeBlock
   ("addscore":_)        -> addScoreCodeBlock conn1 ref request respond
   ("subtractscore":_)   -> subtractScoreCodeBlock conn1 ref request respond
  
   ("insertcode":_)      -> insertCodeBlock conn1 ref request respond -- Send JSON PreColor{color::TS.Text, background::TS.Text} to client side, in aronlib.js

   ("editordata":_)      -> receiveEditorData conn1 ref pdfMapRef request respond -- Receive Latex code from ACE editor, ace editor
   
   -- ("aceeditor":_)       -> respond $ responseHtml "indexEditorACE.html" -- Receive Latex code from ACE editor, ace editor
   --
   --                        NOTE: xfido.com/aceeditor?id=try919591
   --                        If try919591 is in the Redis db, return try919591.html else return default responseHelp
   --
   ("apijson":_)         -> let query = queryString request :: [(BS.ByteString, Maybe BS.ByteString)]
                                idParam = join $ lookup "id" query :: Maybe BS.ByteString
                            in case toStr <$> idParam of
                                 -- => try919591.html
                                 Just s  -> do
                                   redisValue <- redisGet s
                                   case redisValue of  -- indexEditorACEtry919591.html
                                        Just v -> do
                                          jsonFile <- datadirFull v EJSON
                                          jstr <- readFileStr jsonFile
                                          let decodeStr = DA.decode (toLBS jstr) :: Maybe EditorCode
                                          pre decodeStr
                                          case decodeStr of
                                            Nothing  -> return ()
                                            Just x -> logFileG [show x]  -- write to "/tmp/x.x"
                                          respond responseHelp
                                        _      -> respond responseHelp
                                 Nothing  -> respond responseHelp

   ("aceeditor":_)       -> let query = queryString request :: [(BS.ByteString, Maybe BS.ByteString)]
                                idParam = join $ lookup "id" query :: Maybe BS.ByteString
                            in case toStr <$> idParam of
                                 -- => try919591.html
                                 Just s  -> do
                                   redisValue <- redisGet s
                                   case redisValue of  -- indexEditorACEtry919591.html
                                     -- Just v -> respond $ responseHtml $ v ++ ".html" -- Receive Latex code from ACE editor, ace editor
                                     --  src/datadir/latex/try919591/try919591.html
                                     Just v -> do
                                       -- TODO:
                                       -- load src/datadir/latex/try919591/try919591.json
                                       jsonFile <- datadirFull v EJSON
                                       jstr <- readFileStr jsonFile
                                       let decodeStr = DA.decode (toLBS jstr) :: Maybe EditorCode
                                       case decodeStr of
                                         Nothing  -> return ()
                                         (Just x) -> logFileG [show x]  -- write to $glog
   
                                       htmlFile <- datadirFull v EHTML
                                       respond $ responseHtml htmlFile  --  src/datadir/latex/try919591/try919591.html
                                     _      -> respond responseHelp
    
                                 -- xfido.com/aceeditor
                                 -- return default latex pdf file according to indexEditorACE.html
                                 Nothing -> do
                                              ran <- randomName
                                              let pdfName = ran ++ ".pdf"
                                              fullrootdir <- getRootDirFull
                                              let name = (dropExt "indexEditorACE.html") ++ ran ++ ".html"  -- indexEditorACEtry919591.html
                                              let fullName = fullrootdir </> name
                                              -- FIXME: 
                                              copyFile (fullrootdir </> indexEditorHTML) fullName
                                              logFileG [fullrootdir </> indexEditorHTML]  -- write to "/tmp/x.x"
                                              
                                              -- mayEditorCode <- jsonToRecord "/dog" :: IO (Maybe EditorCode)
                                              mayEditorCode <- jsonToRecord (fullrootdir </> indexEditorJSON) :: IO (Maybe EditorCode)
                                              let jeditorcode = case mayEditorCode of
                                                    Just x -> x
                                                    Nothing -> error "Invalid JSON file: EditorCode"
                                              let clientCode = editorcode jeditorcode
                                              fl
                                              -- pre clientCode
                                              fl
                                              let hiddenHtml = [r|<input  type="hidden" id='idlatex' name="myname" value="|] <> pdfName <> [r|" /> |]
                                              -- KEY: video http://xfido.com/song/haskellwebapp2_help.mp4 
                                              -- let hiddenCompileOrSave = [r|<input  type="hidden" id='hidden_compile_save' name="compilesave" value="|] <> "savepage" <> [r|" /> |]
                                              let hiddenCompileOrSave = [r|<input  type="hidden" id='compilesaveID' name="compilesave" value="|] <> "compilepage" <> [r|" /> |]
                                                                            
                                              writeFileStr "/tmp/xx.x" clientCode
                                              let str = strWithSlash $ clientCode
                                              replaceFileLineNoRegex fullName hiddenLATEXCODE clientCode

                                              -- target='_blank' => open a new tab
                                              let hiddenPDF = [r|<a href="|] <> pdfName <> [r|" target='_blank' onclick='promptPDFName()'>PDF</a> |]
                                              replaceFileListStr [("hidden123", hiddenHtml), ("hidden444", hiddenPDF), (hiddenCOMPILESAVE, hiddenCompileOrSave)] fullName
                                                    
                                              -- logFileG ["aceeditor:_ newName => " ++ fullName]  -- write to $glog
                                              redisSet ran ran
                                              logFileG ["fullName=> " ++ fullName]
                                              respond $ responseHtml fullName -- Receive Latex code from ACE editor, ace editor
                                                    
   ("commandservice":_)  -> commandService conn1 ref request respond
   ("todojson":_)        -> todoPostJSON conn1 ref request respond   
   ("deletecode":_)      -> deleteCodeBlock conn1 ref request respond -- Send JSON PreColor{color::TS.Text, background::TS.Text} to client side, in aronlib.js
   []                    -> respond responseHelp
   _                     -> do
                              let pdfFile = toStr $ head $ pathInfo request
                              let mayFile = M.lookup pdfFile rmap

                              -- type RespMap = M.HashMap String String
                              -- RespMap : rmap contains all *.css *.pdf *.js *.html files
                              logFileG ["kkkmayFile => " ++ show mayFile]
                              logFileG ["kkkpdfFile => " ++ show pdfFile]
                              -- logFileG ["rmap =>    " ++ show rmap]
       
                              case mayFile of
                                   Just x -> case (lowerStr . takeExt) x of  -- MultiWayIf
                                                    var | var == ".css"  -> respond $ responseCSS x
                                                        | var == ".pdf"  -> respond $ responsePDF x
                                                        | var == ".png"  -> respond $ responsePNG x
                                                        | var == ".js"   -> respond $ responseJavascript x
                                                        | var == ".html" -> respond $ responseHtml x
                                                        | otherwise      -> respond $ responseHelp
                                   _      -> do
                                               fl
                                               -- pre rmap
                                               fl
                                               -- pre mayFile
                                               -- pdfRef => M.HashMap String String
                                               --        => ("try919591" "try919591")
                                               pdfmap <- readIORef pdfMapRef
                                               let mls = M.toList pdfmap
                                               -- queryId = try919591
                                               let queryId = dropExt pdfFile
                                               -- logFileG ["queryId => " ++ queryId]  -- write to $glog
                                               -- logFileG $ map show mls -- write to $glog
                                               osMap <- confMap configFile
                                               let datadirlatex = lookupJust "datadirlatex" osMap   -- "src/datadir/latex"
                                               -- => xfido.com/aceeditor?id=try919591
                                               --   => try919591.html
                                               -- DONE: TODO:   => try919591.pdf  => src/latex/try919591.pdf

                                               -- => datadirlatex </> v </> v ++ ".pdf"
                                               -- path=>src/datadir/latex/try977655/try977655.pdf
                                               -- URL: xfido.com/image/haskellwebapp2_fetch_pdf.png
                                               redisValue <- redisGet queryId
                                               -- logFileG ["redisValue=>" ++ show redisValue]
                                               case redisValue of
                                                 Just v -> do
                                                             let path = datadirlatex </> v </> v ++ ".pdf"
                                                             logFileG ["path=>" ++ path]
                                                             -- KEY: send pdf file to client side
                                                             respond $ responsePDF path
                                                 _      -> do
                                                             logFileG ["ERROR: WaiLib.hs : pdf file not found in Redis =>  " ++ queryId]
                                                             respond $ responseNothing "ERROR : PDF => pdf file can not be found in Redis"


{-|
  NOTE: USE in main.hs
-}
resourceList::IO [(String, String)]
resourceList = do
  fullrootdir <- getRootDirFull
  logFileG ["fullrootdir=" ++ fullrootdir]
  osMap <- confMap configFile
  let datadirlatex = lookupJust "datadirlatex" osMap   -- "src/datadir/latex"
  let f n = if let s1 = containStr "src/css" n
                   s2 = containStr "src/js" n
                   s3 = containStr "pdf/" n  -- pdf
                   s4 = containStr "pdfimage/" n  -- png
                   s5 = containStr datadirlatex n
                   s6 = containStr "ace/theme" n
                   s7 = containStr "ace/mode" n
                   s8 = containStr "ace/build" n
                   s9 = matchTest (mkRegex "/.*\\.html") n
               in s1 || s2 || s3 || s4 || s5 || s6 || s7 || s8 || s9 then return [n] else return []
  ls <- dirWalk fullrootdir f
  fl
  -- pre ls
  logFileG ls 
  fl
  let matchFileExt x = elem (takeExt x) [".css", ".js", ".pdf", ".html", ".png"]
  
  let fls = map(\x -> (takeName x,  dropPath 1 $ concat $ dropWhile (not . containStr "haskellwebapp2")  $ splitPath x)) $ filter (\x -> matchFileExt x) ls
  return fls

plainIndex::Response
plainIndex = responseFile
    status200
    [("Content-Type", "text/html")]
    "index.html"
    Nothing

pdfFile::Response
pdfFile = responseFile
    status200
    [("Content-Type", "text/html")]
    "pdf.html"
    Nothing
    
{-|
    === Send pdf file to browser 
-}
sendPDF::FilePath -> FilePath -> Response
sendPDF dir fn = responseFile
    status200
    [("Content-Type", "application/pdf"),
     ("Content-Disposition", "inline;filename=" <>  fn')]
    (dir </> fn)
    Nothing
    where
        fn' = toSBS fn
        pdfdir = "pdf/"


{-|
    === Send png file, image file to browser 
-}
sendImage::FilePath -> FilePath  -> Response
sendImage dir fn = responseFile
    status200
    [imgType,
     ("Content-Disposition", "inline;filename=" <>  fn')]
    (dir </> fn)
    Nothing
    where
        imgType = case takeExt fn of
                        v | lowerStr v == ".png"  -> ("Content-Type", "image/png")
                        v | lowerStr v == ".gif"  -> ("Content-Type", "image/gif")
                        v | lowerStr v == ".jpg"  -> ("Content-Type", "image/jpeg")
                        v | lowerStr v == ".jpeg" -> ("Content-Type", "image/jpeg")
                          | otherwise -> ("Content-Type", "")
        fn' = toSBS fn
        pdfdir = "pdf/"

pdfSentX::BS.ByteString -> Response
pdfSentX fn = responseFile
    status200
    [("Content-Type", "image/png"),
     ("Content-Disposition", "inline;filename=" <>  fn)]
    (BU.toString $ pdfdir <> fn)
    Nothing
    where
        pdfdir = "pdf/"

insertinfo::Response
insertinfo = responseFile
    status200
    [("Content-Type", "text/html")]
    "insert.html"
    Nothing

insertUser::Response
insertUser = responseFile
    status200
    [("Content-Type", "text/html")]
    "insertUser.html"
    Nothing

loginHtml::Response
loginHtml = responseFile
    status200
    [("Content-Type", "text/html")]
    "login.html"
    Nothing
  
htmlfetchjson::Response
htmlfetchjson = responseFile
    status200
    [("Content-Type", "text/html")]
    "fetchjson.html"
    Nothing


searchUI::Response
searchUI = responseFile
    status200
    [("Content-Type", "text/html")]
    "searchUI.html"
    Nothing


notFound :: Response
notFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"


notFoundStr::IN.ByteString->Response
notFoundStr s = responseLBS
    status404
    [("Content-Type", "text/plain")]
    s 

-- let path = "/Users/aaa/myfile/bitbucket/snippets/snippet_test.m
snippetP = "myfile/bitbucket/snippets/snippet_test.hs"

-- snippetP = "myfile/bitbucket/snippets/snippet.hs"


mapClear2::[String] -> HMap2 -> HMap2
mapClear2 cx m = foldl (flip M.delete) m cx

insertAll2::[(String, [([String], Integer, Integer, Integer)])] -> HMap2 -> HMap2
insertAll2 [] m = m 
insertAll2 (x:cx) m = insertAll2 cx (insertAppend2 (fst x) (snd x) m)
          
insertAppend2::String -> [([String], Integer, Integer, Integer)] -> HMap2 -> HMap2
insertAppend2 k ls m = M.insert k (ls ++ rls) m
      where 
          rls = fromMaybe [] (M.lookup k m)

{-|
    == Generate 'HMap2' from a list of codeblock

    >Data HMap2 = M.HashMap String [([String], Integer, Integer, Integer)]
-}
listToPrefixMap::[([String], ([String], Integer, Integer, Integer))] -> IORef HMap2 -> IO ()
listToPrefixMap pplist ref = do
        -- let path = "/Users/aaa/myfile/bitbucket/snippets/snippet_test.hs"
        -- let path = "/Users/aaa/myfile/bitbucket/snippets/snippet.hs"

        -- readSnippet::FilePath->IO [([String], [String])]
        -- pplist <- readSnippet path
        -- fw "\npplist"
        -- pre pplist
        let keylist = L.map(\x -> (join $ L.map(\y -> prefix y) (fst x), snd x) ) pplist 

        -- fw "keylist"
        -- pre keylist
        -- [(["dog", "cat"], ([String], Integer))]
        -- keylist = [(["d", "do", "dog", "c", "ca", "cat"], ([String], Integer))]
        let mymap = map(\cx -> [(x, y) | x <- fst cx, y <- [snd cx]]) keylist
        let lmap = join mymap  -- use join
        -- fw "mymap"
        -- pre mymap
        -- pre $ typeOf lmap
        -- sort x of [(x, y)]
        let sortedList = qqsort(\x y -> f x y) lmap                                        
              where f x y = fst x > fst y
        -- convert list [(x, y)] to map
        let mmap = M.fromList lmap                                                         
        let group = groupBy(\x y -> f x y) sortedList                                       
              where f x y = fst x == fst y                                 

        -- fw "group"
        -- pre group
        --
        -- unzip::[("dog", "dogs"), ("cat", "cats")] => (["dog", "cat"], ["dogs", "cats"])
        let uzip = map(\x -> unzip x) group
        -- fw "uzip"
        -- pre uzip

        -- Fixed bug: unique $ snd x => remove duplicated values
        -- cause duplicated blocks: let tupleList = map(\x -> (head . fst $ x, snd x)) uzip
        -- tupleList => [("haskell", [["dog", "line1"], ["cat", "line2"]])]
        -- tupleList => [(String, [[String]])
        let tupleList = map(\x -> (head . fst $ x, unique $ snd x)) uzip
        -- fw "tupleList"
        -- pre tupleList
        -- NOTE: Sorting here is bad idea
        -- let tupleList' = map(\(k, s) ->(k,  qqsort(\(_,id,time,score) (_,id',time', score') -> score > score') s)) tupleList
        -- fw "tupleList'"
        -- pre tupleList'

        -- modifyIORef::IORef a -> (a -> a) -> IO()
        -- modifyIORef ref (insertAll2 tupleList)
        modifyIORef ref (insertAll2 tupleList)
        hmap <- readIORef ref
        -- fw "hmap"
        -- pre hmap
        return () 
                
                                    
-- type HMap2 = M.HashMap String [([String], Integer, Integer, Integer)]
spanBlockX1::([[String]]->[[String]])-> HMap2 -> (Maybe BS.ByteString) -> String
spanBlockX1 f hmap mKey = foldListList2 f $ case (M.lookup (toStr $ fromJust mKey) hmap) of 
                                     Just s -> s  -- [([String], Integer, Integer, Integer)]
                                     _      -> [(["span Block: spanBlockX1 => nothing"], 0, 0, 0)] -- Just s -> [["my cool code", "my nice code"]]


spanBlockFunc::([[String]]->[[String]])-> [[String]]->String
spanBlockFunc f codeblock = foldListList f codeblock

                         

-- USED 
spanBlockXX2::HMap2 ->(Maybe BS.ByteString)->String
spanBlockXX2 hmap mKey = foldListListTxt2 $ case (M.lookup (toStr $ fromJust mKey) hmap) of 
                                     Just s -> s
                                     _      -> [(["spanBlockXX2: nothing Txt"], 0, 0, 0)]
(∘) = (++)



htmlPre::String -> String
htmlPre s = [r| <pre style="font-size:29px;white-space: pre-wrap;" id="id00"> |] <> s <> [r| </pre> |]

{-|
    == User input, autocomplete, search field
-}
replyHtml::String->String->String
replyHtml s listCmd = [r|
            <HTML>   
            <HEAD>   
            <meta charset="utf-8">
            <TITLE>Search Code Snippet</TITLE>
            <LINK rel="stylesheet" type="text/css" href="css/mystyle.css"> 
            <script src="js/aronlib.js"></script>
            <!--
            <LINK rel="stylesheet" type="text/css" href="/style.css"> 
            -->
            </HEAD>
            <BODY> 
            |] <> s <> [r| </BODY></HTML> |]

{-| 
    snippet?id=queryStr
    S8.unpack: ByteString to String
    type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
    anyRoute => Response

    :NOTE: Use 
-}                 
anyRoute2::Connection -> IORef HMap2 -> Request-> Response
anyRoute2 conn ref req =
    -- get query from client
    -- look up the value of id, e.g. snippet?id=value
    -- http://localhost:8080/snippet?id=n%20test
    --                                    ↑
    --                                    + space = %20
    -- Maybe s 
    -- search s from the HMap
    -- replace the format html if any value is found
    -- Otherwise, reply "nothing"
    let query = queryString req :: [(BS.ByteString, Maybe BS.ByteString)]
        cmd = let mayText = join $ lookup "id" query :: Maybe BS.ByteString in toStr <$> mayText
    in  responseFromCmd conn ref cmd 
            -- responseBuilder :: Status -> ResponseHeaders -> Builder -> Response
{-|            
    -- http://localhost:8080/snippet?id=n%20test
    --                                    ↑
    --                                    + space = %20
-}
getQueryString::BS.ByteString -> Request -> Maybe BS.ByteString 
getQueryString ids req = let query = queryString req :: [(BS.ByteString, Maybe BS.ByteString)]
                         in join $ lookup ids query :: Maybe BS.ByteString

responseFromCmd::Connection -> IORef HMap2 -> Maybe String -> Response
responseFromCmd conn ref cmd = 
       case cmd of  
            -- responseBuilder :: Status -> ResponseHeaders -> Builder -> Response
            Just s -> do 
                      -- record command and write to file
                      -- store s in Redis here
                      case s of
                           var | len var > 3 -> case take 2 s of
                                 var | var == "c " -> responseCmd conn s     -- Shell commands
                                     | var == "j " -> responseJavaHtml s     -- Java AronLib.java with Html, CSS.
                                     | var == "h " -> responseHaskellHtml s  -- Haskell AronModule.hs with HTML, CSS.
                                     | var == "k " -> queryLibHaskell s      -- Haskell AronModule.hs No HTML
                                     | var == "x " -> queryLibCpp s          -- Cpp     AronLib.hs No HTML
                                     | var == "i " -> queryLibJavaPackage "Aron." s       -- Java $b/javalib/AronLib.java
                                     | var == "e " -> queryRedisSnippet s
                                     | var == "p " -> queryLibJavaPackage "Print." s -- Java $b/javalib/Print.java
                                     | var == "n " -> responseSnippetTxt2 s ref  -- Snippet with No HTML, CSS.
                                     | var == "o " -> responseSnippetJSON s ref  -- Snippet with Json format
                                     | var == "s " -> responseSnippetHTML2 conn s ref -- Snippet with Html,No Search field
                                     | otherwise   -> responseNothing ""  -- responseSearch conn "search 1"
                               | otherwise   -> responseNothing "nothing55" -- responseSearch conn "search 2" 
            _      -> responseSearch conn "response nothingkkk"

-- | http://localhost:8000/up/
-- | NOTE: file => /upload dir
-- | Plz see uploadPage.html 
-- | /Users/aaa/myfile/bitbucket/haskellwebapp2/uploadPage.html
-- | responseFile :: H.Status -> H.ResponseHeaders -> FilePath -> Maybe FilePart -> Response
uploadPage::Response
uploadPage = responseFile
    status200
    [("Content-Type", "text/html")]
    "uploadPage.html"
    Nothing

data SearchType = CmdT | JavaT | HaskellT | SnippetT

readCmd::FilePath -> SearchType -> IO String
readCmd fn t = do
               -- cmdList  <- readFileToList fn
               cmdList  <- readFileLatin1ToList fn
               -- filter out "va" from list and apply f to each element
               let sortedList = groupCountFilter cmdList 
               let tupList = show <$> groupCount sortedList
               -- writeToFile "/tmp/aa.x" tupList
               -- let sortedList = ["33", "11", "22", "bb", "aa"]
               let htmlStr = concat $ map(\x -> let left = "<option value=\""; 
                                                    right = "\">" 
                                                in case t of 
                                                        CmdT     -> left ++ x ++ right
                                                        JavaT    -> left ++ x ++ right
                                                        HaskellT -> left ++ x ++ right
                                                        SnippetT -> left ++ x ++ right
                                         ) $ sortedList 
               return htmlStr 


{-| 
    === Filter some commands out from a list.
-} 
groupCountFilter::[String] -> [String]
groupCountFilter cs = fst <$> groupCount (let f::String -> Maybe String
                                              f "va" = Nothing
                                              f s    = Just s
                                          in filtermap ( f . trim) cs)
optionHtml::[String] -> String
optionHtml cs = concat $ map(\x -> [r| <option value="|] <> x <> [r|">|]) cs 

responseNothing::String -> Response                                                    
responseNothing s = responseStream                                                   
              status200                                                            
              [(hContentType, "text/html")] $ \write flush -> do                   
              write $ byteString $ toSBS ("responseNothing : " ++ s)

responseNothingBS::BS.ByteString -> Response                                               
responseNothingBS bs = responseStream                                                   
              status200                                                            
              [(hContentType,  "application/json")] $ \write flush -> do                   
              write $ byteString bs

{-|
    === Response JSON record to client side
-}
responseJSON::(DA.ToJSON a) => a -> Response
responseJSON rd = responseJSONBS $ (toSBS . DA.encode) rd
              
responseJSONBS::BS.ByteString -> Response
responseJSONBS bs = responseStream
              status200
              [(hContentType,  "application/json")] $ \write flush -> do                   
              write $ byteString bs  


responseNothingTest::Response                                               
responseNothingTest = responseStream                                                   
              status200                                                            
              [(hContentType,  "application/pdf"),
               ("Content-Disposition", "inline;filename=kkk.pdf")] $ \write flush -> do                   
              write $ byteString $ toSBS ("dog"::String)

replyTaskHtml::BS.ByteString -> BS.ByteString -> BS.ByteString 
replyTaskHtml url s = [r|
            <HTML>   
            <HEAD>   
            <meta charset="utf-8">
            <TITLE>Search Code Snippet</TITLE> 
            <LINK rel="stylesheet" type="text/css" href="/style.css"> 
            </HEAD>
            <BODY> 
            <div style="text-align:center;">
            <br>
            <p> |] <> s <> [r|</p><br><a href= |] <> url <> [r|>Back</a></div></BODY></HTML> |]


            -- <p> |] <> s <> [r|</p><br><a href="http://localhost:8000">Back</a></div></BODY></HTML> |]

--listPage::BS.ByteString -> Response                                               
--listPage bs = responseStream                                                   
--              status200                                                            
--              [(hContentType,  "text/html")] $ \write flush -> do                   
--              write $ byteString bs  

listPage::Connection -> Application
listPage conn req response = do
              userList <- query_ conn "SELECT uid, name, email, password, task, money FROM user" :: IO [User]
              let listTask = BS.concat $ map (\x -> [r|<div>|] <> (t2b $ task x) <> [r|</div><br>|]) userList -- => TS.Text
              -- response $ responseTaskBS (task (head userList))
              hostURL <- getHostName >>= return . toSBS
              response $ responseTaskBS $ replyTaskHtml hostURL listTask 
        where
            t2b = strictTextToStrictByteString

responseTaskBS::BS.ByteString -> Response                                               
responseTaskBS bs = responseStream                                                   
               status200                                                            
               [(hContentType,  "text/html")] $ \write flush -> do                   
               write $ byteString bs  



--writeSB::String -> Builder
--writeSB = write $ byteString $ strToStrictByteString 

  -- write $ byteString $ toSBS $ replyHtml (escapeHtml retStr) listCmd 

{-|
   === Response output from shell command
   1. The maximum number of lines are 200, " | head -200"

   2. Exit code can not be checked, ExitSuccess
   
   Wed May  8 23:10:41 2019 
   3 Error can be checked in stderr  
   >(e, so, si) <- A.runSh $ toSText (drop 2 ncmd) 
   > if si is NOT empty then there is stderr

   >type streamBody = (Builder -> IO()) -> IO() -> IO()
 -} 
responseCmd::Connection -> String -> Response
responseCmd conn cmd = responseStream 
              status200
              [(hContentType, "text/html")] $ \write flush -> do
              -- code <- A.run (cmd ++ "\n")
              -- There is problem with top 500 lines
              -- It seems to be working with 200 lines 
              let ccmd = trim cmd
              let ncmd = ccmd ++ topN
                  
              -- Log the current user input. 
              -- logCurrCmd [ccmd]
              -- runSh :: TS.Text -> IO (ExitCode, TS.Text, TS.Text)    
              (e, so, si) <- runSh $ toSText (drop 2 ncmd) 
              -- ExitCode can not capture exit code of cmd1 in "cmd1 | cmd2"
              -- Wed May  8 23:27:04 2019 
              -- Fixed Error: 
              -- If there is error, si will NOT be empty String
              -- Otherwise, there is NO error.
              let ok = isOk si 
              let shellRet = if ok then (toStr so) else ("Invalid Shell Command:" ++ ncmd)
              -- if ok then writeToFileAppend cmdLog [ccmd] else return () 
              if ok then do
                  -- NOT USED
                  sortList <- queryUserInput conn ccmd
                  -- execute_ conn "CREATE TABLE IF NOT EXISTS userinput (id INTEGER PRIMARY KEY AUTOINCREMENT, xcmd TEXT)"
                  -- execute conn "INSERT INTO userinput (xcmd) VALUES (?)" (UserInput 0 (toSText ccmd))
                  -- cmdsql <- query_ conn "SELECT id, xcmd FROM userinput"::IO [UserInput]
                  -- let cmdList = map toStr (map (xcmd) cmdsql::[TS.Text]) -- [UserInput] => [Text] => [String]
                  -- pa cmdList 
                  -- let sortList = groupCountFilter cmdList
                  -- FIXME: replace htmlBody with htmlBody.htmlbs
                  -- FIXME: replace searchForm with searchForm.htmlbs
                  -- logFile2 "/tmp/bb1.txt" [htmlBodyH $ (searchForm (optionHtml sortList)) ∘ (htmlPre shellRet)]
                  -- write $ byteString $ toSBS $ htmlBodyH $ (searchForm (optionHtml sortList)) ∘ (htmlPre shellRet)
                  write $ byteString $ toSBS $ htmlPre shellRet
              else flush
        where
            topN = " | head -200"
            isOk si = (toStr si) == ""

queryUserInput::Connection -> String -> IO [String]
queryUserInput conn cmd = do
  execute_ conn Query{fromQuery = s2Text "CREATE TABLE IF NOT EXISTS userinput (id INTEGER PRIMARY KEY AUTOINCREMENT, xcmd TEXT)"}
  execute  conn Query{fromQuery = s2Text "INSERT INTO userinput (xcmd) VALUES (?)"} (UserInput 0 (toSText cmd))
  cmdsql <- query_ conn "SELECT id, xcmd FROM userinput"::IO [UserInput]
  let cmdList = map toStr (map (xcmd) cmdsql::[TS.Text]) -- [UserInput] => [Text] => [String]
  -- pa cmdList 
  return $ groupCountFilter cmdList
  
responseJavaHtml::String -> Response
responseJavaHtml cmd = responseStream 
              status200
              [("Content-Type", "text/html")] $ \write flush -> do
              let tcmd = trim cmd                  -- " j  list " => "j  list"
              let hKey  = trim $ drop 2 tcmd       -- "j  list"   => "list"
              let jCmd = redisKey hKey             -- "list"      => "Aron.list"
              ls <- A.run $ query_redis ++ jCmd   
              let lsEscapeHtml = map(\x -> escapeHtml x) ls
              let ls2 = map(\s -> [s]) lsEscapeHtml 
              -- let repStr = foldr(\x y -> x ++ "<br>" ++ y) [] code
              let repStr = H1.table ls2
              -- FIXME: take cmd from database
              -- writeToFileAppend cmdLog [tcmd] 
              -- listCmd <- readCmd cmdLog JavaT
              write $ byteString $ toSBS $ spanBlockFunc (alternateLineColor2 [("color", "#AAAAAA"), ("color", "white")] . transformX) [ls] 
              flush
              where
                redisKey s = "Aron." ++ s

{-|
  === Html table contains png/pdf file gallery
-}
responseGenePDFHtml::Connection -> Response                                                 
responseGenePDFHtml conn = responseStream                                                
              status200                                                              
              [("Content-Type", "text/html")] $ \write flush -> do
              -- bs <- getPDFPath "pdf/" >>= \path -> PDF.pdfMain conn path

              pa <- getEnv "PWD" >>= \x -> return $ x </> "pdf/"  -- TEST ONLY
              bs <- PDF.getPDFPath pa >>= \path -> PDF.pdfMain conn path  -- TEST ONLY
              write $ byteString bs  
              flush                                                                  

  
responseHaskellHtml::String -> Response
responseHaskellHtml cmd = responseStream 
              status200
              [("Content-Type", "text/html")] $ \write flush -> do
              -- append "AronModule." Reddis for Haskell lib
              let tcmd = trim cmd
              let hKey = redisKey $ trim $ drop 2 tcmd  -- "h list" => AronModule.list
              code <- A.run $ query_redis ++ hKey   
              let codeEsc = map(\x -> escapeHtml x) code -- ["code"]
              let repStr = foldr(\x y -> x ++ "<br>" ++ y) [] codeEsc -- ["code']
              pre repStr
              -- writeToFileAppend cmdLog [tcmd] 
              -- listCmd <- readCmd cmdLog HaskellT 
              -- write $ byteString $ toSBS $ replyHtml repStr ""
              write $ byteString $ toSBS $ spanBlockFunc (alternateLineColor2 [("color", "#AAAAAA"), ("color", "white")] . transformX) [code]
              flush
              where
                redisKey s = "AronModule." ++ s
              
queryRedisSnippet::String -> Response
queryRedisSnippet cmd = responseStream
                        status200
                        [("Content-Type", "text/html")] $ \write flush -> do
                        let tcmd = trim cmd
                        -- AronModule. ++ "h writeToFile" => AronModule.writeToFile
                        let qstr = drop 2 tcmd -- "h list" => "list"
                        let hKey = preKey ++ (trim $ qstr)   
                        pre hKey
                        code <- A.run $ query_redis ++ hKey   
                        pre code  
                        if len code > 0 
                          -- then let repStr = init $ foldr(\x y -> x ++ "\n" ++ y) [] tcode in write $ byteString $ toSBS repStr 
                        then let repStr = unlines code in write $ byteString $ toSBS repStr 
                        else write $ byteString emptySBS 
                        flush
                        where
                            preKey = "snippet."


{-| 
    === Run ghci_stack.sh as a process, return some info
-} 
ghcidRun::Ghci -> String -> Response
ghcidRun ghci cmd = responseStream 
        status200
        [("Content-Type", "text/html")] $ \write flush -> do
        let tcmd = drop 2 $ trim cmd
        putStrLn tcmd
        putStrLn cmd
        -- exec :: Ghci -> String -> IO [String]
        let executeStatement = exec ghci 
        sbs <- getGhciInfo tcmd executeStatement
        write $ byteString sbs 
        flush
 where
  f::Stream -> String -> IO()
  f a b = return ()

  getGhciInfo cmd f = do
       -- f str >>= print . head
       ls <- f cmd
       pre ls
--        let str = if (len ls > 0) then (let list = map (\x -> lastNon $ filterNonEmpty $ splitRegex (mkRegex "\\.") x) ls
--                                       in unlines list
--                                      ) else ""
       return $ toSBS $ unlines ls 

  lastNon::[String] -> String
  lastNon cx = if len cx > 0 then last cx else ""

{-| 
    === query function info from redis without Html

    >query_redis = "$HOME/myfile/symbin/RedisQuery "

    >preKey = "AronModule."  AronModule.hs
    >preKey = "Aron."        Aron.java
-}
queryLibHaskell::String -> Response
queryLibHaskell cmd = responseStream 
              status200
              [("Content-Type", "text/html")] $ \write flush -> do
              let tcmd = trim cmd
              putStrLn cmd

              -- AronModule. ++ "h writeToFile" => AronModule.writeToFile
              let qstr = drop 2 tcmd -- "h list" => "list"
              let hKey = preKey ++ (trim $ qstr)   
              pre hKey
              code <- A.run $ query_redis ++ hKey   
              fw "beg"
              pre code
              fw "end"
              pp $ "tcmd=" <<< tcmd

              let tcode = sortedByMatchedStrIndex qstr code
              fw "tcode beg"                           
              pre tcode
              fw "tcode end"
              if len tcode > 0 
              -- then let repStr = init $ foldr(\x y -> x ++ "\n" ++ y) [] tcode in write $ byteString $ toSBS repStr 
              then let repStr = unlines tcode in write $ byteString $ toSBS repStr 
              else write $ byteString emptySBS 
              flush
              where
                preKey = "AronModule."

                -- sorted string according to index of search term.
                -- gx file:///Users/aaa/myfile/bitbucket/stackproject/jupyterlab/sorted_haskell_function.html
                sortedByMatchedStrIndex qstr code = qqsort(\a b -> let la = matchIndex qstr $ head $ splitStr "::" a 
                                                                       lb = matchIndex qstr $ head $ splitStr "::" b 
                                                                   in  la < lb ) $ map trim code

                matchIndex q s = case matchAny q s of
                                   Just x -> fst x
                                   _      -> 10 

queryLibCpp::String -> Response
queryLibCpp cmd = responseStream 
              status200
              [("Content-Type", "text/html")] $ \write flush -> do
              let tcmd = trim cmd
              putStrLn cmd

              -- AronModule. ++ "h writeToFile" => AronModule.writeToFile
              let qstr = drop 2 tcmd -- "h list" => "list"
              let hKey = preKey ++ (trim $ qstr)   
              pre hKey
              code <- A.run $ query_redis ++ hKey   
              pre code
              if len code > 0 
              -- then let repStr = init $ foldr(\x y -> x ++ "\n" ++ y) [] tcode in write $ byteString $ toSBS repStr 
              then let repStr = unlines code in write $ byteString $ toSBS repStr 
              else write $ byteString emptySBS 
              flush
              where
                preKey = "AronLib."

                -- sorted string according to index of search term.
                -- gx file:///Users/aaa/myfile/bitbucket/stackproject/jupyterlab/sorted_haskell_function.html
                sortedByMatchedStrIndex qstr code = qqsort(\a b -> let la = matchIndex qstr $ head $ splitStr "::" a 
                                                                       lb = matchIndex qstr $ head $ splitStr "::" b 
                                                                   in  la < lb ) $ map trim code

                matchIndex q s = case matchAny q s of
                                   Just x -> fst x
                                   _      -> 10 


{-|
    === Should use 'queryLibJavaPackage'

    NOT BEEN USED
-} 
queryLibJava::String -> Response
queryLibJava cmd = responseStream 
              status200
              [("Content-Type", "text/html")] $ \write flush -> do
              let tcmd = trim cmd
              putStrLn cmd
              let hKey = preKey ++ (trim $ drop 2 tcmd)
              code <- A.run $ query_redis ++ hKey   
              let tcode = map trim code
              if len tcode > 0 
              then let repStr = init $ foldr(\x y -> x ++ "\n" ++ y) [] tcode in write $ byteString $ toSBS repStr 
              else write $ byteString emptySBS 
              flush
              where
                preKey = "Aron."

{-|
    === Get java $jlib/AronLib.java without HTML
-} 
queryLibJavaPackage::String -> String -> Response
queryLibJavaPackage preKey cmd = responseStream 
              status200
              [("Content-Type", "text/html")] $ \write flush -> do
              let tcmd = trim cmd
              putStrLn cmd
              -- preKey = Print.  tcmd = "p list"
              -- => hKey = Print.list
              let hKey = preKey ++ (trim $ drop 2 tcmd)
              code <- A.run $ query_redis ++ hKey   
              let tcode = map trim code
              if len tcode > 0 
              then let repStr = init $ foldr(\x y -> x ++ "\n" ++ y) [] tcode in write $ byteString $ toSBS repStr 
              else write $ byteString emptySBS 
              flush
              where
                -- preKey = "Aron."



{-| 
    === Get user input: cmd = "s java regex", autocomplete commands

   TODO1 add host name here
   name <- run "uname"
   ls <- run "uname" >>= \x -> if len x > 0 then return head x else []

   :NOTE: USE in anyRoute2

   @
   data Response
       = ResponseFile H.Status H.ResponseHeaders FilePath (Maybe FilePart)
       | ResponseBuilder H.Status H.ResponseHeaders Builder
       | ResponseStream H.Status H.ResponseHeaders StreamingBody
       | ResponseRaw (IO B.ByteString -> (B.ByteString -> IO ()) -> IO ()) Response
     deriving Typeable

   responseStream::Status -> ResponseHeaders -> StreamingBody -> Response
   type StreamingBody = ( Builder -> IO() ) -> IO () -> IO ()
   @


-} 
responseSearch::Connection -> String -> Response                                                    
responseSearch conn s = responseStream                                                   
              status200  -- Status     ↓---- ResponseHeaders                                                        
              [(hContentType, "text/html")] $ \write flush -> do                   
              --                                ↑     ↑
              --                                |     --------> IO ()
              --                                --------------> Builder -> IO()
              -- create table if table: userinput does not exist
              execute_ conn sql_create_table 
              cmdsql <- query_ conn sql_select ::IO [UserInput]
              let cmdList = let ls = map (xcmd) cmdsql::[TS.Text] in map toStr ls::[String]
              let sortList = groupCountFilter cmdList
              -- limit the number of commands
              let autoList = take 20 sortList
              -- bs <- readFileRepPat "src/htmlBody.htmlbs" "replacekey00" $
              -- bs <- (readFileRepPat "src/searchForm.html" "replaceSearchForm" $ toSBS $ optionHtml autoList)
                     -- >>= readFileRepPat "src/htmlBody.html" "replacekey00" 
              -- let htmlByteStr = toSBS $ htmlBodyH  $ (searchForm (optionHtml autoList))
              -- ls <- runCmd "uname"
              -- let osName = if len ls > 0 then head ls else []
              -- <input  type='hidden' id='osid' name="myname" value="Bjner Stroustrup Cheetah Chateau">
              -- let divStr = [r|<input type='hidden' id='osid' value='|] <> osName <> [r|' />|]
              -- let divSBS = toSBS divStr
              -- let osidStr = "hiddenosid"::BS.ByteString
              -- let bb = searchReplaceAnySBS bs osidStr divSBS
                  
              rls <- runCmd "uname"
              let osName = if len rls > 0 then head rls else []
              bb <- searchMainHtml autoList osName    
              writeFileListBS "/tmp/bs1.html" [bb]
              -- write $ byteString $ BS.concat ([bs] ++ [divSBS])
              logFileG ["logme"] -- /tmp/x.x
              write $ byteString $ BS.concat [bb]
              -- ↑            ↑
              --              --- byteString::ByteString -> Builder
              -- write::Builder -> IO()
              -- 
          where 
            sql_create_table = "CREATE TABLE IF NOT EXISTS userinput (id INTEGER PRIMARY KEY AUTOINCREMENT, xcmd TEXT)"
            sql_select = "SELECT id, xcmd FROM userinput"


searchMainHtml::[String] -> String -> IO BS.ByteString
searchMainHtml autoList osName = do
  bs <- (readFileRepPat "src/searchForm.html" "replaceSearchForm" $ toSBS $ optionHtml autoList)
                     >>= readFileRepPat "src/htmlBody.html" "replacekey00" 
  let divStr = [r|<input type='hidden' id='osid' value='|] <> osName <> [r|' />|]
  let divSBS = toSBS divStr
  let osidStr = "hiddenosid"::BS.ByteString
  let bb = searchReplaceAnySBS bs osidStr divSBS
  ls <- getPreStyle
  let s = concatStr (map cssToStr ls) []
  let sub = toSBS $ "<style>.co0{" + s + "}</style>"
  let pat1 = "replacestylecolor"::BS.ByteString
  let b1 = searchReplaceAnySBS bb pat1 sub
  return b1
  where
    (+) = (++)
            
{-| 
    === Get user input: cmd = "s java regex", autocomplete commands

    1. remove spaces from cmd
    2. insert cmd to table: userinput if userinput exists, otherwise create table: userinput
        1. sorted all cmd and create Html form with all cmd
        2. create Html output from cmd query.

        + store user input commands, autocomplete commands in a table: userinput
        + if table does not exist, create one, otherwise insert data to table: userinput
        + autocomplete, query commands from sqlite table

    3. No Search field, only query blocks data

    :NOTE: USE in anyRoute2

    REFNOTE: /Users/aaa/myfile/bitbucket/publicfile/notdelete/codeblock.hs

    XXX
-}               
responseSnippetHTML2::Connection -> String -> IORef HMap2-> Response
responseSnippetHTML2 conn cmd ref = responseStream
              status200
              [("Content-Type", "text/html"), ("Access-Control-Allow-Origin", "*")] $ \write flush -> do
              let sCmd = trim cmd

              -- store user input commands, autocomplete commands in a table: userinput
              -- if table does not exist, create one, otherwise insert data to table: userinput
              -- autocomplete, query commands from sqlite table
              -- logCurrCmd [sCmd]
              -- create table if table: userinput does not exist
              execute_ conn sql_create_table 
              execute conn sql_insert (UserInput 0 (toSText cmd))
              cmdsql <- query_ conn sql_select ::IO [UserInput]
              let cmdList = let ls = map xcmd cmdsql::[TS.Text] in map toStr ls::[String]
              -- pa cmdList 

              let sortList = groupCountFilter cmdList
              -- limit the number of commands
              let autoList = take 20 sortList

              -- pa sortList 
              -- writeToFileAppend cmdLog [sCmd] 
              -- listCmd <- readCmd cmdLog SnippetT 
              -- write $ byteString $ toSBS $ replyHtml (spanBlock hmap (Just (toSBS (drop 2 sCmd)) )) listCmd 
              hmap <- readIORef ref
              pre sCmd
              fw "--"
              -- pre $ spanBlockX transform hmap (Just (toSBS (drop 2 sCmd)))
              -- let htmlByteStr = toSBS $ spanBlockX transform hmap (Just (toSBS (drop 2 sCmd)))
              -- let htmlByteStr = toSBS $ spanBlockX1 transform hmap (Just (toSBS (drop 2 sCmd)))
              let htmlByteStr = toSBS $ spanBlockX1 transformX hmap (Just (toSBS (drop 2 sCmd)))
              logFileG $ [toStr htmlByteStr]

              -- following line causes the server error
              -- writeFileBS "/tmp/b.html" htmlByteStr
              -- response html byte string
              -- byteString::ByteString -> Builder
              write $ byteString htmlByteStr 

              flush
              where 
                sql_create_table = "CREATE TABLE IF NOT EXISTS userinput (id INTEGER PRIMARY KEY AUTOINCREMENT, xcmd TEXT)"
                sql_insert = "INSERT INTO userinput (xcmd) VALUES (?)" 
                sql_select = "SELECT id, xcmd FROM userinput"
              
              

{-| 
    === Query snippet from HMap2 without Html

    :NOTE: USE in anyRoute2

    cmd -> "n emacs"
-}            
responseSnippetTxt2::String -> IORef HMap2 -> Response
responseSnippetTxt2 cmd ref = responseStream
              status200
              [("Content-Type", "text/html")] $ \write flush -> do
              let sCmd = trim cmd
              putStrLn cmd
              -- store command to log file
              -- TODO: Write cmd to database
              -- writeToFileAppend cmdLog [sCmd] 
              -- Get the HMap from IO reference
              hmap <- readIORef ref 
              -- drop 2 sCmd : "n java" => "java"
              -- key = "java" => use the key in hmap 
              -- response the byteString to client
              write $ byteString $ toSBS $ spanBlockXX2 hmap (Just (toSBS (drop 2 sCmd)))
              flush
  
-- spanBlockXX2 hmap mKey = foldListListTxt2 $ case (M.lookup (toStr $ fromJust mKey) hmap) of 
                                     -- Just s -> s
                                     -- _      -> [(["spanBlockXX2: nothing Txt"], 0, 0, 0)]


{-|
 === KEY: json snippet, json codeblock

 @
  SEE: $j/HttpRequestJson.java
       $scr/sj.sh
 @
 
-}
responseSnippetJSON::String -> IORef HMap2 -> Response
responseSnippetJSON cmd ref = responseStream
              status200
              [(hContentType,  "application/json")] $ \write flush -> do
              let sCmd = trim cmd
              putStrLn cmd
              -- store command to log file
              -- TODO: Write cmd to database
              -- writeToFileAppend cmdLog [sCmd] 
              -- Get the HMap from IO reference
              hmap <- readIORef ref
              let may = M.lookup (drop 2 sCmd) hmap
              let lt = case may of
                           Just cx -> map(\(a, n, _, _) -> (a, n)) cx
                           _      -> [(["nothing"], 0)]
              let ls = map fst lt  
              let lr = map snd lt
              -- drop 2 sCmd : "n java" => "java"
              -- key = "java" => use the key in hmap 
              -- response the byteString to client
              -- let snippet = SnippetJSON{name = "genematkk",  snippet = [["dog", "cat", "excited"], ["aa", "bb", "cc"]]}
              let snippet = SnippetJSON{pidls = lr, name = "responseSnippetJSON fun",  snippet = ls} 
              logFileG [show snippet] 
              let gbs = (toSBS . DA.encode) snippet
              write $ byteString gbs
              flush              

{-|
    === Generate matrix from Json javascript XMLHttpRequest

    * See *haskellwebapp2/postMatrix.html*
    * <file:///Users/aaa/myfile/bitbucket/haskellwebapp2/postMatrix.html postMatrix.html>
    * <http://localhost:8080/matrix Link To Matrix>
    *
    * Receive request from client
    * Decode body
    * Pattern matching Record Maybe 'GeneMatrix'
    * Construct record 'MatInt' if ncol and nrow are in 'GeneMatrix', otherwise create an empty 'MatInt'

    @
      MatInt{name="", matrix=[]}
    @
-} 
geneRectMat::Application
geneRectMat req response = do 
        str <- getRequestBodyChunk req
        
        let may = DA.decode $ toLBS str :: Maybe GeneMatrix 
        fw "may"
        print may
        let matJson = case may of 
                    (Just x) -> x 
                    _        -> GeneMatrix{cmd = "", ncol = 0, nrow=0} 
        fw "matJson"
        print matJson
        -- get ncol and nrow from client, otherwise set MatInt{name="", matrix=[]} 
        let gmatrix = case (cmd matJson) of 
                                -- genematrix is from file => haskellwebapp2/postMatrix.html
                                "genematrix" -> let nc = (ncol matJson) 
                                                    nr = (nrow matJson)
                                                in MatInt{name = "genemat", matrix = geneMatMN nc nr}
                                _            -> MatInt{name = "genemat", matrix = []} 
        let gbs = (toSBS . DA.encode) gmatrix
        fw "gbs"
        pre $ toStr gbs 
        let json = (toSBS . DA.encode) GeneMatrix{cmd = "mycmd", ncol=3, nrow=4}
        fw "str"
        S8.putStrLn str
        fw "response gbs"
        response $ responseNothingBS gbs

        
{-|
    === Generate HTML Table
-}
geneHTMLTable::Application
geneHTMLTable req response = do 
        str <- getRequestBodyChunk req        
        let may = (DA.decode . toLBS) str :: Maybe GeneMatrix 
        fw "may"
        print may
        let htabJson = case may of 
                    (Just x) -> x 
                    _        -> GeneMatrix{cmd = "", ncol = 0, nrow = 0} 
        fw "htabJson"
        print htabJson
        -- get ncol and nrow from client, otherwise set MatInt{name="", matrix=[]} 
        let htmlTable = case (cmd htabJson) of 
                                -- genematrix is from file => haskellwebapp2/postMatrix.html
                                "htmltable"  -> let nc = (ncol htabJson) 
                                                    nr = (nrow htabJson)
                                                in HTMLTable{name = "htmltable", matrix = htmlTableRowColSText nc nr}
                                _            -> HTMLTable{name = "htmltable", matrix = []} 
        let htab = (toSBS . DA.encode) htmlTable
        fw "htab"
        pre $ toStr htab
        fw "htmltable"
        pre htmlTable
        fw "str"
        S8.putStrLn str
        fw "response htab"
        response $ responseJSON htmlTable

       
{-|
	=== Get text color and background color from Redis

        * The function will be called from Ajax in aronlib.js
        * Click on the background/color => make a Ajax call

        @
         ("getcolor":_)        -> getPreFromRedis request respond -- Send JSON PreColor{color::TS.Text, background::TS.Text} to client side, in aronlib.js
        @

        > aronlib.js => Ajax => getPreFromRedis

        > getPreStyle::IO [(String, String)]

        * send JSON to client

        > data PreColor = PreColor{color::TS.Text, background::TS.Text}

        > redis-cli
        > keys 'HTMLPre.color'
        > keys 'HTMLPre.background-color'

        * Redis KEYS 'color'
        * Redis KEYS 'background-color'
-}
getPreFromRedis::Application
getPreFromRedis req response = do
                   styleList <- getPreStyle
                   let preColor = if len styleList == 2
                         then let m = M.fromList styleList
                              in  PreColor{color = case (M.lookup "color" m) of
                                                               Just x -> toSText x
                                                               _      -> ""
                                          ,
                                           background = case (M.lookup "background-color" m) of
                                                               Just x -> toSText x
                                                               _      -> ""
                                          }
                         else PreColor{color = "", background = ""}
                   -- Form a JSON object PreColor and send it back to client side
                   fw "preColor"
                   pre preColor
                   
                   when (color preColor == "") $ print "Redis has no key 'color' => WaiLib.hs getPreFromRedis"
                   when (background preColor == "") $ print "Redis has no key 'background' => WaiLib.hs getPreFromRedis" >> return ()
                   if (color preColor == "") then print "empty" else print "not empty"
                   response $ responseJSON preColor

{-|
 ==== KEY: Update snippet from client side

 * Need pid from client side

 SEE: 'hiddenForm2'
 @
 Post method 
    <form action="/update" name="Update" class="hf" id="f123" method="POST">
 
 -- In AronModule.hs
 data UpdateCodeBlock = UpdateCodeBlock{pid::Integer, newcode::String, begt::Integer, endt::Integer} deriving (GEN.Generic, Show)
                                         ↑ 
                                         + -> Primary Key in TABLE: CodeBlock

 Duplicate row id = pid,  and set show=0
 @           
-}
updateCodeBlock::Connection -> IORef HMap2 -> Application
updateCodeBlock conn ref req response = do
  str <- getRequestBodyChunk req
  let may = (DA.decode . toLBS) str :: Maybe UpdateCodeBlock
  fw "may"
  pre may
  let codeJson = case may of 
        (Just x) -> x 
        _        -> UpdateCodeBlock{pid = 0, newcode="no code", begt=0, endt=0} 
  fw "updateCodeBlock WaiLib.hs"
  pre codeJson
  -- Duplicate row id = pid,  and set show=0
  duplicatedRowNoShow conn $ pid codeJson
  updateDatabaseNewCodeTable conn (pid codeJson) (toSText $ newcode codeJson)
  -- if update ok, then send back "ok"
  cmd <- redisGetLastCmd keyLastCmd
  let begtClient = begt codeJson
  let upcodeblock = CodeBlockReply{ok = "True", retcmd = "update", retdata = cmd, retbegt = begtClient, retendt = 0}
  newList <- readDatabaseCodeBlock conn 
  -- pre newList
  -- read the map out from ref
  -- conver all the keys to list of keyssnippetMap::[([String], [String])] -> IORef HMap -> IO ()
  -- rehash the map
  -- type HMap = M.HashMap String [[String]] 
  -- IORef HMap => ref
  updatePrefixMap newList ref
    
--   hmap <- readIORef ref 
--   let keys = M.keys hmap
--   modifyIORef ref (mapClear2 keys)
--   listToPrefixMap newList ref
  response $ responseJSON upcodeblock

{-|
    === KEY: validate snippet format and padding

    * Check whether the input data is valid format
    * At least two lines
    * First line: a:b:c

    @
    snippet:*: code
    line 1

    a:b:c len ["a", "b", "c"] > 2


    Input:
    dog, cat pig
    line 1
 
    Padding:
    314:*:dog, cat pig
    line 1
   
    @
-}
validateFormat::String -> String  -> (Bool, String)
validateFormat s ran = if b then (False, s) else ((not . null) ls', unlines ls')
  where
    ls = lines s
    sp e = splitStr ":" e
    n = (len . sp . head) ls  -- a:b:c => len ["a", "b", "c"], n == 0 => padding with  random:*: xxx
    b = len ls <= 1
    x = head ls
    cs = tail ls
    pad ran = ran + ":*:" + x
    ls' = if n == 1 then (pad ran) : cs else ls  -- splitStr ":" "abc" => ["abc"]
    (+) = (++)

{-|
    === KEY: insert code block, insert code to database, update prefix map

    * It supports command line and Emacs

    @

    @
-}
insertCodeBlock::Connection -> IORef HMap2 -> Application
insertCodeBlock conn ref req response = do
  str <- getRequestBodyChunk req
  fw "str"
  pre str
  fw "req"
  pre req
  let may = (DA.decode . toLBS) str :: Maybe UpdateCodeBlock
  fw "may"
  pre may
  let codeJson = case may of 
        (Just x) -> x 
        _        -> UpdateCodeBlock{pid = 0, newcode = "no code insertCodeBlock", begt = 0, endt = 0} 
  fw "codeJson"
  pre codeJson
  let code = newcode codeJson
  let ls = lines $ toStr code
  pre ls
  pre $ splitStr ":" (head ls)
  pre code
  let begtClient = begt codeJson
  -- KEY: Last command, top command
  cmd <- redisGetLastCmd keyLastCmd
  let upcodeblock = CodeBlockReply{ok = "True", retcmd = "add", retdata = cmd, retbegt = begtClient, retendt = 0}
  randStr <- randomInteger 100000 1000000 >>= \x -> return $ intToString x
  
  let (isValid, code') = validateFormat (toStr code) randStr
  pre code'
  if isValid
    then do
    insertDatabaseNewCodeTable conn (pid codeJson) (toSText code')
    -- if update ok, then send back "ok"
    newList <- readDatabaseCodeBlock conn 
    -- pre newList
    -- read the map out from ref
    -- conver all the keys to list of keyssnippetMap::[([String], [String])] -> IORef HMap -> IO ()
    -- rehash the map
    -- type HMap = M.HashMap String [[String]] 
    -- IORef HMap => ref
    updatePrefixMap newList ref
    -- pre newList
    
    --   hmap <- readIORef ref 
    --   let keys = M.keys hmap
    --   modifyIORef ref (mapClear2 keys)
    --   listToPrefixMap newList ref
    response $ responseJSON upcodeblock
  else do
    putStrLn "ERROR: Invalid file format"
    let upcodeblock' = updateOk "False" upcodeblock
    response $ responseJSON upcodeblock'


data EditorCode = EditorCode{
  editorbeg::Integer,
  editorend::Integer,
  editorfile::String,
  editorcmd::String,
  editorcode::String,
  editortheme::String,
  editormode::String
  } deriving (Generic, Show)

instance DA.FromJSON EditorCode
instance DA.ToJSON EditorCode where
    toEncoding = DA.genericToEncoding DA.defaultOptions
      
data EditorCodeReply = EditorCodeReply{
  replybeg::Integer,
  replyend::Integer,
  ret::String,
  replydata::String,
  replyfname::String,
  replytheme::String,
  replymode::String
  } deriving (Generic, Show)

instance DA.FromJSON   EditorCodeReply
instance DA.ToJSON     EditorCodeReply where
    toEncoding = DA.genericToEncoding DA.defaultOptions

-- Lens implementation for EditorCodeReply
type MyLens a b = (a -> b, b -> a -> a)

-- BEG12 ret => field
getRet::EditorCodeReply -> String
getRet rd = ret rd

setRet::String -> EditorCodeReply -> EditorCodeReply
setRet s rd = rd {ret = s}

getReplytheme::EditorCodeReply -> String
getReplytheme rd = replytheme rd

setReplytheme::String -> EditorCodeReply -> EditorCodeReply
setReplytheme s rd = rd {replytheme = s}
-- END12

              
getL :: MyLens a b -> a -> b
getL (g, _) = g  -- getL (g, _) a = g a

setL :: MyLens a b -> b -> a -> a
setL (_, h) = h  --  setL (_, h) b a = h b a

modL :: MyLens a b -> (b -> b) -> a -> a
modL l f a = setL l (f (getL l a)) a

ret'::MyLens EditorCodeReply String
ret' = (getRet, setRet)

replytheme'::MyLens EditorCodeReply String
replytheme' = (getReplytheme, setReplytheme)
       
-- (^.)::a -> MyLens a b -> b
-- a ^. l = getL l a

(^=)::MyLens a b -> b -> a -> a
(l ^= b) a = setL l b a  -- (^=) = setL
       
       
                
-- instance Default EditorCodeReply where
--   def = EditorCodeReply 0 0 "" "" ""
      

data ProcLatex = ProcLatex {x1cmd :: String,
                            x1opt :: [String],
                            x1cwd :: String} deriving (Show)

myloop::String -> IO Int
myloop f = do
      putStrLn "loop it"
      fExist f >>= \b -> if b then return 1 else myloop f            

loopDelay::Int -> String -> IO Int
loopDelay max f = do
      threadDelay 1000000
      putStrLn $ "max=" ++ show max
      if max > 8000000 then return 0
        else fExist f >>= \b -> if b then return 1 else loopDelay (max + 1000000) f            

{-|

                               
                                 
                          ↓       ↓        ↓         ↓
-}
runOnExternalProgram :: String -> Int -> String -> FilePath -> IO (Either String String)
runOnExternalProgram pdflatex n fLatexName outdir = 
    -- convert the input to a parameter of the external program
    let x = show $ n + 12
        -- TODO: pass as argument
        -- pdflatex = "/usr/local/texlive/2021/bin/universal-darwin/pdflatex"
    -- bracketOnError:: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
    in bracketOnError
        -- (createProcess (proc "ls" []){std_in = CreatePipe
                                         -- ,std_out = CreatePipe
                                         -- ,std_err = Inherit})
        -- (createProcess (proc "/opt/local/bin/pdflatex" ["-output-directory", outdir, fLatexName </> "latex.tex"]){ cwd = Just fLatexName
        (createProcess (proc pdflatex ["-halt-on-error", "-output-directory", outdir,  fLatexName]){ cwd = Just (dropName fLatexName)
                                                    ,std_in = CreatePipe
                                                    ,std_out = CreatePipe
                                                    ,std_err = Inherit})
        (\(Just inh, Just outh, _, pid) -> terminateProcess pid >> waitForProcess pid)
        (\(Just inh, Just outh, _, pid) -> do

          -- fork a thread to consume output
          -- http://neilmitchell.blogspot.com/2012/06/flavours-of-mvar_04.html
          -- let loop f = do
                -- putStrLn "loop it"
                -- fExist f >>= \b -> if b then return 1 else loop f            
          mkdir $ outdir </> "myaaa"
          now <- timeNowSecond
          writeFileListAppend "/tmp/f.x" ["mkdir=>" ++ show now]
          let lo bo = if bo then bo else lo bo    
          output <- hGetContents outh
          outMVar <- newEmptyMVar
          -- forkIO $ evaluate (
            -- len ("a"::String)
            -- length output
            -- lo True
            -- myloop "/tmp/file.x" >>= \x -> return x
            --- length output
            -- ) >>= putMVar outMVar
          
          forkIO $ evaluate (
            do
            n <- loopDelay 1000000 fLatexName
            -- let ln = len output
            -- putMVar outMVar (n + ln)
            return n
            -- lo True
            -- myloop "/tmp/file.x" >>= \x -> return x
            --- length output
            ) >>= \x -> x >>= putMVar outMVar
          
          -- no input in this case
          hClose inh

          -- KEY: thread block, polling, wait on output, blocking until there is value in outMVar
          takeMVar outMVar
          now <- timeNowSecond
          logFileG ["after takeMVar=>" ++ show now]  -- write to $glog
          -- hClose outh

          -- wait for process
          ex <- waitForProcess pid

          case ex of
            ExitSuccess -> do
              -- convert the output as needed
              let verboseAnswer = "External program answered: " ++ output
              -- return verboseAnswer
              return $ Right verboseAnswer
            ExitFailure r -> do
              -- ioError (mkIOError OtherError ("spawned process exit: " ++ show r) Nothing Nothing) )
              logFileG ["ERROR: runOnExternalProgram: Compile error =>" ++ output]  -- write to $glog
              return $ Left ("ERROR: spawned process exit: " ++ show r ++ "runOnExternalProgram: Latex Compile ERROR: output=>" ++ output))


data LatexFilePath = LatexFilePath{xHtmlPath::String, xLatexPath::String, xPDFPath::String} deriving (Generic, Show)

randomName::IO String
randomName = ((++) "try") <$> (show <$> randomInt 100000 1000000)

--               HTML    PDF    JSON  file
data EFileType = EHTML | EPDF | EJSON

{-|
   KEY:

   TODO: add root directory config file?

   @
   configFile = "./config.txt"
   data EFileType = EHTML | EPDF | EJSON
   @
-}
datadirFull::String -> EFileType ->IO String
datadirFull tryRandom ftype = do
  osMap <- confMap configFile                           -- config.txt
  home <- getEnv "HOME"
  let rootdir      = lookupJust "rootdir" osMap         -- "myfile/mybin/haskellwebapp2Bin"
  let datadirlatex = lookupJust "datadirlatex" osMap    -- "src/datadir/latex"
  let fullPath = home </> rootdir </> datadirlatex
  let ext = case ftype of
        EHTML -> ".html"
        EPDF  -> ".pdf"
        EJSON -> ".json"
  return $ fullPath </> tryRandom </> tryRandom ++ ext  -- src/datadir/latex/try1515


{-|
 KEY: todo end point, todo app
-}
todoPostJSON::Connection -> IORef HMap2 -> Application
todoPostJSON conn ref req response = do
  tmpfile <- getEnv "mytmp"
  str <- getRequestBodyChunk req
  let may = (DA.decode . toLBS) str :: Maybe TodoItem
  pre may
  let todo = case may of 
        (Just x) -> x 
        _        -> TodoItem{todoId = 0, keyItem = "keyx", todoItem = "todoItem1"} 
  pre todo
  let todoId'     = todoId todo
  let keyItem'    = keyItem todo
  let todoItem'   = todoItem todo
  let errorSText  = toSText $ "ERROR: Invalid cmd:" ++ "ERR"
  let todoReply = TodoReply{
                    cmdReply = "From TodoReply"
                  } 

  -- execute_ conn "CREATE TABLE IF NOT EXISTS todoApp (id INTEGER PRIMARY KEY AUTOINCREMENT, keyItem TEXT, todoItem TEXT)"
  execute conn "INSERT INTO todoApp (key_item, todo_item) VALUES (?,?)" ( keyItem', todoItem')
  -- execute conn Query{fromQuery = s2Text "INSERT INTO todoApp (key_item, todo_item) VALUES (?,?)"} (keyItem', todoItem')
  response $ responseJSON todoReply

{-|
click on "compile"
 goto aronlib.js
  getElementsById("editor")
   get latex source code
    form JSON object => "compile"
                     => latex source code
    send to Server

 Server Side:
  ("editordata") ->
             receiveEditorData
                decode JSON object
                 => "compile"
                 => latex source code
                  => write latex source code to file $b/"latex.tex"
                  => pass path $b/latex.tex to
                       runOnExternalProgram
                         pdflatex compile latex.tex
                          => latex.pdf
       Either (String String) <= return from runOnExternalProgram

       Response to Client side in JSON
       => ret => "True" => Compile => OK
          ret => "False" => Compile => Error
          => On Client side aronlib.js
             => If ret == "True"
                  
           
-}
receiveEditorData::Connection -> IORef HMap2 -> IORef PDFMap -> Application
receiveEditorData conn ref pdfMapRef req response = do
  str <- getRequestBodyChunk req
  let may = (DA.decode . toLBS) str :: Maybe EditorCode
  fw "Receive data:"
  pre may
  let codeJson = case may of 
        (Just x) -> x 
        _        -> EditorCode{editorbeg = 0, editorend = 0, editorfile = "", editorcmd = "", editorcode = "no data from editor", editortheme = "", editormode = ""} 
  fw "codeJson"
  pre codeJson
  let editBeg   = editorbeg   codeJson                                               
  let editEnd   = editorend   codeJson                                               
  let editFile  = editorfile  codeJson -- editorfile => "try919591.pdf"
  let editCmd   = editorcmd   codeJson -- editorcmd  => "compile" or "save"
  let editCode  = editorcode  codeJson -- editorcode  => "latex source code" 
  let editTheme = editortheme codeJson                                               
  let editMode  = editormode  codeJson                                               

  let ls = splitStrChar "[/]" editMode

  -- if len ls == 0 then error "Error: editormode codeJson" else do
  --  logFile2 "/tmp/x.x" ["show codeJson"]
  osMap <- confMap configFile
  let datadirlatex = lookupJust "datadirlatex" osMap   -- "src/datadir/latex"
  
  mathPath <- getEnv "m"
  fullrootdir <- getRootDirFull
  
  pdflatex <- whichGetPath "pdflatex"
  
  ran <- randomName -- try919591

  if editCmd == "save" then do
    threadDelay 1000000
    -- ran <- randomName -- try919591

    -- IF user input from browser "Save", then use input name
    --  ELSE use random name => ranTexFile
    logFileG ["editFile =>" ++ editFile]  -- write to $glog
    let tryRandom = if (not . null) editFile then dropExt editFile else ran
        
    --  flatexFile = bitbucket/math/try919591.tex
    let flatexFile =  mathPath </> tryRandom ++ ".tex"
    writeFile flatexFile editCode
        
    --  outdirSave = haskellwebapp2/src/datadir/latex/try919591
    let outdirSave = fullrootdir </> datadirlatex </> tryRandom
    mkdir $ datadirlatex </> tryRandom

    htmlFile <- datadirFull tryRandom EHTML  -- "src/datadir/latex/try919591/try919591{.html, .pdf...}
    -- generate (random key, fileName.html, randomLatex.tex)
    copyFile (fullrootdir </> indexEditorHTML) htmlFile

    -- LIO.writeFile "/tmp/json.json" (encodeToLazyText codeJson)
    
    logFileG ["copy " ++  (fullrootdir </> indexEditorHTML) ++ " => " ++ htmlFile] -- write to "/tmp/x.x"
    pre htmlFile

    

    -- TODO: FIXME: \\ => \
    -- ls <- readFileListStrict htmlFile
    -- let ss = splitListWhen (\x -> (trim x) == "replace123" ) ls
    -- let ln = lines editCode  -- it seems to me lines ESCAPE \\ to \\\\ ?
    -- -- let ln = map(\x -> dropEnd 1 $ drop 1 $ show x) $ lines editCode
    -- let repls = (head ss) ++ ln ++ (last ss)
    -- writeFileList htmlFile repls

    -- replaceFileLineEscapeStrict htmlFile "replace123" editCode
    replaceFileLineNoRegex htmlFile hiddenLATEXCODE editCode
      
    -- replaceFileWithStr "replace123" editCode htmlFile
    let pdfName = dropExt (takeName htmlFile) ++ ".pdf"
    let hiddenHtml = [r|<input  type="hidden" id='idlatex' name="myname" value="|] <> pdfName <> [r|" /> |]
    let hiddenPDF = [r|<a href="|] <> pdfName <> [r|" onclick='promptPDFName()'>PDF</a> |]
    -- replaceFileWithStr "hidden123" hiddenHtml htmlFile
    -- KEY: video http://xfido.com/song/haskellwebapp2_help.mp4 
    let hiddenCompileOrSave = [r|<input  type="hidden" id='compilesaveID' name="compilesave" value="|] <> "savepage" <> [r|" /> |]
    replaceFileListWord [("hidden123", hiddenHtml), ("hidden444", hiddenPDF), (hiddenCOMPILESAVE, hiddenCompileOrSave)] htmlFile
    -- TODO: Fixed use random number => random file name
    -- TODO:           random number => user input name
    -- modifyIORef pdfMapRef $ M.insert ran ran
    -- modifyIORef pdfMapRef $ M.insert ran tryRandom
    modifyIORef pdfMapRef $ M.insert tryRandom tryRandom
    redisSet tryRandom tryRandom
    mkdir outdirSave
    logFileG ["mkdir => " ++ outdirSave]
    hostURI <- getHostName  -- http://xfido.com or localhost
    let replyURL = hostURI ++ "/aceeditor?id=" ++ tryRandom
    let upcodeblock = EditorCodeReply{replybeg = editBeg, replyend = editEnd, ret = "True", replydata = replyURL, replyfname = takeName flatexFile, replytheme = "", replymode = ""}
    -- Compile math/try919591.tex => outdirSave = src/latex/datadir/try919591/{try919591.pdf, try919591.log try919591.aux}
        
    -- FIXME: if file is Javascript, then the line can be ignored

    mayei <- timeout (5*1000000) $ runOnExternalProgram pdflatex 3 flatexFile outdirSave
    case mayei of
        Just ei -> case ei of
                     Right x -> do
                       putStrLn x
                       -- Save editor theme, mode, language
                       let jsonFile = outdirSave </> tryRandom ++ ".json"
                       LIO.writeFile jsonFile (encodeToLazyText codeJson)
                       -- response $ responsePDF $ dropExtension flatexFile ++ ".pdf"
                       -- hostURI <- getHostName  -- http://xfido.com or localhost
                       -- let replyURL = hostURI ++ "/aceeditor?id=" ++ ran
                       -- let replyURL = hostURI ++ "/aceeditor?id=" ++ tryRandom
                       -- logFile2 "/tmp/x.x" [tryRandom]
                       -- let upcodeblock = EditorCodeReply{replybeg = editBeg, replyend = editEnd, ret = "True", replydata = replyURL, replyfname = takeName flatexFile, replytheme = "", replymode = ""}
                       response $ responseJSON upcodeblock
                     Left  e -> do
                       putStrLn e
                       -- let upcodeblock = EditorCodeReply{replybeg = editBeg, replyend = editEnd, ret = "False", replydata = "save => Error", replyfname = takeName flatexFile}
                       let upcodeblock' = (ret' ^= "False") upcodeblock
                       response $ responseJSON upcodeblock'
        Nothing -> do
          let upcodeblock' = (ret' ^= "False") upcodeblock
          response $ responseJSON upcodeblock'
          -- ↑
          -- - - response = (Response -> IO ResponseReceived)
    
  else if editCmd == "compile" then do
    bitbucket <- getEnv "b"
    mathPath <- getEnv "m"
    -- let latexFile = "latex.tex"
    let latexName = dropExt editFile
    logFileG [editFile]  -- write to $glog
    logFileG [latexName] -- write to $glog
    let latexFile = if (not . null) editFile then latexName ++ ".tex" else error "editorfile => editFile CAN NOT BE EMPTY"
    let flatexFile = mathPath </> latexFile
    writeFile flatexFile editCode
    fl
    pre flatexFile
    fl
    pre editCode

    -- 
    -- file:///Users/aaa/myfile/bitbucket/image/4Tile.svg
    --
    -- runOnExternalProgram::Int -> String -> FilePath -> IO (Either String String)
    --                                                               ↑
    --                    ↓-------------------------------------------
    -- timeout::Int -> IO a -> IO (Maybe a)
    --                      => IO Maybe (Either String String)
    -- TODO: here
    let outdirCompile = fullrootdir </> datadirlatex </> latexName
    mkdir outdirCompile
    let upcodeblock = EditorCodeReply{replybeg = editBeg, replyend = editEnd, ret = "True", replydata = "compile => OK", replyfname = takeName flatexFile, replytheme = "", replymode = ""}
    logFileG [outdirCompile] -- write to $glog
    mayei <- let sec = 1000000 in timeout (5*sec) $ runOnExternalProgram pdflatex 3 flatexFile outdirCompile
    case mayei of
        Just ei -> case ei of
                     Right x -> do
                       putStrLn x
                       osMap <- confMap configFile
                       let datadirlatex = lookupJust "datadirlatex" osMap   -- "src/datadir/latex"

                       -- jsonFile <- datadirFull latexName EJSON
                       -- json <- jsonToRecord jsonFile :: IO (Maybe EditorCodeReply)
                       -- pre json
                       -- let upcodeblock = EditorCodeReply{replybeg = editBeg, replyend = editEnd, ret = "True", replydata = "compile => OK", replyfname = takeName flatexFile, replytheme = "", replymode = ""}
                       response $ responseJSON upcodeblock
                     Left  e -> do
                       putStrLn e
                       let upcodeblock' = (ret' ^= "False") upcodeblock
                       response $ responseJSON upcodeblock'
        Nothing -> do
                     -- let upcodeblock = EditorCodeReply{replybeg = editBeg, replyend = editEnd, ret = "False", replydata = "mayei => Nothing", replyfname = takeName flatexFile, replytheme = "", replymode = ""}
                     let upcodeblock' = (ret' ^= "False") upcodeblock
                     response $ responseJSON upcodeblock'
  else do
    let upcodeblock = EditorCodeReply{replybeg   = editBeg,
                                      replyend   = editEnd,
                                      ret        = "False",
                                      replydata  = editCmd ++ " : compile or save. Unknown option",
                                      replyfname = "",
                                      replytheme = "",
                                      replymode  = ""}
    response $ responseJSON upcodeblock


-- read postMatrix.html and replace osname with osName=Dawin, Linux, FreeBSD whatever it is
-- 
respondMatrix::Connection -> IORef HMap2 -> Application
respondMatrix _ _ req response = do
              str <- getRequestBodyChunk req
              ls <- runCmd "uname"
              let osName = if len ls > 0 then head ls else []
              bs <- readFileRepPat "postMatrix.html" "osname" $ toSBS osName
              pre bs
              -- https://hackage.haskell.org/package/bytestring-0.11.0.0/docs/Data-ByteString-Builder.html
              -- byteString :: ByteString -> Data.ByteString.Builder
              -- https://hackage.haskell.org/package/wai-3.2.2.1/docs/Network-Wai.html
              response $ responseLBS
                status200
                [("Content-Type", "text/html")]
                (toLBS bs)

            -- Nothing -> response $ responseLBS
            -- status400
            -- [("Content-Type", "text/plain; charset=utf-8")]
            -- "No file parameter found"

{-|              
data CommandService = CommandService{cmdServ::String, paramArg::String, inputData::String} deriving (Generic, Show)
instance DA.FromJSON CommandService
instance DA.ToJSON CommandService where
    toEncoding = DA.genericToEncoding DA.defaultOptions
-}
      
{-|
    === KEY: support services from outside browser
    @
    case 1:
       cmdStr = "alignment"
       cmdParam = "="

       a = b
        c  =d
       e = f
         ⇓
       a = b
       c = d
       e = f

    case 2: use space as delimter
       cmdStr = "alignment"
       cmdParam = ""

       a  b
        c  d
       e  f
         ⇓
       a b
       c d
       e f

     Emacs Copy Selected Region
          |
          ↓
     Write to $b/tmp/x.x
          |
          ↓
     Read $b/tmp/x.x
     Http Request => [Send Json]
     $sym/RequestJson  alignment
          |
          ↓
     commandService [Receive Json]
          |
          + → alignment ($b/tmp/x.x inside)
          |
          + → alignmentstr -p kk   (Use pipe instead of file)
          |
          + → commentCode ($b/tmp/x.x inside)
          |
          + → uncommentcode ($b/tmp/x.x inside)
          ↓
     runSh above cmd
          |
          ↓
     Capture STDOUT => Json => ReplyCode{...stdoutx=stdout}
          |
          ↓
     Response JSON ReplyCode{...stdoutx=stdout}

    bitbucket/tmp/x.x , NOT /tmp/x.x
    Store all the code to be processed: alignment, alignmentstr, commentCode, uncommentcode

    @



-}
commandService::Connection -> IORef HMap2 -> Application
commandService conn ref req response = do
  tmpfile <- getEnv "mytmp"
  let cmdAlignment     = "alignment"    
  let cmdAlignmentStr  = "alignmentstr" 
  let cmdCommentCode   = "commentcode"  
  let cmdUncommentCode = "uncommentcode"
  str <- getRequestBodyChunk req
  let may = (DA.decode . toLBS) str :: Maybe CommandService
  fw "may"
  pre may
  let cmdService = case may of 
        (Just x) -> x 
        _        -> CommandService{cmdServ = "", paramArg="", inputData="no data3344"} 
  fw "cmdService"
  pre cmdService
  let cmd      = cmdServ   cmdService
  let paramArgx = paramArg  cmdService
  logFileG [toStr paramArgx]

  -- Use "=" if paramArgx is empty 
  let delimiter = (len $ trim paramArgx) == 0 ? "=" $ paramArgx
  
  pp $ "paramArgx=" ++ paramArgx
  let strData  = inputData cmdService
  let upcodeblock = CodeBlockReply{ok = "False", retcmd = "", retdata = "", retbegt = 0, retendt = 0}
  pre cmdService

  if | cmd == cmdAlignment   -> do
       -- TODO: use tmpfile as argument
       -- let cmdStr = "Alignment.sh"::String
       -- (e2, so, si2) <- runSh $ toSText (if paramArgx == "" then cmdStr else (???)[cmdStr, paramArgx])
       -- if e2 == ExitSuccess then let rcode = ReplyCode{rcmd="", rerror = si2, stdoutx=so} 
                            -- in response $ responseJSON rcode
       -- else do
           -- let upcodeblock' = updateRetcmd ("runSh " + cmdStr) upcodeblock
           -- response $ responseJSON upcodeblock'

       (mayOut, exitCode) <- createProcessPipeData "Alignment.sh" ["-p", delimiter] (toStr strData)
       if exitCode == ExitSuccess
       then do
            stdoutData <- case mayOut of
                        Just hout -> TIO.hGetContents hout >>= return
                        Nothing   -> return ""
            logFileG ["stdoutx=" ++ (toStr stdoutData)]
            let rcode = ReplyCode{rcmd="", rerror = toSText (""::String), stdoutx=stdoutData} 
              in response $ responseJSON rcode
       else do
           logFileG ["ERROR: exitFailure:" ++ cmdAlignment]
           let upcodeblock' = updateRetcmd "Alignment.sh -p =" upcodeblock
           response $ responseJSON upcodeblock'



     | cmd == cmdAlignmentStr -> do
       logFileG ["cmd=" ++ cmd]
       -- TODO: use pipe as stdin
       -- alignmentStr -f /tmp/f.x
       -- alignmentStr -p kk  <-- pipe as stdin, kk is dummy var. Not Used for Now
       (mayOut, exitCode) <- createProcessPipeData "alignmentStr" ["-p", "kk"] (toStr strData)
       if exitCode == ExitSuccess
       then do
            stdoutData <- case mayOut of
                        Just hout -> TIO.hGetContents hout >>= return
                        Nothing   -> return ""
            logFileG ["stdoutx=" ++ (toStr stdoutData)]
            let rcode = ReplyCode{rcmd="", rerror = toSText (""::String), stdoutx=stdoutData} 
              in response $ responseJSON rcode
       else do
           logFileG ["ERROR WaiLib.hs: exitFailure:"++ cmdAlignmentStr]
           let upcodeblock' = updateRetcmd "alignmentStr -p kk" upcodeblock
           response $ responseJSON upcodeblock'

     | cmd == cmdCommentCode -> do
       -- SEE: $j/CommentCode.java for "-p"
       (mayOut, exitCode) <- createProcessPipeData "CommentCode.sh" ["-p", paramArgx] (toStr strData)
       if exitCode == ExitSuccess
       then do
            stdoutData <- case mayOut of
                        Just hout -> TIO.hGetContents hout >>= return
                        Nothing   -> return ""
            logFileG ["From WaiLib.hs => stdoutx=" ++ (toStr stdoutData)]
            let rcode = ReplyCode{rcmd="", rerror = toSText (""::String), stdoutx=stdoutData} 
              in response $ responseJSON rcode
       else do
           logFileG ["ERROR WaiLib.hs: exitFailure:" ++ cmdCommentCode]
           let upcodeblock' = updateRetcmd "CommentCode.sh -p =" upcodeblock
           response $ responseJSON upcodeblock'



       -- TODO: use tmpfile as argument
       -- let cmdStr = "CommentCode.sh"::String
       -- (e2, so, si2) <- runSh $ toSText (if paramArgx == "" then cmdStr else cmdStr + " " + paramArgx)
       -- if e2 == ExitSuccess then let rcode = ReplyCode{rcmd="", rerror = si2, stdoutx=so} 
                            -- in response $ responseJSON rcode
       -- else do
           -- let upcodeblock' = updateRetcmd ("runSh " + cmdStr) upcodeblock
           -- response $ responseJSON upcodeblock'

     | cmd == cmdUncommentCode -> do
       (mayOut, exitCode) <- createProcessPipeData "UncommentCode.sh" ["-p", paramArgx] (toStr strData)
       if exitCode == ExitSuccess
       then do
            stdoutData <- case mayOut of
                        Just hout -> TIO.hGetContents hout >>= return
                        Nothing   -> return ""
            logFileG ["stdoutx=" ++ (toStr stdoutData)]
            let rcode = ReplyCode{rcmd="", rerror = toSText (""::String), stdoutx=stdoutData} 
              in response $ responseJSON rcode
       else do
           logFileG ["ERROR: exitFailure:" ++ cmdUncommentCode]
           let upcodeblock' = updateRetcmd "UncommentCode.sh -p =" upcodeblock
           response $ responseJSON upcodeblock'


       -- TODO: use tmpfile as argument
       -- let cmdStr = "UncommentCode.sh"::String
       -- (e2, so, si2) <- runSh $ toSText (if paramArgx == "" then cmdStr else cmdStr + " " + paramArgx)
       -- if e2 == ExitSuccess then let rcode = ReplyCode{rcmd="", rerror = si2, stdoutx=so} 
                            -- in response $ responseJSON rcode
       -- else do
           -- let upcodeblock' = updateRetcmd ("runSh " + cmdStr) upcodeblock
           -- response $ responseJSON upcodeblock'


     | otherwise             -> do
       let errorSText = toSText $ "ERROR: Invalid cmd:" + cmd
       let rcode = ReplyCode{rcmd="", rerror = errorSText , stdoutx = errorSText} 
       response $ responseJSON rcode
  where
    (+) = (++)
    (∈) cx = concatStr cx " "
    concatSPC cx = concatStr cx " "
      
 
deleteCodeBlock::Connection -> IORef HMap2 -> Application
deleteCodeBlock conn ref req response = do
  str <- getRequestBodyChunk req
  let may = (DA.decode . toLBS) str :: Maybe UpdateCodeBlockX
  fw "may"
  pre may
  let codeJson = case may of 
        (Just x) -> x 
        _        -> UpdateCodeBlockX{pidx = 0, pidlistx = [], newcodex ="no code deleteCodeBlock", begtx = 0, endtx = 0} 
  fw "deleteCodeBlock"
  pre codeJson
  deleteDatabaseNewCodeTable conn (pidlistx codeJson) (toSText $ newcodex codeJson)
  -- if update ok, then send back "ok"

  cmd <- redisGetLastCmd keyLastCmd
  let begtClient = begtx codeJson
  let upcodeblock = CodeBlockReply{ok = "True", retcmd="delete", retdata = cmd, retbegt = begtClient, retendt = 0}
  newList <- readDatabaseCodeBlock conn 
  -- pre newList
  -- read the map out from ref
  -- conver all the keys to list of keyssnippetMap::[([String], [String])] -> IORef HMap -> IO ()
  -- rehash the map
  -- type HMap = M.HashMap String [[String]] 
  -- IORef HMap => ref
  updatePrefixMap newList ref
    
  --   hmap <- readIORef ref 
  --   let keys = M.keys hmap
  --   modifyIORef ref (mapClear2 keys)
  --   listToPrefixMap newList ref
  response $ responseJSON upcodeblock

  

{-|
   type HMap2 = M.HashMap String [([String], Integer)]
-}
updatePrefixMap::[([String], ([String], Integer, Integer, Integer))] -> IORef HMap2 -> IO()
updatePrefixMap ls ref = do
  hmap <- readIORef ref
  let keys = M.keys hmap
  -- delete all keys from ref => IORef HMap2
  logFileG ["  11 updatePrefixMap"]
  modifyIORef ref (mapClear2 keys)
  logFileG ["  12 updatePrefixMap"]
  listToPrefixMap ls ref
  logFileG ["  13 updatePrefixMap"]
  return ()

    
splitWhenTwo::(a -> Bool) -> [a] -> ([a], [a])
splitWhenTwo f cs = (takeWhile (not . f) cs, dropWhile (not . f) cs)
             
{-|
    === redis get, redis set, background color, color

    @
    Redis color/background DB
    HTMLPre.color:#333333
    HTMLPre.background:#AAAAAA
    @

    'getPreStyle'

    @
     [txtColor, bgColor]
     =>
     [
         ( "color"
         , "#8c9172"
         )
     ,
         ( "background-color"
         , "#3c6358"
         )
     ]
    @

-}
getPreStyle::IO [(String, String)]
getPreStyle = do
        let keyTxtColor = "color"
        txtColor <- redisGetPre keyTxtColor
        let keyBgColor = "background-color"
        bgColor <- redisGetPre keyBgColor
        return [txtColor, bgColor]
 where
   redisGetPre k = do
                   let key = pre ++ k
                   may <- redisGet key  -- key = "HTMLPre.color", "HTMLPre.background"
                   let color = case may of
                                 Just x -> let ls = splitWhen (== ':') x in (head ls, last ls)
                                 _      -> ("#333333", "#AAAAAA")
                   return color
              where
                pre = "HTMLPre."
        
{-|
   === Update background color from user

   * See modifycolor.css  submitUpdateBackground

   * Receive background color from client and decode the JSON string

  src/mystyle.css
  pre {
    display: block;
    font-family: monospace;
    font-size: 14pt;
    white-space: pre;
    margin-top: 1px;
    /* margin-right: 1px; */
    margin-bottom: 1px;
    /* margin-left: 4px; */
    background: #6b695869;  <-- change color
    border-style: outset;
    border-width: thin;
  }

  data R1 = R1{name::String}
  data R2 = R2{name::String}

  data RR1 = RR1 Maybe R1
  data RR2 = RR2 Maybe R2

  @
    pre{color:#90996c;
       background-color:#000000;
    }
  @

  'getPreStyle'

  @
   ------------------------------------styleList-----------------------------------
   [
       ( "color"
       , "#8c9172"
       )
   ,
       ( "background-color"
       , "#3c6358"
       )
   ]
  @
-} 
updateBackground::Application 
updateBackground req response = do
        str <- getRequestBodyChunk req
        let may = (DA.decode . toLBS) str :: Maybe Bgcolor
        fw "updateBackground may"
        pre may
        case may of 
          (Just x) -> if | hasPrefix "color:" str -> redisSet (prefix + "color") str  -- redisSet "HTMLPre.color" "#333"
                         | (hasPrefix "background-color:" str || hasPrefix "background:" str) -> redisSet (prefix + "background-color") str  -- redisSet "HTMLPre.background-color" "#444"
                         | otherwise -> error $ "Invalid CSS Style=[" + str + "]"
                   where
                     str = toStr $ colorname x
          _        -> return ()
        styleList <- getPreStyle
        fw "styleList"
        pre styleList
        let ls = formCSS styleList
        fw "ls"
        pre ls
        let bigstr = "pre{" + unlines ls + "}"
        -- let newcolor = [r| pre { background: |] <> colorname colorJson <> [r|;} |]
        TIO.writeFile cssfile (toSText bigstr)
        response $ responseCSS cssfile
        -- response $ responseNothingBS "updateBackground nothing"
   where
     prefix = "HTMLPre."
     formCSS ls = map(\x -> (fst x) + ":" + (snd x) + ";") ls
     cssfile = "src/css/modifycolor.css"
     (+) = (++)



{-|
	=== Update pre text color

	* Receive text color from client and decode the JSON string

        * See src/aronlib.js

        See 'updateBackground'

        NOT USED NOW
-}
updateTextColor::Application   
updateTextColor req response = do
        str <- getRequestBodyChunk req
        -- data Textcolor = Textcolor = {textcolor :: TS.Text } deriving (Generic, Show)
        let may = (DA.decode . toLBS) str :: Maybe Textcolor
        fw "Maybe Textcolor"
        pre may
        let colorJson = case may of 
                    (Just x) -> x 
                    _        -> Textcolor{textcolor = ""} 
        fw "Textcolor matJson"
        pre colorJson
        redisSet (prefix ++ "color") (toStr $ textcolor colorJson)
        fw "Textcolor response gbs"
        let newcolor = [r| pre { color: |] <> textcolor colorJson <> [r|;} |]
        fw "newcolor"
        pre newcolor
        style <- getPreStyle
        fw "style"
        pre style
        if checkCSSColorFormat (textcolor colorJson) then TIO.writeFile "src/css/modifycolor.css" newcolor else return ()
        response $ responseNothingBS "updateTextColor nothing"
   where
     prefix = "HTMLPre."
                                                                                                            
-- checkCSSColorFormat::TS.Text -> Bool
-- checkCSSColorFormat s = TS.length s <= 7 && TS.head s == '#' && (isHexStr (toStr s'))
--     where 
--         s' = TS.drop 1 s
              
receiveCode::Application
receiveCode req response = do 
        str  <- getRequestBodyChunk req
        let may = DA.decode $ toLBS str :: Maybe CompileCode 
        fw "may"
        print may
        let ccode = case may of 
                       (Just x) -> x 
                       _       -> CompileCode{compiler = "", option = "", code= ""} 
        fw "cool" 
        fw "cool" 
        let firstLine = head $ lines $ toStr (code ccode)
        let lang = last $ splitStr "[[:space:]]+" firstLine
        if | lang == "cpp" -> TIO.writeFile "/tmp/code.cpp" (code ccode)
           | lang == "haskell" -> TIO.writeFile "/tmp/code.hs" (code ccode)
           | otherwise -> return () 
        pp lang 
        let cmd = if | lang == "cpp" -> "g++ -o out /tmp/code.cpp && ./out" :: String
                     | lang == "haskell" -> "runh2 /tmp/code.hs && /tmp/code" :: String
                     | otherwise -> "" :: String
        (e2, so, si2) <- runSh $ toSText cmd
        if e2 /= ExitSuccess then let rcode = ReplyCode{rcmd="", rerror = si2, stdoutx=si2} 
                                  in response $ responseJSON rcode 
        else do
            pp so     
            let replyCode = ReplyCode{rcmd="", rerror="", stdoutx= so} 
            response $ responseJSON replyCode

-- NOT USED
receiveCode2::Application                                                                            
receiveCode2 req response = do                                                                       
        str <- getRequestBodyChunk req                                                                      
        let may = DA.decode $ toLBS str :: Maybe CompileCode                                
        fw "may"                                                                                    
        print may                                                                                   
        let ccode = case may of                                                                     
                       (Just x) -> x                                                                
                       _       -> CompileCode{compiler = "", option = "", code= ""}                 
        fw "cool"                                                                                   
        fw "cool"                                                                                   
        let firstLine = head $ lines $ toStr (code ccode)                                 
        let lang = last $ splitStr "[[:space:]]+" firstLine                                         
        if | lang == "cpp" -> TIO.writeFile "./cppcode.cpp" (code ccode)                            
           | lang == "haskell" -> TIO.writeFile "./code.hs" (code ccode)                         
           | otherwise -> return ()                                                                 
        pp lang                                                                                     
        let cmd = if | lang == "cpp" -> "g++ -o cppout ./cppcode.cpp && ./cppout"                         
                     | lang == "haskell" -> "runh2 ./code.hs && /tmp/code"                       
                     | otherwise -> ""                                                              
        sout <- A.run cmd                                              
        let rcode = ReplyCode{rcmd="", rerror = "", stdoutx= (toSText $ head sout)}          
        response $ responseJSON rcode




responseEditor:: Response
responseEditor = responseFile
    status200
    [("Content-Type", "text/html")]
    "compileCode.html"
    Nothing

{-|
   === response javacript file function

   * response aronlib.js to client
-} 
responseJavascript::FilePath -> Response
responseJavascript fname = responseFile
  status200
  [(hContentType, "text/javascript")]
  fname
  Nothing

responseCSS::FilePath -> Response
responseCSS fname = responseFile
  status200
  [(hContentType, "text/css")]
  fname
  Nothing

  
{-|
   === KEY: response PDF file, send pdf file to client

   @
   pdfSent::BS.ByteString -> Response
   @
-} 
responsePDF::FilePath -> Response
responsePDF fname = responseFile
  status200
  [(hContentType, "application/pdf"),
   (hContentDisposition, "inline;filename=" <> toSBS fname),
   (hCacheControl, "no-cache")
  ]
  fname
  Nothing
  
{-|
   === response png/PNG file 

   @
   pdfSent::BS.ByteString -> Response
   @
-} 
responsePNG::FilePath -> Response
responsePNG fname = responseFile
  status200
  [(hContentType, "image/png")
   -- (hContentDisposition, "inline;filename=" <> toSBS fname)
   -- (hCacheControl, "no-cache")
  ]
  fname
  Nothing

responseHtml::FilePath -> Response
responseHtml fname = responseFile
  status200
  [(hContentType, "text/html")]
  fname
  Nothing

--    case lookup "id" params of
--        Nothing -> undefined
--        Just x -> undefined

-- http://localhost:8000/up/
-- | NOTE: file => /upload dir
-- | Plz see uploadPage.html 
replyEditor :: Response
replyEditor = responseFile
    status200
    [("Content-Type", "text/html")]
    "indexEditor.html"
    Nothing

responseHelp :: Response
responseHelp = responseFile
    status200
    [("Content-Type", "text/html")]
    "help.html"
    Nothing

replyCssButton :: Response
replyCssButton = responseFile
    status200
    [("Content-Type", "text/html")]
    "cssButton.html"
    Nothing

wordcountReply :: Response
wordcountReply = responseFile
    status200
    [("Content-Type", "text/html")]
    "wordcountReply.html"
    Nothing

matrixReply :: Response
matrixReply = responseFile
    status200
    [("Content-Type", "text/html")]
    "postMatrix.html"
    Nothing

sendHTMLTableCmd :: Response
sendHTMLTableCmd = responseFile
    status200
    [("Content-Type", "text/html")]
    "sendHTMLTableCmd.html"
    Nothing

replyJS :: Response
replyJS = responseFile
    status200
    [("Content-Type", "text/javascript")]
    "ace/build/src/ace.js"
    Nothing


{-| 
    === Insert name and age to MySqlite-simple file-based database.

    http://localhost:8000/insert/

    File: insert.html
    <form action="/insert" method="POST" enctype="multipart/form-data">
      Name <input type="text" name="name"><br>
      Age <input type="text" name="age"><br>
      <input type="submit" value="submit">
    </form> 

    >insert data to table: people
    >"INSERT INTO people (name, age) VALUES (?,?)" 
-} 
insertDatabase::Connection -> Application
insertDatabase conn req response = do
    (params, files) <- parseRequestBody lbsBackEnd req
    case requestMethod req of
        "POST" -> do 
              let name = case lookup "name" params of 
                                Just name -> name 
                                _         -> "name nothing"
              let age = case lookup "age" params of 
                                Just age  -> age 
                                _         -> "age nothing" 


              execute_ conn "CREATE TABLE IF NOT EXISTS people (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, age TEXT)"
              execute conn "INSERT INTO people (name, age) VALUES (?,?)" (Person 0 (toSText name) (toSText age))
              people <- query_ conn "SELECT id, name, age from people" :: IO [Person]
              pre people
              hostURL <- getHostName
              pre hostURL
              response =<< let Just uri = parseURI (hostURL ++ "/insertinfo/") in redirect' status302 [] uri 
              -- response $ responseNothing $ b2s $ BS.concat [name, age]
        _      -> response $ responseNothing "post nothing from insertDatabase"
      
{-| 
    @
    -- defind in AronModule.hs
    data CodeBlock = 
    CodeBlock 
    { codeblockId  :: Int64
    , header       :: TS.Text
    , codeblock    :: TS.Text
    } deriving (Eq, Read, Show)


    Input:
    a:*.hs:c,d
    line 1
    line 2

    mycode <- query_ conn "SELECT id, header, codeblock from CodeBlock" :: IO [CodeBlock]

    id        = 106
    header    = a:*:c
    codeblock = a:*.hs:c,d \n line 1 \n line 2

    tupleList = [(["a", "c,d"], ["a:*.hs:c", "line 1", "line 2"],    106)
    pplist    = [(["a", "c", "d"], ["a:*.hs:c", "line 1", "line 2"], 106)

    DATE: Thursday, 29 June 2023 00:11 PDT
    Update CodeBlock table with new column: show INTEGER DEFAULT 1
    SET show = 0 to hide the row
    @
-} 
readDatabaseCodeBlock::Connection -> IO [([String], ([String], Integer, Integer, Integer))]
readDatabaseCodeBlock conn = do
                      -- read CodeBlock table => [CodeBlock]::[Text]
                      mycode <- query_ conn "SELECT id, header, codeblock, addedtime, score from CodeBlock WHERE show = 1" :: IO [CodeBlock]
                      -- mycode = [CodeBlock]::[Text]
                      fw "mycode beg"
                      -- pre mycode
                      fw "mycode end"
                      -- only codeblocks
                      -- let list = map (\x -> ( codeBlockId x, (toStr.header) x, (toStr.codeblock) x) ) mycode 
                      let list = map (\x -> let cb = (toStr . codeblock) x in ( codeBlockId x, head $ plines cb, cb, addedtime x, score x) ) mycode 
                      --  list = [(id, header, codebock)]
                      fw "list"
                      -- pre list
                      -- let ll = filter(\x -> length x > 0) $ splitWhen(\x -> (length $ trim x) == 0) list
                      --
                      -- NOTE: header field in CodeBlock is NOT USED
                      let ll = map (\(id, _, c, addedtime, score) -> (id, head $ plines c, plines c, addedtime, score)) list
                      
                      
                      --   ll = [(id, header, [[line 1], [line 2]])]
                      --   let plist = map(\x -> ((splitStrChar "[,:]" $ head x), x) ) ll
                          
                      --   1. take the first line of codeblock
                      --   2. splitStrChar "[:]" =>  mycode : *.hs : Just Bieber Constable, the governer of royal castle
                      --                         =>  [mycode] [*.hs] [Just Bieber Constable, the governer of royal castle]
                      --      remove *.hs 
                      --   3. removeIndex 1 => [mycode] [Just Bieber Constable, the governer of royal castle]
                      --   4. tupleList = [[mycode] [Just Bieber Constable, the governer of royal castle]]
                      
                      let tupleList = map(\(id, header, cs, addedtime, score) -> ((removeIndex 1 $ splitStrChar "[:]" header), cs, id, addedtime, score) ) ll
                      fw "tupleList"
                      -- pre tupleList
                      let pplist = map(\(header, cs, id, addedtime, score) -> (
                                                      uniqueOrder $ foldr(++) [] $ map (splitStrCharTrim "[,]") header, 
                                                      (cs, toInteger id, toInteger addedtime, toInteger score))
                                             ) tupleList
                      fw "pplist"
                      -- pre pplist
                      -- pre $ typeOf pplist    
                      return pplist
                      
--              fl
--              pre mycode
--              fl
--              let codels = map (\x -> let h = head $ lines $ strictTextToStr x 
--                                      in (removeIndex 1 $ splitStrChar "[:]" h, lines $ strictTextToStr x)) $ map (\x -> codeblock x) mycode 
--              return  codels
--    where 
--        b2s = strictTextToStr . strictByteStringToStrictText
--        toSText = strictByteStringToStrictText

readSnippet2::FilePath->IO [([String], [String])]
readSnippet2 path = do 
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


queryCreateTable::DQ.Query
queryCreateTable = query
  where
    q = "CREATE TABLE IF NOT EXISTS CodeBlock (id INTEGER PRIMARY KEY AUTOINCREMENT, header TEXT, codeblock TEXT, addedtime DATETIME DEFAULT (strftime('%s', 'now')), score INTEGER DEFAULT 0)"
    query = DQ.Query{fromQuery = s2Text q}

            
{-| 
    === Create CodeBlock table

    NOT USED
-} 
createCodeBlockTable::Connection -> IO() 
createCodeBlockTable conn = do
              execute_ conn queryCreateTable
              return ()

{-|
  NOT USED
-}
addCodeBlockTable::Connection -> TS.Text -> TS.Text -> IO() 
addCodeBlockTable conn header text = do
              let header' = trimT header
              let text' = trimT text
              execute_ conn queryCreateTable
              execute conn Query{fromQuery = s2Text "INSERT INTO CodeBlock (header, codeblock) VALUES (?,?)"} (CodeBlock 0 header' text' 0 0)
              return ()

{-|
  NOT USED
-}
updateDatabaseCodeBlockTable::Connection -> TS.Text -> TS.Text -> IO() 
updateDatabaseCodeBlockTable conn oldHeader text = do
              let oldHeader' = trimT oldHeader 
              execute conn Query{fromQuery = s2Text "DELETE FROM CodeBlock WHERE header = ? "} (Only oldHeader') 
              codeblock <- query_ conn Query{fromQuery = s2Text "SELECT id, header, codeblock from CodeBlock"} :: IO [CodeBlock]
              -- pre codeblock 

              let newHeader = let ln = plines $ toStr text in toSText $ head ln
              let newHeader' = trimT newHeader
              let text' = trimT text
              execute_ conn queryCreateTable
              execute conn Query{fromQuery = s2Text "INSERT INTO CodeBlock (header, codeblock) VALUES (?,?)"} (CodeBlock 0 newHeader' text 0 0)
              rowId <- lastInsertRowId conn
              let myhead = "hi"
              -- execute conn "DELETE FROM CodeBlock WHERE id = ? " (Only rowId) 
              -- TODO:
              -- oldHeader need to be cleanup a bit to compare the origin header
              fw "oldHeader beg"
              pre oldHeader
              fw "oldHeader end"
              return ()
  
{-|
   === Update database table

   @
   data CodeBlock =
       CodeBlock
       { codeBlockId :: Int64
       , header      :: TS.Text
       , codeblock   :: TS.Text
       , addedtime   :: Int64
       , score       :: Int64
       } deriving (Eq, Read, Show)
   @
-}              
updateDatabaseNewCodeTable::Connection -> Integer -> TS.Text -> IO()
updateDatabaseNewCodeTable conn pid ucode = do
  let mycode = "hi"::TS.Text
  let pidInt = fromIntegral pid
  let header = head $ linesSText ucode
  executeNamed conn Query{fromQuery = s2Text "UPDATE CodeBlock SET header = :header , codeblock = :codeblock, addedtime = strftime('%s', 'now') WHERE id = :id "} [":header" := header, ":codeblock" := ucode, ":id" := (pidInt::Int64)]
  -- executeNamed conn Query{fromQuery = s2Text "UPDATE CodeBlock SET header = :header , codeblock = :codeblock, addedtime = strftime('%s', 'now'),  score = :score WHERE id = :id "} [":header" := header, ":codeblock" := ucode, ":score" := score, ":id" := (pidInt::Int64)]

updateDatabaseScoreTable::Connection -> Integer -> String -> IO()
updateDatabaseScoreTable conn pid upordown  = do
  let pidInt = fromIntegral pid
  let addedtime = 999::Int64
  let score     = 33::Int64
  case upordown of
    "upscore"   -> executeNamed conn Query{fromQuery = s2Text "UPDATE CodeBlock SET score = score + 1 WHERE id = :id"} [":id" := (pidInt::Int64)]
    "downscore" -> executeNamed conn Query{fromQuery = s2Text "UPDATE CodeBlock SET score = score - 1 WHERE id = :id"} [":id" := (pidInt::Int64)]
    _           -> error "Unknow command from updateDatabaseScoreTable"
  
updateDatabaseShowTable::Connection -> Integer -> IO()
updateDatabaseShowTable conn pid = do
  let pidInt = fromIntegral pid
  executeNamed conn Query{fromQuery = s2Text "UPDATE CodeBlock SET show = 0 WHERE id = :id"} [":id" := (pidInt::Int64)]

  
{-|
   ==== Two Commands only
   "upscore"    => score += 1
   "downscore"  => score -= 1
-}
updateScoreCodeBlock::Connection -> IORef HMap2 -> String -> Application
updateScoreCodeBlock conn ref cmd req response = do
  str <- getRequestBodyChunk req
  let may = (DA.decode . toLBS) str :: Maybe UpdateCodeBlock
  fw "may"
  pre may
  let codeJson = case may of 
        (Just x) -> x 
        _        -> UpdateCodeBlock{pid = 0, newcode="no code", begt=0, endt=0} 
  fw "updateScoreCodeBlock WaiLib.hs"
  pre codeJson
  updateDatabaseScoreTable conn (pid codeJson) cmd
  -- if update ok, then send back "ok"
  let begtClient = begt codeJson

  -- KEY: Last command, top command
  cmd <- redisGetLastCmd keyLastCmd
  let upcodeblock = CodeBlockReply{ok = "True", retcmd = "updateScore", retdata = cmd, retbegt = begtClient, retendt = 0}
  result <- newEmptyMVar

  logFileG ["1 updateScoreCodeBlock"]
  newList <- readDatabaseCodeBlock conn

  logFileG ["2 updateScoreCodeBlock"]
  updatePrefixMap newList ref
  putMVar result 100        
  
  putStrLn "Waiting"
  value <- takeMVar result
  
  logFileG ["3 updateScoreCodeBlock"]
  response $ responseJSON upcodeblock

addScoreCodeBlock:: Connection -- ^ Connection
  -> IORef HMap2 -- ^ type HMap2 = M.HashMap String [([String], Integer, Integer, Integer)]
  -> Application
addScoreCodeBlock conn ref = updateScoreCodeBlock conn ref "upscore"
  
subtractScoreCodeBlock:: Connection -- ^ Connection
  -> IORef HMap2 -- ^ type HMap2 = M.HashMap String [([String], Integer, Integer, Integer)]
  -> Application
subtractScoreCodeBlock conn ref = updateScoreCodeBlock conn ref "downscore"
  
{-|
   KEY: insert code to database
-- data CodeBlock = 
--    CodeBlock 
--    { codeblockId        :: Int64
--    , header    :: TS.Text
--    , codeblock :: TS.Text
--    } deriving (Eq, Read, Show)
-} 
insertDatabaseNewCodeTable::Connection -> Integer -> TS.Text -> IO()
insertDatabaseNewCodeTable conn pid ucode = do
  let pidInt = fromIntegral pid -- pidInt is not used here
  let header' = toSText $ head $ plines $ toStr ucode
  execute conn Query{fromQuery = s2Text "INSERT INTO CodeBlock (header, codeblock) VALUES(?, ?)"} ((header', ucode)::(TS.Text, TS.Text))

{-|
  KEY: duplicated row and set show=0
-}
duplicatedRowNoShow::Connection -> Integer-> IO()
duplicatedRowNoShow conn pid = do
  let pidInt = fromIntegral pid
  execute conn Query{fromQuery = s2Text "INSERT INTO CodeBlock (header, codeblock) SELECT header, codeblock from CodeBlock WHERE id = ? "} (Only (pidInt::Int64))
  execute_ conn Query{fromQuery = s2Text "UPDATE CodeBlock SET show = 0 WHERE id = (select MAX(id) from CodeBlock)"}
  

{-|
 @
 Fake deletion, SET show = 0 in database table 
 NOTE: ucode is NOT USED
 @
-}
deleteDatabaseNewCodeTable::Connection -> [Integer] -> TS.Text -> IO()
deleteDatabaseNewCodeTable conn pidlist ucode = do
  -- let pidInt = fromIntegral pid
  let x1 = fromIntegral 868 
  let ps = concatStr (map show pidlist) ","
  let px = "(" <> ps <> ")"
  -- executeNamed conn Query{fromQuery = s2Text "UPDATE CodeBlock SET show = 0 WHERE id = :id "} [":id" := (pidInt::Int64)]
  -- executeNamed conn Query{fromQuery = s2Text "UPDATE CodeBlock SET show = 0 WHERE id IN (:id, :x1)"} [":id" := (pidInt::Int64), ":x1" := (x1 :: Int64)]
  executeNamed conn Query{fromQuery = s2Text $ "UPDATE CodeBlock SET show = 0 WHERE id IN " <> px} []
  
deleteDatabaseCodeBlockTable::Connection -> TS.Text -> IO() 
deleteDatabaseCodeBlockTable conn header = do
  let header' = trimT header
  execute conn Query{fromQuery = s2Text "DELETE FROM CodeBlock WHERE header = ? "} (Only header') 
  codeblock <- query_ conn Query{fromQuery = s2Text "SELECT id, header, codeblock from CodeBlock"} :: IO [CodeBlock]
  pre codeblock 
  return ()

loginCheck::Connection -> Application
loginCheck conn req response = do
    (params, files) <- parseRequestBody lbsBackEnd req
    case requestMethod req of
        "POST" -> do 
              let email_ = case lookup "email" params of 
                                Just email -> toSText email 
                                _          -> "email nothing" 

              let password_ = case lookup "password" params of 
                                -- Just password -> BS.takeWhile (not.DW.isSpace) $ BS.dropWhile (DW.isSpace) password 
                                Just password -> toSText password 
                                _             -> "password nothing" 
              print email_
              print password_ 
              -- row <- queryNamed conn "SELECT * FROM user WHERE uid = :uid" [":uid" := uid] :: IO [User]
              row <- queryNamed conn Query{fromQuery = s2Text "SELECT * FROM user WHERE email = :email AND password = :password"} [":password" := password_, ":email" := email_] :: IO [User]
              print row
              response $ responseNothing "row nothing"
        _      -> response $ responseNothing "user nothing"


{-| 
    validate user input and santize me
-} 
securityValidate:: BS.ByteString -> 
                   BS.ByteString -> 
                   BS.ByteString -> 
                   BS.ByteString -> 
                   BS.ByteString -> 
                   Bool
securityValidate name email password task money = nameBool && passwordBool && emailBool
        where
            nameT = toSText name
            passwordT = toSText password
            nameBool = if (TS.length nameT) > 0 && (TS.length nameT) < 40 && TS.all (isAlphaNum) nameT then True else False
            passwordBool = if TS.all (isAlphaNum) passwordT then True else False
            emailBool = EM.isValid email

insertUserDB::Connection -> Application
insertUserDB conn req response = do
    (params, files) <- parseRequestBody lbsBackEnd req
    case requestMethod req of
        "POST" -> do 
              let name_ = case lookup "name" params of    -- ByteString
                          Just name -> name 
                          _         -> "name nothing"
              let email_ = case lookup "email" params of 
                           Just email -> email 
                           _          -> "email nothing" 

              let password_ = case lookup "password" params of 
                              Just password -> password 
                              _             -> "password nothing" 

              let task_ = case lookup "task" params of 
                          Just task -> task 
                          _         -> "task nothing" 

              let money_ = case lookup "money" params of 
                           Just money -> money 
                           _         -> "money nothing" 

              -- validate user input
              -- formatValidate::User -> Bool
              execute_ conn Query{fromQuery = s2Text "CREATE TABLE IF NOT EXISTS user (uid INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, email TEXT, password TEXT, task TEXT, money INTEGER)"}
              row <- queryNamed conn Query{fromQuery = s2Text "SELECT * FROM user WHERE email = :email"} [":email" := (toSText email_)] :: IO [User]
              if len row == 0 then do
                  execute conn Query{fromQuery = s2Text "INSERT INTO user (name, email, password, task, money) VALUES (?,?,?,?,?)"} (User 0 (toSText name_) (toSText email_) (toSText password_) (toSText task_) (b2Integer money_))
                  changeRow <- changes conn
                  print $ "changeRow=" ++ (show changeRow)
                  if changeRow == 1 then do
                      userList <- query_ conn Query{fromQuery = s2Text "SELECT uid, name, email, password, task, money FROM user"} :: IO [User]
                      mapM_ print userList
                      -- let listTask = TS.concat $ map (\x -> [r|<div>|] <> (t2b $ task x) <> [r|</div><br>|]) userList -- => TS.Text
                      -- response $ responseTaskBS (task (head userList))
                      hostURL <- getHostName >>= return . toSBS
                      response $ responseTaskBS $ replyTaskHtml hostURL task_ 
                  else do
                      response $ responseTaskBS "Insect task field" 
                      -- response =<< let Just uri = parseURI "http://localhost:8000/insertUser/" in redirect' status302 [] uri 
              else do
                  response $ responseNothing "email exists"
        _      -> response $ responseNothing "no POST"

    where 
        b2Integer = fi . b2i
        b2i = stringToInt . strictTextToStr . toSText
        

-- | -------------------------------------------------------------------------------- 
-- | Wed Dec  5 15:06:00 2018 
--   Sat Jun  8 23:42:18 2019 
-- | upload with following POST to upload file to server
-- | -------------------------------------------------------------------------------- 
-- <form action="/upload" method="POST" enctype="multipart/form-data">
--  Upload File: <input type="file" name="file"><br>
--  <input type="submit" value="submit">
-- </form> 
-- | -------------------------------------------------------------------------------- 
--   http://localhost:8000/up/
-- | File is uploaded to => haskell_web/uploaddir 
upload::String -> Application
upload updir req response = do
    -- Parse the request body. We'll ignore parameters and just look
    -- at the files
    (params, files) <- parseRequestBody lbsBackEnd req
--    case lookup "id" params of
--        Nothing -> undefined
--        Just x -> undefined

    -- Look for the file parameter called "file"
    case lookup "file" files of
        -- Not found, so return a 400 response
        Nothing -> response $ responseLBS
            status400
            [("Content-Type", "text/plain; charset=utf-8")]
            "No file parameter found"
        -- Got the file 
        -- take the file name
        -- grab the content
        -- write the file to filesystem
        Just file -> do
            let
                -- Determine the name of the file to write out
                name = takeFileName $ toStr $ fileName file
                -- and grab the content
                content = fileContent file
            -- Write it out
            writeToFile "/tmp/f1.x" [name]
            LA.writeFile (updir ++ name) content
            response $ replyCssButton 

searchMap:: Application
searchMap req response = do
    -- Parse the request body. We'll ignore parameters and just look
    -- at the files
    (_params, files) <- parseRequestBody lbsBackEnd req

    -- Look for the file parameter called "file"
    case lookup "post" _params of
        -- Not found, so return a 400 response
        Nothing -> response $ responseLBS
            status400
            [("Content-Type", "text/plain; charset=utf-8")]
            "No post"
        -- Got it!
        Just "post" -> response $ responseLBS
            status200
            [("Content-Type", "text/text")]
            "Just post" 


{-| 
    === user input autocomplete

    * search field, search input, search form

    <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/color change background color Javascript>

    * search form has moved to htmlBody.htmlbs

    submitUpdateBackground => aronlib.js

    NOTE: NOT USE IT NOW ❌ 
    gf: js/aronlib.js
-} 
searchForm::String -> String
searchForm s = [r| 
             <div style="text-align:center;">
             <form action="/snippet" method="get" target=""> 
             <input type="text" style="font-size:18pt;height:50px;width:400px;" id="inputid" value="s van" oninput="searchChange(this);" name="id" list="autocomplete">  
                 <datalist id="autocomplete">" |] <> s <> [r| </datalist><br>  
             </form> 
             </div> 
                <div>
                
                <input id='bgidcolor' type="color" onchange="submitUpdateBackground('background-color:' + this.value)" name="colorname"
                value="#e66465">
                <label for="body_background">Background</label>
                
                <input id='textcolorid' type="color" onchange="submitUpdateTextColor('color:' + this.value)" name="textcolorname"
                value="#e66465">
                <label for="text_color">TextColor</label>
                <svg width="20" height="20">
                  <rect width="20" height="20" style="fill:rgb(10,10,255);stroke-width:3;stroke:rgb(0,0,0)" />
                </svg>
                
                <svg width="20" height="20">
                  <rect width="20" height="20" style="fill:rgb(0,33,55);stroke-width:3;stroke:rgb(0,9,20)" />
                </svg>
                </div>
             |]

