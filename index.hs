{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Control.Exception (SomeException(SomeException), catch)
import Data.Char (isDigit, isLetter)
--TODO:例外処理
--Array

main :: IO ()
main=do
  let tokens=toTokenList "{\"number\":42,\"minus\":-12,\"shousu\":3.14}"
  print tokens
arrayTest :: IO ()
arrayTest=do
  let tokens=toTokenList "{\"array\":[\"one\",{},65536,[1,2,3],\"three.js\",true,false,1.414]}"
  let array=jsonArray "array" tokens
  print array
  print $ arrayjsonString 0 array
  print $ arrayjsonInt 2 array
  print $ arrayjsonBoolean 4 array
  print $ arrayjsonBoolean 5 array
  print $ arrayjsonDouble 7 array

test :: IO ()
test=do
  let tokens=toTokenList "{\"boolT\":true,\"taxi\":1729,\"boolF\":false,\"string\":\"oh waaa\",\"number\":42,\"minus\":-12,\"shousu\":3.14}"
  print $ jsonBoolean "boolT" tokens
  print $ jsonInt "taxi" tokens
  print $ jsonBoolean "BoolF" tokens
  print $ jsonString "string" tokens
  print $ jsonInt "number" tokens
  print $ jsonInt "minus" tokens
  print $ jsonDouble "shousu" tokens


  putStrLn ""

  let nest=toTokenList "{\"level\":  3 ,  \"next\" :  {\"levelStr\"  :  \"2\" , \"nest\":{\"arr\":[\"str\",6,[{}]]}}}"
  print $ jsonInt "level" nest
  let lv2=jsonObject "next" nest
  print $ tokenToString lv2
  print $ jsonString "levelStr" lv2
  let lv3=jsonObject "nest" lv2
  print $ tokenToString lv3
  let arr=jsonArray "arr" lv3
  print $ tokenToString arr
  print $ arrayjsonString 0 arr
  print $ arrayjsonInt 1 arr




tokenToString :: [(a, [b])] -> [b]
tokenToString=concatMap snd

anyChar :: String -> (Char, String)
anyChar (x:xs)=(x,xs)

--合致すれば返す
satisfy :: (a -> Bool) -> [a] -> (a, [a])
satisfy f (x:xs)
  |f x=(x,xs)

--合致している間返す
many :: (Char -> Bool) -> String -> String
many f (x:xs)
  |f x=if null xs then [x] else x:many f xs
  |otherwise=""

--初めて合致した物以降を返す(合致したものは含まない)
back :: (Char -> Bool) -> String -> String
back f (x:xs)
  |f x=xs
  |null xs=""
  |otherwise=back f xs

--初めて合致した物以降を返す(合致したものを含む)
back' :: (Char -> Bool) -> String -> String
back' f (x:xs)
  |f x=x:xs
  |null xs=""
  |otherwise=back' f xs

backToken :: String -> [(String, String)] -> [(String, String)]
backToken token (x:xs)
  |token==fst x=xs
  |null xs=[]
  |otherwise=backToken token xs

--初めて余った閉じカッコ以降を返す
tailObjectClose :: [(String, String)] -> [(String, String)]
tailObjectClose (token:tokens)
  |null tokens=[]
  |fst token=="OBJECT_OPEN"=tailObjectClose $ tailObjectClose tokens
  |fst token=="OBJECT_CLOSE"=tokens
  |otherwise=tailObjectClose tokens

tailArrayClose :: [(String, String)] -> [(String, String)]
tailArrayClose (token:tokens)
  |null tokens=[]
  |fst token=="ARRAY_OPEN"=tailArrayClose $ tailArrayClose tokens
  |fst token=="ARRAY_CLOSE"=tokens
  |otherwise=tailArrayClose tokens

--オブジェクトを一つ返す
getAnObject :: [(String,String)] -> [(String,String)]
getAnObject (token:tokens)
  |null tokens=[token]
  |fst token=="OBJECT_CLOSE"=[token]
  |otherwise=do
    let obj=getAnObject tokens
    let as=getAnObject (drop (length obj) tokens)
    token:if fst token=="OBJECT_OPEN" then obj++as else getAnObject tokens

getAnArray :: [(String,String)] -> [(String,String)]
getAnArray (token:tokens)
  |null tokens=[token]
  |fst token=="ARRAY_CLOSE"=[token]
  |otherwise=do
    let arr=getAnArray tokens
    let as=getAnArray (drop (length arr) tokens)
    token:if fst token=="ARRAY_OPEN" then arr++as else getAnArray tokens


--含まれていれば返す
isOneOf :: String-> String -> (Char, String)
isOneOf s (x:xs)
 |x `elem` s =(x,xs)

isNumber :: Char -> Bool
isNumber x=isDigit x || x=='-' ||x=='.'

toTokenList :: String -> [(String, String)]
toTokenList json=fst (objectToTokenList json [])

jsonString :: String -> [(String, String)] -> String
jsonString key (token:tokens)
  |fst token=="OBJECT_OPEN" =getObjectString key tokens

jsonInt :: String -> [(String, String)] -> Int
jsonInt key (token:tokens)
  |fst token=="OBJECT_OPEN" = read $ getObjectNumber key tokens::Int

jsonDouble :: String -> [(String, String)] -> Double
jsonDouble key (token:tokens)
  |fst token=="OBJECT_OPEN" = read $ getObjectNumber key tokens::Double

jsonBoolean :: String -> [(String, String)] -> Bool
jsonBoolean key (token:tokens)
  |fst token=="OBJECT_OPEN" = getObjectBoolean key tokens

jsonObject :: String -> [(String, String)] -> [(String, String)]
jsonObject key (token:tokens)
  |fst token=="OBJECT_OPEN"=getObject key tokens

jsonArray :: String -> [(String, String)] -> [(String, String)]
jsonArray key (token:tokens)
  |fst token=="OBJECT_OPEN"=getObjectArray key tokens

arrayjsonString :: Int -> [(String, String)] -> String
arrayjsonString index (token:tokens)
  |fst token=="ARRAY_OPEN"=getArrayString index tokens

arrayjsonInt :: Int -> [(String, String)] -> Int
arrayjsonInt index (token:tokens)
  |fst token=="ARRAY_OPEN"=read $ getArrayNumber index tokens

arrayjsonDouble :: Int -> [(String, String)] -> Double
arrayjsonDouble index (token:tokens)
  |fst token=="ARRAY_OPEN"=read $ getArrayNumber index tokens

arrayjsonBoolean :: Int -> [(String, String)] -> Bool
arrayjsonBoolean index (token:tokens)
  |fst token=="ARRAY_OPEN"=getArrayBoolean index tokens

getObjectString :: String -> [(String, String)] -> String
getObjectString key (token:token':tokens)
  |null tokens="UNSOLVED"
  |otherwise=case fst token of
    "KEY"
      |snd token == key->do
        let (valueType,value)=head tokens
        if fst token' == "COLON" && valueType == "STRING" then value else "UNSOLVED"
      |otherwise->getObjectString key (token':tokens)
    "OBJECT_OPEN"->getObjectString key (tailObjectClose tokens)
    "ARRAY_OPEN"->getObjectString key (tailArrayClose tokens)
    _->getObjectString key (token':tokens)

getObjectNumber :: String -> [(String, String)] -> String
getObjectNumber key (token:token':tokens)
  |null tokens= "wawawa"
  |otherwise=case fst token of
    "KEY"
      |snd token == key ->do
        let (valueType,value)=head tokens
        if fst token' == "COLON" && valueType == "NUMBER" then value else "wowowo"
      |otherwise->getObjectNumber key (token':tokens)
    "OBJECT_OPEN"->getObjectNumber key (tailObjectClose tokens)
    "ARRAY_OPEN"->getObjectNumber key (tailArrayClose tokens)
    _->getObjectNumber key (token':tokens)

getObjectBoolean :: String -> [(String, String)] -> Bool
getObjectBoolean key (token:token':tokens)
  |null tokens=False
  |otherwise=case fst token of
    "KEY"
      |snd token == key ->do
        let (valueType,value)=head tokens
        if fst token' == "COLON" && valueType == "BOOLEAN" then value=="true" else False
      |otherwise->getObjectBoolean key (token':tokens)
    "OBJECT_OPEN"->getObjectBoolean key (tailObjectClose tokens)
    "ARRAY_OPEN"->getObjectBoolean key (tailArrayClose tokens)
    _->getObjectBoolean key (token':tokens)

getObject :: String -> [(String, String)] -> [(String, String)]
getObject key (token:tokens)
  |null tokens=[("","")]
  |otherwise=case fst token of
    "KEY"
      |snd token == key ->
        if fst (head tokens) == "COLON" && fst (tokens!!1) == "OBJECT_OPEN" then (tokens!!1):getAnObject (drop 2 tokens) else [("","")]
      |otherwise->getObject key tokens
    "OBJECT_OPEN"->getObject key $ tailObjectClose tokens
    "ARRAY_OPEN"->getObject key $ tailArrayClose tokens
    _->getObject key tokens

getObjectArray :: String -> [(String, String)] -> [(String, String)]
getObjectArray key (token:tokens)
  |null tokens=[("","")]
  |otherwise=case fst token of
    "KEY"
      |snd token == key->
        if fst (head tokens) == "COLON" && fst (tokens!!1) == "ARRAY_OPEN" then (tokens!!1):getAnArray (drop 2 tokens) else [("","")]
      |otherwise->getObjectArray key tokens
    "OBJECT_OPEN"->getObjectArray key $ tailObjectClose tokens
    "ARRAY_OPEN"->getObjectArray key $ tailArrayClose tokens
    _->getObjectArray key tokens


getArrayString :: Int -> [(String, String)] -> String
getArrayString index (token:tokens)
  |null tokens="UNSOLVED"
  |index<0="MINUS"
  |index==0=snd token
  |fst token=="OBJECT_OPEN"=getArrayString (index-1) $ tail $ tailObjectClose tokens
  |fst token=="ARRAY_OPEN"=getArrayString (index-1) $ tail $ tailArrayClose tokens
  |fst (head tokens)=="COMMA"=getArrayString (index-1) $ tail tokens

getArrayNumber ::Int -> [(String, String)] -> String
getArrayNumber index (token:tokens)
  |index<0= "minus"
  |index==0=snd token
  |null tokens="null"
  |fst token=="OBJECT_OPEN"=getArrayNumber (index-1) $ tail $ tailObjectClose tokens
  |fst token=="ARRAY_OPEN"=getArrayNumber (index-1) $ tail $ tailArrayClose tokens
  |fst (head tokens)=="COMMA"=getArrayNumber (index-1) $ tail tokens


getArrayBoolean :: Int -> [(String, String)] -> Bool
getArrayBoolean index (token:tokens)
  |null tokens=False
  |index<0=False
  |index==0=snd token=="true"
  |fst token=="OBJECT_OPEN"=getArrayBoolean (index-1) $ tail $ tailObjectClose tokens
  |fst token=="ARRAY_OPEN"=getArrayBoolean (index-1) $ tail $ tailArrayClose tokens
  |fst (head tokens)=="COMMA"=getArrayBoolean (index-1) $ tail tokens

objectToTokenList :: String -> [(String,String)] -> ([(String,String)],String)
objectToTokenList (x:xs) tokens
  |not (null tokens) && length (filter (\s->fst s == "OBJECT_OPEN") tokens )==length (filter (\s->fst s == "OBJECT_CLOSE") tokens)=(tokens,x:xs)
  |null xs=(tokens++[if x=='}' then ("OBJECT_CLOSE","}") else ("UNSOLVED",[x])],"")
  |x==' '||x=='\n'=objectToTokenList xs tokens
  |x=='['=do
    let (tkn,str)=arrayToTokenList (x:xs) []
    if null str then (tokens++tkn,"")
    else objectToTokenList str (tokens++tkn)
  |x=='{'=objectToTokenList xs (tokens++[("OBJECT_OPEN","{")])
  |x=='}'=objectToTokenList xs (tokens++[("OBJECT_CLOSE","}")])
  |x==':'=objectToTokenList xs (tokens++[("COLON",":")])
  |x==','=objectToTokenList xs (tokens++[("COMMA",",")])
  |x=='"'= if fst (last tokens)=="OBJECT_OPEN"||fst (last tokens)=="COMMA"
    then objectToTokenList (back (=='"') xs) (tokens++[("KEY",many (/='"') xs)])
    else if fst (last tokens)=="COLON" then objectToTokenList (back (=='"') xs) (tokens++[("STRING",many (/='"') xs)])
    else objectToTokenList xs (tokens++[("UNSOLVED",[x])])
  |fst (last tokens)=="COLON"= if isNumber x then objectToTokenList (back' (not . isNumber) xs) (tokens++[("NUMBER",x:many isNumber xs)])
    else if take 4 (x:xs) =="true" then objectToTokenList (drop 3 xs) (tokens++[("BOOLEAN","true")])
    else if take 5 (x:xs) =="false" then objectToTokenList (drop 4 xs) (tokens++[("BOOLEAN","false")])
    else objectToTokenList xs (tokens++[("UNSOLVED",[x])])
  |otherwise=objectToTokenList xs (tokens++[("UNSOLVED",[x])])


arrayToTokenList :: String -> [(String, String)] -> ([(String, String)],String)
arrayToTokenList (x:xs) tokens
  |not (null tokens)  && length (filter (\s->fst s == "ARRAY_OPEN") tokens )==length (filter (\s->fst s == "ARRAY_CLOSE") tokens)=(tokens,x:xs)
  |null xs=(tokens++[if x==']' then ("ARRAY_CLOSE","]") else ("UNSOLVED",[x])],"")
  |x==' '||x=='\n'=arrayToTokenList xs tokens
  |x=='{'=do
    let (tkn,str)=objectToTokenList (x:xs) []
    if null str then (tokens,"")
    else arrayToTokenList str (tokens++tkn)
  |x=='['=arrayToTokenList xs (tokens++[("ARRAY_OPEN","[")])
  |x==']'=arrayToTokenList xs (tokens++[("ARRAY_CLOSE","]")])
  |x==','=arrayToTokenList xs (tokens++[("COMMA",",")])
  |x=='"'=arrayToTokenList (back (=='"') xs) (tokens++[("STRING",many (/='"') xs)])
  |isNumber x=arrayToTokenList (back' (not . isNumber) xs) (tokens++[("NUMBER",x:many isNumber xs)])
  |take 4 (x:xs) =="true"=arrayToTokenList (drop 3 xs) (tokens++[("BOOLEAN","true")])
  |take 5 (x:xs) =="false"=arrayToTokenList  (drop 4 xs) (tokens++[("BOOLEAN","false")])
  |otherwise=arrayToTokenList xs (tokens++[("UNSOLVED",[x])])


parseTest :: Show a => (t -> (a, b)) -> t -> IO ()
parseTest p s = do
    print $ fst $ p s
    `catch` \(SomeException e) ->
        putStr $ show e