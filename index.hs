{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Index where
import Data.Char (isDigit, isLetter)
--TODO:例外処理

tokenToString :: [(String, String)] -> String
tokenToString=concatMap (\(tkn,value)->if tkn=="KEY"||tkn=="STRING" then "\""++value++"\"" else value)

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

--生の文字列のオブジェクトを一つ返す
getAnObjectString :: [Char] -> [Char]
getAnObjectString (x:xs)
  |null xs=[x]
  |x=='}'=[x]
  |otherwise=do
    let obj=getAnObjectString xs
    let as=getAnObjectString (drop (length obj) xs)
    x:if x=='{' then obj++as else getAnObjectString xs

getAnArrayString :: [Char] -> [Char]
getAnArrayString (x:xs)
  |null xs=[x]
  |x==']'=[x]
  |otherwise=do
    let arr=getAnArrayString xs
    let as=getAnArrayString (drop (length arr) xs)
    x:if x=='[' then arr++as else getAnArrayString xs

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


isNumber :: Char -> Bool
isNumber x=isDigit x || x=='-' ||x=='.'

toTokenList :: String -> [(String, String)]
toTokenList json= fst (objectToTokenList json [])

rootJson :: String -> [(String, String)]
rootJson json=fst $ objectToTokenList json []

rootArrayjson json=fst $ arrayToTokenList json []

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
  |fst token=="ARRAY_OPEN"=read $ getArrayNumber index tokens::Int

arrayjsonDouble :: Int -> [(String, String)] -> Double
arrayjsonDouble index (token:tokens)
  |fst token=="ARRAY_OPEN"=read $ getArrayNumber index tokens::Double

arrayjsonBoolean :: Int -> [(String, String)] -> Bool
arrayjsonBoolean index (token:tokens)
  |fst token=="ARRAY_OPEN"=getArrayBoolean index tokens

arrayjsonObject :: Int -> [(String, String)] -> [(String, String)]
arrayjsonObject index (token:tokens)
  |fst token=="ARRAY_OPEN"=getArrayObject index tokens

arrayjsonArray :: Int -> [(String, String)] -> [(String, String)]
arrayjsonArray index (token:tokens)
  |fst token=="ARRAY_OPEN"=getArrayArray index tokens

getObjectString :: String -> [(String, String)] -> String
--オブジェクトはKEY,COLON,VALUE,COMMAの四周期で回る
--                      KEY               COLON              VALUE              COMMA
getObjectString key ((fstType,fstValue):(sndType,sndValue):(trdType,trdValue):(fothType,fothValue):tokens)
  |fstType=="KEY"&&fstValue==key&&sndType=="COLON"&&trdType=="STRING"=trdValue
  |otherwise=getObjectString key tokens

getObjectNumber :: String -> [(String, String)] -> String
getObjectNumber key ((fstType,fstValue):(sndType,sndValue):(trdType,trdValue):(fothType,fothValue):tokens)
  |fstType=="KEY"&&fstValue==key&&sndType=="COLON"&&trdType=="NUMBER"=trdValue
  |otherwise=getObjectNumber key tokens

getObjectBoolean :: String -> [(String, String)] -> Bool
getObjectBoolean key ((fstType,fstValue):(sndType,sndValue):(trdType,trdValue):(fothType,fothValue):tokens)
  |fstType=="KEY"&&fstValue==key&&sndType=="COLON"&&trdType=="BOOLEAN"=trdValue=="true"
  |otherwise=getObjectBoolean key tokens

getObject :: String -> [(String, String)] -> [(String, String)]
getObject key ((fstType,fstValue):(sndType,sndValue):(trdType,trdValue):(fothType,fothValue):tokens)
  |fstType=="KEY"&&fstValue==key&&sndType=="COLON"&&trdType=="OBJECT"=fst $ objectToTokenList trdValue []
  |otherwise=getObject key tokens


getObjectArray :: String -> [(String, String)] -> [(String, String)]
getObjectArray key ((fstType,fstValue):(sndType,sndValue):(trdType,trdValue):(fothType,fothValue):tokens)
  |fstType=="KEY"&&fstValue==key&&sndType=="COLON"&&trdType=="ARRAY"=fst $ arrayToTokenList trdValue []
  |otherwise=getObjectArray key tokens

getArrayString :: Int -> [(String, String)] -> String
--Arrayは VALUE COMMA の二周期
--                      VALUE              COMMA
getArrayString index ((fstType,fstValue):(sndType,sndValue):tokens)
  |index==0&&fstType=="STRING"=fstValue
  |index>0=getArrayString (index-1) tokens
  

getArrayNumber ::Int -> [(String, String)] -> String
getArrayNumber index ((fstType,fstValue):(sndType,sndValue):tokens)
  |index==0&&fstType=="NUMBER"=fstValue
  |index>0=getArrayNumber (index-1) tokens

getArrayBoolean :: Int -> [(String, String)] -> Bool
getArrayBoolean index ((fstType,fstValue):(sndType,sndValue):tokens)
  |index==0&&fstType=="BOOLEAN"=fstValue=="true"
  |index>0=getArrayBoolean (index-1) tokens

getArrayArray :: Int -> [(String, String)] -> [(String, String)]
getArrayArray index ((fstType,fstValue):(sndType,sndValue):tokens)
  |index==0&&fstType=="ARRAY"=fst $ arrayToTokenList fstValue []
  |index>0=getArrayArray (index-1) tokens
  
getArrayObject ::Int-> [(String, String)] -> [(String, String)]
getArrayObject index ((fstType,fstValue):(sndType,sndValue):tokens)
  |index==0&&fstType=="OBJECT"=fst $ objectToTokenList fstValue []
  |index>0=getArrayObject (index-1) tokens


objectToTokenList :: String -> [(String,String)] -> ([(String,String)],String)
objectToTokenList (x:xs) tokens
  |not (null tokens) && length (filter (\s->fst s == "OBJECT_OPEN") tokens )==length (filter (\s->fst s == "OBJECT_CLOSE") tokens)=(tokens,x:xs)
  |null xs=(tokens++[if x=='}' then ("OBJECT_CLOSE","}") else ("UNSOLVED",[x])],"")
  |x==' '||x=='\n'=objectToTokenList xs tokens
  |null tokens&&x=='{'=objectToTokenList xs [("OBJECT_OPEN","{")]
  |x=='['=do
    let arr=getAnArrayString xs
    let tails=drop (length arr) xs
    if null tails then (tokens++[("ARRAY",x:arr)],"") else objectToTokenList tails $ tokens++[("ARRAY",x:arr)]
  |x=='{'=do
    let obj=getAnObjectString xs
    let tails=drop (length obj) xs
    if null tails then (tokens++[("OBJECT",x:obj)],"") else objectToTokenList tails $ tokens++[("OBJECT",x:obj)]
  |x==':'=objectToTokenList xs (tokens++[("COLON",":")])
  |x==','=objectToTokenList xs (tokens++[("COMMA",",")])
  |x=='"'= if fst (last tokens)=="COMMA"||fst (last tokens)=="OBJECT_OPEN"
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
  |null tokens&&x=='['=arrayToTokenList xs (tokens++[("ARRAY_OPEN","[")])
  |x=='['=do
    let arr=getAnArrayString xs
    let tails=drop (length arr) xs
    if null tails then (tokens++[("ARRAY",x:arr)],"") else arrayToTokenList tails $ tokens++[("ARRAY",x:arr)]
  |x=='{'=do
    let obj=getAnObjectString xs
    let tails=drop (length obj) xs
    if null tails then (tokens++[("OBJECT",x:obj)],"") else arrayToTokenList tails $ tokens++[("OBJECT",x:obj)]
  |x==','=arrayToTokenList xs (tokens++[("COMMA",",")])
  |x=='"'=arrayToTokenList (back (=='"') xs) (tokens++[("STRING",many (/='"') xs)])
  |isNumber x=arrayToTokenList (back' (not . isNumber) xs) (tokens++[("NUMBER",x:many isNumber xs)])
  |take 4 (x:xs) =="true"=arrayToTokenList (drop 3 xs) (tokens++[("BOOLEAN","true")])
  |take 5 (x:xs) =="false"=arrayToTokenList  (drop 4 xs) (tokens++[("BOOLEAN","false")])
  |otherwise=arrayToTokenList xs (tokens++[("UNSOLVED",[x])])