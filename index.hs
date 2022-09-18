{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Control.Exception (SomeException(SomeException), catch)
import Data.Char (isDigit, isLetter)
--TODO:例外処理
--Numberの小数、マイナス
--Array

main :: IO ()
main=do
  let tokens=toTokenList "{\"boolT\":true,\"taxi\":1729,\"boolF\":false,\"string\":\"oh waaa\",\"number\":42,\"minus\":-12,\"shousu\":3.14}"
  print $ jsonBoolean "boolT" tokens
  print $ jsonNumber "taxi" tokens
  print $ jsonBoolean "BoolF" tokens
  print $ jsonString "string" tokens
  print $ jsonNumber "number" tokens
  print $ jsonNumber "minus" tokens
  print $ jsonNumber "shousu" tokens
  

  putStrLn ""

  let nest=toTokenList "{\"level\":  3 ,  \"next\" :  {\"levelStr\"  :  \"2\" , \"nest\":{\"arr\":[\"str\",6,[{}]]}}}"
  print $ jsonNumber "level" nest
  let lv2=jsonObject "next" nest
  print $ tokenToString lv2
  print $ jsonString "levelStr" lv2
  let lv3=jsonObject "nest" lv2
  print $ tokenToString lv3

  --putStrLn ""
  --print $ toTokenList "[1,\"へろー\",[true,{\"thisId\":\"nested Object\"}]]"


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
  |fst token=="Array_OPEN"=tailArrayClose $ tailArrayClose tokens
  |fst token=="Array_CLOSE"=tokens
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


--含まれていれば返す
isOneOf :: String-> String -> (Char, String)
isOneOf s (x:xs)
 |x `elem` s =(x,xs)



toTokenList :: String -> [(String, String)]
toTokenList json=fst (objectToTokenList json [])

jsonString :: String -> [(String, String)] -> String
jsonString key (token:tokens)
  |fst token=="OBJECT_OPEN" =getObjectString key tokens

jsonNumber :: String -> [(String, String)] -> Int
jsonNumber key (token:tokens)
  |fst token=="OBJECT_OPEN" = getObjectNumber key tokens

jsonBoolean :: String -> [(String, String)] -> Bool
jsonBoolean key (token:tokens)
  |fst token=="OBJECT_OPEN" = getObjectBoolean key tokens

jsonObject :: String -> [(String, String)] -> [(String, String)]
jsonObject key (token:tokens)
  |fst token=="OBJECT_OPEN"=getObject key tokens


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

getObjectNumber :: String -> [(String, String)] -> Int
getObjectNumber key (token:token':tokens)
  |null tokens=(-1)
  |otherwise=case fst token of
    "KEY"
      |snd token == key ->do
        let (valueType,value)=head tokens
        if fst token' == "COLON" && valueType == "NUMBER" then read value::Int else 0
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
      |snd token == key ->do
        if fst (head tokens) == "COLON" && fst (tokens!!1) == "OBJECT_OPEN" then (tokens!!1):getAnObject (drop 2 tokens) else [("","")]
      |otherwise->getObject key tokens
    "OBJECT_OPEN"->getObject key (tailObjectClose tokens)
    "ARRAY_OPEN"->getObject key (tailArrayClose tokens)
    _->getObject key tokens

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
  |fst (last tokens)=="COLON"= if isDigit x then objectToTokenList (back' (not . isDigit) xs) (tokens++[("NUMBER",x:many isDigit xs)])
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
  |isDigit x=arrayToTokenList (back' (not . isDigit) xs) (tokens++[("NUMBER",x:many isDigit xs)])
  |take 4 (x:xs) =="true"=arrayToTokenList (drop 3 xs) (tokens++[("BOOLEAN","true")])
  |take 5 (x:xs) =="false"=arrayToTokenList  (drop 4 xs) (tokens++[("BOOLEAN","false")])
  |otherwise=arrayToTokenList xs (tokens++[("UNSOLVED",[x])])


parseTest :: Show a => (t -> (a, b)) -> t -> IO ()
parseTest p s = do
    print $ fst $ p s
    `catch` \(SomeException e) ->
        putStr $ show e