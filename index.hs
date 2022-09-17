{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Control.Exception (SomeException(SomeException), catch)
import Data.Char (isDigit, isLetter,isHexDigit)

  
main :: IO ()
main=do
  print $ objectToTokenList "{\"string\":\"oh waaa\",\"number\":42,\"boolT\":true,\"boolF\":false}" []
  putStrLn ""
  print $ objectToTokenList "{\"level\":0,\"next\":{\"levelStr\":\"2\",\"nest\":{\"arr\":[\"str\",6,[{}]]}}}" []
  putStrLn ""
  print $ arrayToTokenList "[1,\"へろー\",[true,{\"thisId\":\"nested Object\"}]]" []


anyChar :: [Char] -> (Char, [Char])
anyChar (x:xs)=(x,xs)

--合致すれば返す
satisfy :: (a -> Bool) -> [a] -> (a, [a])
satisfy f (x:xs)
  |f x=(x,xs)

--合致している間返す
many :: (Char -> Bool) -> [Char] -> [Char]
many f (x:xs)
  |f x=if null xs then [x] else x:many f xs
  |otherwise=""

--初めて合致した物以降を返す(合致したものは含まない)
back :: (Char -> Bool) -> [Char] -> [Char]
back f (x:xs)
  |f x=xs
  |null xs=""
  |otherwise=back f xs

--初めて合致した物以降を返す(合致したものを含む)
back' :: (Char -> Bool) -> [Char] -> [Char]
back' f (x:xs)
  |f x=x:xs
  |null xs=""
  |otherwise=back' f xs
  
--含まれていれば返す
isOneOf :: [Char]-> [Char] -> (Char, [Char])
isOneOf s (x:xs)
 |x `elem` s =(x,xs)

jsonString :: [Char] -> [Char]
jsonString xs=do
  let (_,str)=isOneOf "`\"" xs
  many (/='"') str

jsonNumber :: [Char] -> [Char]
jsonNumber xs
  |fst (anyChar xs) == '-'="-"++jsonNumber (snd $ anyChar xs)
  |otherwise=n2
    where
      n1=many isDigit xs
      len=length n1
      n2=
        if length xs>len&&xs!!length n1 == '.' then
          n1++"."++jsonNumber (drop (len+1) xs)
        else n1


jsonValue :: [Char] -> [Char]
jsonValue (x:xs)
  |x=='\''||x=='"'=jsonString $ x:xs
  |x=='-'||isDigit x=jsonNumber $ x:xs
  |isLetter x=many (\v->isLetter x||isDigit x) $ x:xs


toTokenList json=fst (objectToTokenList json [])


objectToTokenList :: [Char] -> [(String,String)] -> ([(String,String)],String)
objectToTokenList (x:xs) tokens
  |not (null tokens) && length (filter (\s->fst s == "OBJECT_OPEN") tokens )==length (filter (\s->fst s == "OBJECT_CLOSE") tokens)=(tokens,x:xs)
  |null xs=(tokens++[if x=='}' then ("OBJECT_CLOSE","}") else ("UNSOLVED",[x])],"")
  |x==' '=objectToTokenList xs tokens
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


arrayToTokenList :: [Char] -> [(String, String)] -> ([(String, String)],String)
arrayToTokenList (x:xs) tokens
  |not (null tokens)  && length (filter (\s->fst s == "ARRAY_OPEN") tokens )==length (filter (\s->fst s == "ARRAY_CLOSE") tokens)=(tokens,x:xs)
  |null xs=(tokens++[if x==']' then ("ARRAY_CLOSE","]") else ("UNSOLVED",[x])],"")
  |x==' '=arrayToTokenList xs tokens
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