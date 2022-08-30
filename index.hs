{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Control.Exception (SomeException(SomeException), catch)
import Data.Char (isDigit, isLetter)
main :: IO ()
main=do
  print $ jsonValue "Hello"
  print $ jsonValue "Hello wrold"
  print $ jsonValue "1232"
  print $ jsonValue "-1232"
  print $ jsonValue "-1232.32"


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

parseTest :: Show a => (t -> (a, b)) -> t -> IO ()
parseTest p s = do
    print $ fst $ p s
    `catch` \(SomeException e) ->
        putStr $ show e