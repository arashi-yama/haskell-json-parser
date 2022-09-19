import Index (toTokenList,jsonString,jsonInt,jsonDouble,jsonBoolean,jsonArray,jsonObject,arrayjsonInt,arrayjsonDouble,arrayjsonString,arrayjsonArray,arrayjsonObject,tokenToString,arrayjsonBoolean)

main :: IO ()
main=do
  json<-readFile "./example.json"
  let root=toTokenList json
  print $ jsonString "name" root                         --haskell-json-parser
  print $ jsonInt "version" root                         --0
  print $ arrayjsonString 0 (jsonArray "authors" root)   --arashiyama
  let primes=jsonArray "primes" root
  print [arrayjsonInt x primes|x<-[0..8]]                --[2,3,5,7,9,11,13,17,19]

  let intro=jsonObject "introduceMyself" root
  print $ jsonBoolean "male" intro                       --true
  print $ jsonBoolean "famale" intro                     --false
  print $ jsonDouble "most like number" intro            --3.14
  print $ jsonString "date" intro                        --2022-09-19T10:38:00Z
  let programingLangs=jsonArray "programing languages" intro

  let js=arrayjsonObject 0 programingLangs
  print $ jsonString "name" js                           --JavaScript
  print $ jsonString "skill" js                          --noob

  putStrLn $ tokenToString programingLangs               --[{"name":"JavaScript","skill":"noob"},{"name":"Kotlin","skill":"noob"},{"name":"Haskell","skill":"noob"}]
  --print json


arrayTest :: IO ()
arrayTest=do
  let tokens=toTokenList "{\"array\":[\"one\",{\"aiueo\":[\"kakikukeko\"]},65536,[1,2,3],\"three.js\",true,false,1.414]}"
  let array=jsonArray "array" tokens
  print array
  print $ arrayjsonString 0 array
  let obj=arrayjsonObject 1 array
  let arr=jsonArray "aiueo" obj
  print $ arrayjsonString 0 arr
  print $ arrayjsonInt 2 array
  let ary=arrayjsonArray 3 array
  print $ tokenToString ary
  print $ arrayjsonInt 0 ary
  print $ arrayjsonInt 1 ary
  print $ arrayjsonInt 2 ary
  print $ arrayjsonBoolean 4 array
  print $ arrayjsonBoolean 5 array
  print $ arrayjsonDouble 7 array
  let array2=toTokenList "[0,1,2,\"Fizz\",4 , \"Bazz\"]"
  putStrLn ""
  print array2
  print $ arrayjsonInt 0 array2
  print $ arrayjsonInt 1 array2
  print $ arrayjsonInt 2 array2
  print $ arrayjsonString 3 array2
  print $ arrayjsonInt 4 array2
  print $ arrayjsonString 5 array2
  print array2

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
