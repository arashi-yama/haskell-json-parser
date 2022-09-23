import Index (jsonString,jsonInt,jsonDouble,jsonBoolean,jsonArray,jsonObject,arrayjsonInt,arrayjsonDouble,arrayjsonString,arrayjsonArray,arrayjsonObject,tokenToString,arrayjsonBoolean, getObject, rootJson, rootArrayjson)
import Data.Either ()

arrayTest :: IO ()
arrayTest=do
  let root=rootArrayjson "[\"one\",65536,1.414,true,false,{\"aiueo\":[\"kakikukeko\"]},[1,2,3],null]"
  let zero=root>>=arrayjsonString 0
  let one=root>>=arrayjsonInt 1
  let two=root>>=arrayjsonDouble 2
  let three=root>>=arrayjsonBoolean 3
  let four=root>>=arrayjsonBoolean 4
  let five=root>>=arrayjsonObject 5
  let kaki=five>>=jsonArray "aiueo">>=arrayjsonString 0
  let array=root>>=arrayjsonArray 6
  let ao=array>>=arrayjsonInt 0
  let at=array>>=arrayjsonInt 1
  let ath=array>>=arrayjsonInt 2
  let nul=root>>=arrayjsonString 7
  print $ tokenToString <$> root
  print zero 
  print one
  print two
  print three
  print four
  print kaki
  print $ tokenToString <$> array
  print ao
  print at
  print ath
  print nul

objTest :: IO ()
objTest=do
  putStrLn "String"
  let root=rootJson "{\"boolT\":true,\"taxi\":1729,\"boolF\":false,\"string\":\"oh waaa\",\"number\":42,\"minus\":-12,\"shousu\":3.14,\"nul\":null}"
  let boolT=root>>=jsonBoolean "boolT"
  let taxi=root>>=jsonInt "taxi"
  let boolF=root>>=jsonBoolean "boolF"
  let string=root>>=jsonString "string"
  let number=root>>=jsonInt "number"
  let minus=root>>=jsonInt "minus"
  let shousu=root>>=jsonDouble "shousu"
  let nul=root>>=jsonDouble "nul"
  print root
  print boolT
  print taxi
  print boolF
  print string
  print number
  print minus
  print shousu
  either putStrLn (putStrLn . tokenToString) root
 
illed :: IO ()
illed=do
  print $ rootJson ""
  print $ rootJson "{"
  print $ rootJson "}"
  print $ rootJson "{123:\"123\""
  print $ rootJson "{[]}"
  print $ rootJson "{{}}"
  print $ rootJson "{{}"
  print $ rootJson "{}}"
  print $ rootJson "{[}"
  print $ rootJson "{]}"
  print $ rootJson "{\"}"
  print $ rootJson "{wowo}"
  print $ rootJson "{\"key\",null}"
  print $ rootJson "{\"key\",\"wawo\"}"
  print $ rootJson "{\"key\",{}}"
  print $ rootJson "{\"key\":,\"a\"}"
  print $ rootJson "{\"key\":oh}"
  print $ rootJson "{\"key\":{}"
  print $ rootJson "{\"key\":truetrue}"
  print $ rootJson "{\"key\":falseoa}"
  print $ rootJson "{\"key\":nullnull}"

  putStrLn ""

  print $ rootArrayjson ""
  print $ rootArrayjson "["
  print $ rootArrayjson "]"
  print $ rootArrayjson "[1,2,3"
  print $ rootArrayjson "[,,"
  print $ rootArrayjson "[,]"
  print $ rootArrayjson "[oh]"
  print $ rootArrayjson "[[]"
  print $ rootArrayjson "[{]}"
  print $ rootArrayjson "[truetrue]"
  print $ rootArrayjson "[falsefalse]"
  print $ rootArrayjson "[truefalse]"
  print $ rootArrayjson "[-1]"
  print $ rootArrayjson "[-1-1-1-1-1-1]"
  print $ rootArrayjson "[....]"

  

  print 1