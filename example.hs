import Index (jsonString,jsonInt,jsonDouble,jsonBoolean,jsonArray,jsonObject,arrayjsonInt,arrayjsonDouble,arrayjsonString,arrayjsonArray,arrayjsonObject,tokenToString,arrayjsonBoolean, getObject, rootJson, rootArrayjson)
import Data.Either (fromRight, fromLeft)

main :: IO ()
main=do
  json<-readFile "./example.json"
  let root=rootJson json
  let name=root>>=jsonString "name"
  let version=root>>=jsonInt "version"
  let authors=root>>=jsonArray "authors"
  let author=authors>>=arrayjsonString 0
  let primes=root>>=jsonArray "primes"
  let primesList=[primes>>=arrayjsonInt x|x<-[0..8]]
  let introduce=root>>=jsonObject "introduce myself"
  let isMale=introduce>>=jsonBoolean "male"
  let isFamale=introduce>>=jsonBoolean "famale"
  let from=introduce>>=jsonString "from"
  let mostLikeNumber=introduce>>=jsonDouble "most like number"
  let programingLanguages=introduce>>=jsonArray "programing languages"
  let phead=programingLanguages>>=arrayjsonObject 0
  let pheadName=phead>>=jsonString "name"
  let pheadSkill=phead>>=jsonString "skill"
  print name--"arashiyama"
  print version--0
  print author--"arashiyama"
  print primesList--[2,3,5,7,9,11,13,17,19]
  print isMale--True
  print isFamale--False
  print from--"Japan"
  print mostLikeNumber--3.14
  print pheadName--"JavaScript"
  print pheadSkill--"noob"
  either putStrLn (putStrLn . tokenToString) programingLanguages


