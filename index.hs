module Index where
import Data.Char (isDigit, isLetter)
import Data.Either ()

data JSONToken=OBJECT_OPEN|OBJECT_CLOSE|ARRAY_OPEN|ARRAY_CLOSE|COMMA|KEY|COLON|STRING|NUMBER|BOOLEAN|ARRAY|OBJECT|NULL|START deriving (Show,Eq)        
type Token=(JSONToken,String)

tokenToString :: [Token] -> String
tokenToString=concatMap (\(typ,value)->if typ==KEY||typ==STRING then "\""++value++"\"" else value)

--合致している間返す
many :: (Char -> Bool) -> String -> String
many _ []=""
many f (x:xs)
  |f x=if null xs then [x] else x:many f xs
  |otherwise=""

--初めて合致した物以降を返す(合致したものは含まない)
back :: (Char -> Bool) -> String -> String
back _ []=""
back f (x:xs)
  |f x=xs
  |null xs=""
  |otherwise=back f xs

--初めて合致した物以降を返す(合致したものを含む)
back' :: (Char -> Bool) -> String -> String
back' _ []=""
back' f (x:xs)
  |f x=x:xs
  |null xs=""
  |otherwise=back' f xs

--生の文字列のオブジェクトを一つ返す
getAnObjectString :: String -> String
getAnObjectString []=""
getAnObjectString (x:xs)
  |null xs=[x]
  |x=='}'=[x]
  |otherwise=do
    let obj=getAnObjectString xs
    let as=getAnObjectString (drop (length obj) xs)
    x:if x=='{' then obj++as else getAnObjectString xs

getAnArrayString :: String -> String
getAnArrayString []=""
getAnArrayString (x:xs)
  |null xs=[x]
  |x==']'=[x]
  |otherwise=do
    let arr=getAnArrayString xs
    let as=getAnArrayString (drop (length arr) xs)
    x:if x=='[' then arr++as else getAnArrayString xs

--初めて余った閉じカッコ以降を返す
tailObjectClose :: [Token] -> [Token]
tailObjectClose []=[]
tailObjectClose (token:tokens)
  |null tokens=[]
  |fst token==OBJECT_OPEN=tailObjectClose $ tailObjectClose tokens
  |fst token==OBJECT_CLOSE=tokens
  |otherwise=tailObjectClose tokens

tailArrayClose :: [Token] -> [Token]
tailArrayClose []=[]
tailArrayClose (token:tokens)
  |null tokens=[]
  |fst token==ARRAY_OPEN=tailArrayClose $ tailArrayClose tokens
  |fst token==ARRAY_CLOSE=tokens
  |otherwise=tailArrayClose tokens

--オブジェクトを一つ返す
getAnObject :: [Token] -> [Token]
getAnObject []=[]
getAnObject (token:tokens)
  |null tokens=[token]
  |fst token==OBJECT_CLOSE=[token]
  |otherwise=do
    let obj=getAnObject tokens
    let as=getAnObject (drop (length obj) tokens)
    token:if fst token==OBJECT_OPEN then obj++as else getAnObject tokens

getAnArray :: [Token] -> [Token]
getAnArray []=[]
getAnArray (token:tokens)
  |null tokens=[token]
  |fst token==ARRAY_CLOSE=[token]
  |otherwise=do
    let arr=getAnArray tokens
    let as=getAnArray (drop (length arr) tokens)
    token:if fst token==ARRAY_OPEN then arr++as else getAnArray tokens

getNumber :: String -> String
getNumber []=""
getNumber xs
  |head xs=='-'='-':getNumber (tail xs)
  |otherwise=do
    let int=many isDigit xs
    let intLength=length int
    let isDecimal=(length xs > intLength) && (xs !! intLength == '.')
    if isDecimal then int++"."++many isDigit (drop (intLength+1) xs) else int


rootJson :: String -> Either String [Token]
rootJson json= objectToTokenList json [] START

rootArrayjson :: String -> Either String [Token]
rootArrayjson json=arrayToTokenList json [] START

jsonString :: String -> [Token] -> Either String String
jsonString _ []=Left "Empty JSON input"
jsonString key (token:tokens)
  |fst token==OBJECT_OPEN =getObjectString key tokens
  |otherwise=Left "Must start with '{'"

jsonInt :: String -> [Token] -> Either String Int
jsonInt _ []=Left "Empty JSON input"
jsonInt key (token:tokens)
  |fst token==OBJECT_OPEN = read <$> getObjectNumber key tokens
  |otherwise=Left "Must start with '{'"

jsonDouble :: String -> [Token] -> Either String Double
jsonDouble _ []=Left "Empty JSON input"
jsonDouble key (token:tokens)
  |fst token==OBJECT_OPEN = read <$> getObjectNumber key tokens
  |otherwise=Left "Must start with '{'"

jsonBoolean :: String -> [Token] -> Either String Bool
jsonBoolean _ []=Left "Empty JSON input"
jsonBoolean key (token:tokens)
  |fst token==OBJECT_OPEN = getObjectBoolean key tokens
  |otherwise=Left "Must start with '{'"

jsonObject :: String -> [Token] -> Either String [Token]
jsonObject _ []=Left "Empty JSON input"
jsonObject key (token:tokens)
  |fst token==OBJECT_OPEN=getObject key tokens
  |otherwise=Left "Must start with '{'"


jsonArray :: String -> [Token] -> Either String [Token]
jsonArray _ []=Left "Empty JSON input"
jsonArray key (token:tokens)
  |fst token==OBJECT_OPEN=getObjectArray key tokens
  |otherwise=Left "Must start with '{'"


arrayjsonString :: Int -> [Token] ->Either String String
arrayjsonString _ []=Left "Empty JSON input"
arrayjsonString index (token:tokens)
  |fst token==ARRAY_OPEN=getArrayString index tokens
  |otherwise=Left "Must start with '['"

arrayjsonInt :: Int -> [Token] ->Either String Int
arrayjsonInt _ []=Left "Empty JSON input"
arrayjsonInt index (token:tokens)
  |fst token==ARRAY_OPEN=read <$> getArrayNumber index tokens
  |otherwise=Left "Must start with '['"


arrayjsonDouble :: Int -> [Token] ->Either String Double
arrayjsonDouble _ []=Left "Empty JSON input"
arrayjsonDouble index (token:tokens)
  |fst token==ARRAY_OPEN=read <$> getArrayNumber index tokens
  |otherwise=Left "Must start with '['"

arrayjsonBoolean :: Int -> [Token] ->Either String Bool
arrayjsonBoolean _ []=Left "Empty JSON input"
arrayjsonBoolean index (token:tokens)
  |fst token==ARRAY_OPEN=getArrayBoolean index tokens
  |otherwise=Left "Must start with '['"

arrayjsonObject :: Int -> [Token] ->Either String [Token]
arrayjsonObject _ []=Left "Empty JSON input"
arrayjsonObject index (token:tokens)
  |fst token==ARRAY_OPEN=getArrayObject index tokens
  |otherwise=Left "Must start with '['"

arrayjsonArray :: Int -> [Token] ->Either String [Token]
arrayjsonArray _ []=Left "Empty JSON input"
arrayjsonArray index (token:tokens)
  |fst token==ARRAY_OPEN=getArrayArray index tokens
  |otherwise=Left "Must start with '['"

getObjectString :: String -> [Token] -> Either String String
getObjectString _ []=Left "String length is not enought"
getObjectString _ [_]=Left "String length is not enought"
getObjectString _ [_,_]=Left "String length is not enought"
getObjectString _ [_,_,_]=Left "String length is not enought"
getObjectString key ((fstType,fstValue):(sndType,sndValu):(trdType,trdValue):(fothType,fothValue):tokens)
  |not $ fstType==KEY&&sndType==COLON&&(fothType==COMMA||fothType==OBJECT_CLOSE)=Left "Syntax Error"
  |fstValue==key=case trdType of
    STRING->Right trdValue
    NULL->Left "null"
    _->Left $ "Couldn't match type. Exepted type:STRING. ActualType:"++show trdType
  |fothType==OBJECT_CLOSE=Left "No such a key"
  |otherwise=getObjectString key tokens

getObjectNumber :: String -> [Token] ->  Either String String
getObjectNumber _ []=Left "String length is not enought"
getObjectNumber _ [_]=Left "String length is not enought"
getObjectNumber _ [_,_]=Left "String length is not enought"
getObjectNumber _ [_,_,_]=Left "String length is not enought"
getObjectNumber key ((fstType,fstValue):(sndType,sndValue):(trdType,trdValue):(fothType,fothValue):tokens)
  |not $ fstType==KEY&&sndType==COLON&&(fothType==COMMA||fothType==OBJECT_CLOSE)=Left "Syntax Error"
  |fstValue==key=case trdType of
    NUMBER->Right trdValue
    NULL->Left "null"
    _->Left $ "Couldn't match type. Exepted type:NUMBER. ActualType:"++show trdType
  |fothType==OBJECT_CLOSE=Left "No such a key"
  |otherwise=getObjectNumber key tokens

getObjectBoolean :: String -> [Token] -> Either String Bool
getObjectBoolean _ []=Left "String length is not enought"
getObjectBoolean _ [_]=Left "String length is not enought"
getObjectBoolean _ [_,_]=Left "String length is not enought"
getObjectBoolean _ [_,_,_]=Left "String length is not enought"
getObjectBoolean key ((fstType,fstValue):(sndType,sndValue):(trdType,trdValue):(fothType,fothValue):tokens)
  |not $ fstType==KEY&&sndType==COLON&&(fothType==COMMA||fothType==OBJECT_CLOSE)=Left "Syntax Error"
  |fstValue==key=case trdType of
    BOOLEAN->Right $ trdValue=="true"
    NULL->Left "null"
    _->Left $ "Couldnt match type. Exepted type:BOOLEAN. ActualType:"++show trdType
  |fothType==OBJECT_CLOSE=Left "No such a key"
  |otherwise=getObjectBoolean key tokens

getObject :: String -> [Token] -> Either String [Token]
getObject _ []=Left "String length is not enought"
getObject _ [_]=Left "String length is not enought"
getObject _ [_,_]=Left "String length is not enought"
getObject _ [_,_,_]=Left "String length is not enought"
getObject key ((fstType,fstValue):(sndType,sndValue):(trdType,trdValue):(fothType,fothValue):tokens)
  |not $ fstType==KEY&&sndType==COLON&&(fothType==COMMA||fothType==OBJECT_CLOSE)=Left "Syntax Error"
  |fstValue==key=case trdType of
    OBJECT->objectToTokenList trdValue [] START
    NULL->Left "null"
    _->Left $ "Couldn't match type. Exepted type:OBJECT. ActualType:"++show trdType
  |fothType==OBJECT_CLOSE=Left "No such a key"
  |otherwise=getObject key tokens


getObjectArray :: String -> [Token] ->  Either String [Token]
getObjectArray _ []=Left "String length is not enought"
getObjectArray _ [_]=Left "String length is not enought"
getObjectArray _ [_,_]=Left "String length is not enought"
getObjectArray _ [_,_,_]=Left "String length is not enought"
getObjectArray key ((fstType,fstValue):(sndType,sndValue):(trdType,trdValue):(fothType,fothValue):tokens)
  |not $ fstType==KEY&&sndType==COLON&&(fothType==COMMA||fothType==OBJECT_CLOSE)=Left "Syntax Error"
  |fstValue==key=case trdType of
    ARRAY->arrayToTokenList trdValue [] START
    NULL->Left "null"
    _->Left $ "Couldn't match type. Exepted type:ARRAY. ActualType:"++show trdType
  |fothType==OBJECT_CLOSE=Left "No such a key"
  |fothType==OBJECT_CLOSE=Left "No such a key"
  |otherwise=getObjectArray key tokens

getArrayString :: Int -> [Token] -> Either String String
getArrayString _ []=Left "String length is not enought"
getArrayString _ [_]=Left "String length is not enought"
getArrayString index ((fstType,fstValue):(sndType,sndValue):tokens)
  |sndType/=COMMA&&sndType/=ARRAY_CLOSE=Left "Syntax Error"
  |index==0=case fstType of
    STRING->Right fstValue
    NULL->Left "null"
    _->Left $ "Couldn't match type. Exepted type:String. ActualType:"++show fstType
  |sndType==ARRAY_CLOSE=Left "Index is too learge"
  |index>0=getArrayString (index-1) tokens
  |otherwise=Left "Uncaught parse failure"


getArrayNumber ::Int -> [Token] -> Either String String
getArrayNumber _ []=Left "String length is not enought"
getArrayNumber _ [_]=Left "String length is not enought"
getArrayNumber index ((fstType,fstValue):(sndType,sndValue):tokens)
  |sndType/=COMMA&&sndType/=ARRAY_CLOSE=Left "Syntax Error"
  |index==0=case fstType of
    NUMBER->Right fstValue
    NULL->Left "null"
    _->Left $ "Couldn't match type. Exepted type:NUMBER. ActualType:"++show fstType
  |sndType==ARRAY_CLOSE=Left "Index is too learge"
  |index>0=getArrayNumber (index-1) tokens
  |otherwise=Left "Uncaught parse failure"

getArrayBoolean :: Int -> [Token] -> Either String Bool
getArrayBoolean _ []=Left "String length is not enought"
getArrayBoolean _ [_]=Left "String length is not enought"
getArrayBoolean index ((fstType,fstValue):(sndType,sndValue):tokens)
  |sndType/=COMMA&&sndType/=ARRAY_CLOSE=Left "Syntax Error"
  |index==0=case fstType of
    BOOLEAN->Right $ fstValue=="true"
    NULL->Left "null"
    _->Left $ "Couldn't match type. Exepted type:BOOLEAN. ActualType:"++show fstType
  |sndType==ARRAY_CLOSE=Left "Index is too learge"
  |index>0=getArrayBoolean (index-1) tokens
  |otherwise=Left "Uncaught parse failure"

getArrayArray :: Int -> [Token] -> Either String[Token]
getArrayArray _ []=Left "String length is not enought"
getArrayArray _ [_]=Left "String length is not enought"
getArrayArray index ((fstType,fstValue):(sndType,sndValue):tokens)
  |sndType/=COMMA&&sndType/=ARRAY_CLOSE=Left "Syntax Error"
  |index==0=case fstType of
    ARRAY->arrayToTokenList fstValue [] START
    NULL->Left "null"
    _->Left $ "Couldn't match type. Exepted type:ARRAY. ActualType:"++show fstType
  |sndType==ARRAY_CLOSE=Left "Index is too learge"
  |index>0=getArrayArray (index-1) tokens
  |otherwise=Left "Uncaught parse failure"

getArrayObject ::Int-> [Token] -> Either String [Token]
getArrayObject _ []=Left "String length is not enought"
getArrayObject _ [_]=Left "String length is not enought"
getArrayObject index ((fstType,fstValue):(sndType,sndValue):tokens)
  |sndType/=COMMA&&sndType/=ARRAY_CLOSE=Left "Syntax Error"
  |index==0=case fstType of
    OBJECT->objectToTokenList fstValue [] START
    NULL->Left "null"
    _->Left $ "Couldn't match type. Exepted type:OBJECT. ActualType:"++show fstType
  |sndType==ARRAY_CLOSE=Left "Index is too learge"
  |index>0=getArrayObject (index-1) tokens
  |otherwise=Left "Uncaught parse failure"


makeErrorText :: Char -> [(JSONToken, String)] -> String
makeErrorText x tokens="\nat "++parsed++"\n"++errored
  where
    parsed=tokenToString tokens++[x]
    errored=replicate (length parsed+2) ' '++"^"

objectToTokenList :: String -> [Token] -> JSONToken ->Either String [Token]
objectToTokenList "" _ _=Left "Empty String"
--lastの参照はO(N).何度か last tokens するよりは引数に追加した
objectToTokenList json@(x:xs) tokens lastTokenType
  |null xs=if x=='}'&&not (null tokens) then Right (tokens++[(OBJECT_CLOSE,"}")]) else Left $ "Unexpected JSON end by "++[x]
  |x==' '||x=='\n'=objectToTokenList xs tokens lastTokenType
  |null tokens=if x=='{' then objectToTokenList xs [(OBJECT_OPEN,"{")] OBJECT_OPEN else Left $ "Object must start with '{' but start with "++[x]      
  |x==':'&&lastTokenType==KEY=objectToTokenList xs (tokens++[(COLON,":")]) COLON
  |x==','&&lastTokenType `elem` [STRING,NUMBER,BOOLEAN,NULL,OBJECT,ARRAY]=objectToTokenList xs (tokens++[(COMMA,",")]) COMMA
  |x=='"'&&(lastTokenType==COMMA||lastTokenType==OBJECT_OPEN)=objectToTokenList (back (=='"') xs) (tokens++[(KEY,many (/='"') xs)]) KEY
  |lastTokenType/=COLON=Left $ "Unepected token:"++[x]++makeErrorText x tokens
  |x=='['=do
    let arr=getAnArrayString xs
    let tails=drop (length arr) xs
    if null tails then Left "End with Array" else objectToTokenList tails (tokens++[(ARRAY,x:arr)]) ARRAY
  |x=='{'=do
    let obj=getAnObjectString xs
    let tails=drop (length obj) xs
    if null tails then Left "End with Object" else objectToTokenList tails (tokens++[(OBJECT,x:obj)]) OBJECT
  |x=='"'=objectToTokenList (back (=='"') xs) (tokens++[(STRING,many (/='"') xs)]) STRING
  |isDigit x||x=='-'=do
    let num=getNumber json
    objectToTokenList (drop (length num) json) (tokens++[(NUMBER,num)]) NUMBER
  |take 4 json=="true"=objectToTokenList (drop 3 xs) (tokens++[(BOOLEAN,"true")]) BOOLEAN
  |take 5 json=="false"=objectToTokenList (drop 4 xs) (tokens++[(BOOLEAN,"false")]) BOOLEAN
  |take 4 json=="null"= objectToTokenList (drop 3 xs) (tokens++[(NULL,"null")]) NULL
  |otherwise=Left $ "Unsolved token:"++[x]++makeErrorText x tokens


arrayToTokenList :: String -> [Token] -> JSONToken ->Either String [Token]
arrayToTokenList "" _ _=Left "Empty String"
arrayToTokenList array@(x:xs) tokens lastTokenType
  |null xs=if x==']'&&not (null tokens) then Right $ tokens++[(ARRAY_CLOSE,"]")] else Left $ "Unexpected JSON end by "++[x]
  |x==' '||x=='\n'=arrayToTokenList xs tokens lastTokenType
  |null tokens&&x=='['=arrayToTokenList xs (tokens++[(ARRAY_OPEN,"[")]) ARRAY_OPEN
  |x==','&&lastTokenType `elem` [STRING,NUMBER,BOOLEAN,NULL,OBJECT,ARRAY]=arrayToTokenList xs (tokens++[(COMMA,",")]) COMMA
  |lastTokenType/=COMMA&&lastTokenType/=ARRAY_OPEN=Left $ "Unexpected token:"++[x]++makeErrorText x tokens
  |x=='['=do
    let arr=getAnArrayString xs
    let tails=drop (length arr) xs
    if null tails then Left "End with Array" else arrayToTokenList tails (tokens++[(ARRAY,x:arr)]) ARRAY
  |x=='{'=do
    let obj=getAnObjectString xs
    let tails=drop (length obj) xs
    if null tails then Left "End with Array" else arrayToTokenList tails (tokens++[(OBJECT,x:obj)]) OBJECT
  |x=='"'=arrayToTokenList (back (=='"') xs) (tokens++[(STRING,many (/='"') xs)]) STRING
  |isDigit x||x=='-'=do
    let num=getNumber array
    arrayToTokenList (drop (length num) array) (tokens++[(NUMBER,num)]) NUMBER
  |take 4 (x:xs) =="true"=arrayToTokenList (drop 3 xs) (tokens++[(BOOLEAN,"true")]) BOOLEAN
  |take 5 (x:xs) =="false"=arrayToTokenList  (drop 4 xs) (tokens++[(BOOLEAN,"false")]) BOOLEAN
  |take 4 (x:xs) =="null"=arrayToTokenList (drop 3 xs) (tokens++[(NULL,"null")]) NULL
  |otherwise=Left $ "Unsolved token:"++[x]++makeErrorText x tokens