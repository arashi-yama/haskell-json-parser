# JSONパーサー
## 使い方
1.トークン列化する  
```haskell
let root=rootJson "{\"hello\":\"world\"}"
let array=rootArrayjson "[1,2,3]"
```

2.型に合わせて関数に与える  
JSONオブジェクトから取得する関数は`json[Type] key tokens`という形になっています。  
JSON配列から取得する関数は`arrayjson[Type] index tokens`という形になっています。  
JSONから値を取得するには  
```haskell
root>>=jsonString "hello" --world
array>>=arrayjsonInt 0 --1
```
のようにします。  

tokenToString関数でトークン列を元に戻せます。パースした箇所の改行、空白は失われます。  