# JSONパーサー

## 使い方
1.トークン列化する  
```haskell
let tokens=toTokenList "{\"hello\":\"world\"}"
```

2.型に合わせて関数に与える  
オブジェクトから取得する関数は`json[Type] key tokens`という形になっています。  
配列から取得する関数は`arrayjson[Type] index tokens`という形になっています。  

オブジェクトから文字列を取得するには  
```haskell
jsonString "hello" tokens
```
のようにします。  

tokenToString関数でトークン列を元に戻せます。改行、空白は失われます。  