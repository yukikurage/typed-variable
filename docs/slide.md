---
marp: true
theme: yuki
---

## 変数に変数名を代入したい

ゆきくらげ

---

<!--
header: 変数に変数名を代入したい
class: primary
-->

## サンプルコード

```haskell
  x := y
$ read x := 1
$ read y
```

PureScript に埋め込まれた一種の言語内 DSL

---

## Y 言語

先程の DSL と 1 対 1 対応

```javascript
let x = y;
let !x = 1;
return !y;
```

今後は Y 言語でコードを書く

---

## 変数の定義，代入，参照

- 定義`let ... = ...`

```javascript
let x = 1;
```

- 代入`... = ...`

```javascript
x = 2;
```

- 参照`!`

```javascript
let y = !x;
```

---

## 戻り値

`return`

```javascript
return 1;
```

プログラムは 1 つの値を返す
必ず最後の行に書く

---

## 変数名

- 英小文字から始まる

参照演算子`!`を分ける → 変数名と変数の中身の区別が可能

```javascript
let y = 2;
let fuga = !y;
let hoge = y;
```

`!fuga == 2` `!hoge == y`

---

## サンプルコード再び

```javascript
let x = y;
let !x = 1;
return !y;
```

1 行目，`y`に`!`は付いていない → `x`に代入したのは`y`という変数名

2 行目，`!x == y` → `y`に`1`を代入

3 行目，`!y == 1` → プログラムは`1`を返す

---

## 実は……

### **型安全**

```javascript
let x = y;
!x = 1;//エラー yは定義されていない
return !y;
```

---

## 型の一つの見方

- 値
  - 実行時に評価される
- 型
  - コンパイル時に評価される

型チェックはコンパイル時に行う
→ 型レベル文字列を変数名として扱えば，型チェックが効く

---

## 割愛

- 型システム
- ラムダ式
- メタ構文

今後は再帰を書けるようにしたい
