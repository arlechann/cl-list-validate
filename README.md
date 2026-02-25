# list-validate

`list-validate` は Common Lisp で書かれた、宣言的な parse / validate 用ユーティリティです。  
`alist` / `plist` / `list` / 単一値に対して、ルールを前方から順番に適用できます。

## 特徴

- `validate-*`: 検証のみ
- `parse-*`: 変換のみ
- `parse+validate-*`: 変換後に検証
- `define-validator` / `define-parser` によるルール拡張
- 失敗時は `validation-error` / `validation-parse-error` を送出

## インストール

ASDF からロードします。

```lisp
(asdf:load-system :list-validate)
```

## テスト実行

```bash
make test
```

## 基本例

### alist

```lisp
(list-validate:parse+validate-alist
    ((id :id (:parse :integer) (:validate :required :integer))
     (name "name" (:parse :string) (:validate :required (:max-length 32))))
  '((:id . "10") ("name" . "alice"))
  (list id name))
;; => (10 "alice")
```

### list

```lisp
(list-validate:parse-list
    ((head :integer)
     (tail (&rest :integer)))
  '("1" "2" "3")
  (list head tail))
;; => (1 (2 3))
```

### 単一値

```lisp
(list-validate:validate-value (id 10 :required :integer) id)
;; => 10

(list-validate:parse-value (id "10" :integer) id)
;; => 10

(list-validate:parse+validate-value
    (id "10" (:parse :integer) (:validate :required :integer))
  id)
;; => 10
```

## 主な公開インターフェース

- `validate-alist`
- `validate-plist`
- `validate-list`
- `validate-value`
- `parse-alist`
- `parse-plist`
- `parse-list`
- `parse-value`
- `parse+validate-alist`
- `parse+validate-plist`
- `parse+validate-list`
- `parse+validate-value`
- `define-validator`
- `define-parser`

## 定義済みルール

### Validator ルール

- `:required`: `nil` と空文字列を不許可
- `:pass`: 常に成功
- `:optional`: 値が `nil` の場合は後続の validator をスキップして成功
- `:integer`: 整数判定
- `:string`: 文字列判定
- `:number`: 数値判定
- `:float`: 浮動小数判定
- `:null`: `nil` 判定
- `:upper-string`: 全て大文字の文字列
- `:lower-string`: 全て小文字の文字列
- `(:or &rest rules)`: いずれかのルールが成功
- `(:and &rest rules)`: 全てのルールが成功
- `(:not rule)`: ルール結果を否定
- `(:min min-value)`: 数値が下限以上
- `(:max max-value)`: 数値が上限以下
- `(:between min max)`: 数値が範囲内
- `(:min-length n)`: `sequence` 長が `n` 以上
- `(:length n)`: 文字列長が `n` と一致
- `(:max-length n)`: `sequence` 長が `n` 以下
- `(:one-of values)`: 値が候補集合に含まれる
- `(:list &rest rules)`: リストの各要素を順に検証
- `(:alist &rest key-rule-pairs)`: alist のキー単位で検証
- `(:plist &rest key-rule-pairs)`: plist のキー単位で検証

### Parser ルール

- `:identity`: 値をそのまま返す
- `:drop`: 値を捨てて `nil` を返す
- `(:default default-value)`: 値が `nil` のときデフォルト値を使う
- `:integer`: 文字列を整数へ変換
- `:float`: 文字列を浮動小数へ変換
- `:number`: 文字列を整数または浮動小数へ変換
- `:boolean`: 真偽値相当の値へ変換
- `:keyword`: keyword シンボルへ変換
- `:symbol`: シンボルへ変換
- `:string`: 文字列化を試みる（`princ-to-string`）
- `:trim`: 前後空白を除去
- `:null-if-empty`: 空文字/空白文字列を `nil` に正規化
- `:upper-string`: 大文字化
- `:lower-string`: 小文字化
- `(:split sep)`: 文字列を区切りで分割
- `(:join sep)`: `sequence` を区切りで連結
- `(:or &rest rules)`: 最初に成功した parser を採用
  - 左から順に parser を試し、最初に成功した結果を返す
  - 全て失敗した場合は失敗
  - 例: `(:or :integer :string)` に `"10"` を渡すと `10` を返す
- `(:and &rest rules)`: 前から順に parser を適用
  - 左から順に parser を試し、全て成功したら結果を返す
  - いずれかに失敗した場合は失敗
  - 例: `(:and :integer :string)` に `"10"` を渡すと `"10"` を返す
- `(:not rule)`: 指定 parser が成功したら失敗、失敗したら元値で成功
  - `rule` が成功したら失敗
  - `rule` が失敗したら「元の値」で成功
  - 例: `(:not :string)` に `10` を渡すと成功して `10` を返す
  - 例: `(:not :string)` に `"abc"` を渡すと失敗
- `(:chain &rest rules)`: `:and` 同様に前から順に parser を適用
- `(:list &rest rules)`: `sequence` を `list` に変換し、 `rules` で要素単位でパース
- `(:map rule)`: `sequence` の全要素に同一 parser を適用

## ルールの簡単な使用例

### Validator 例

```lisp
;; :required
(list-validate:validate-value (v "x" :required) v) ; => "x"
;; :pass
(list-validate:validate-value (v nil :pass) v) ; => NIL
;; :optional
(list-validate:validate-value (v nil :optional :integer) v) ; => NIL
;; :integer
(list-validate:validate-value (v 10 :integer) v) ; => 10
;; :string
(list-validate:validate-value (v "abc" :string) v) ; => "abc"
;; :number
(list-validate:validate-value (v 1.5 :number) v) ; => 1.5
;; :float
(list-validate:validate-value (v 1.5 :float) v) ; => 1.5
;; :null
(list-validate:validate-value (v nil :null) v) ; => NIL
;; :upper-string
(list-validate:validate-value (v "ABC" :upper-string) v) ; => "ABC"
;; :lower-string
(list-validate:validate-value (v "abc" :lower-string) v) ; => "abc"
;; (:or ...)
(list-validate:validate-value (v 10 (:or :integer :string)) v) ; => 10
;; (:and ...)
(list-validate:validate-value (v 10 (:and :required :integer)) v) ; => 10
;; (:not ...)
(list-validate:validate-value (v "abc" (:not :integer)) v) ; => "abc"
;; (:min n)
(list-validate:validate-value (v 10 (:min 5)) v) ; => 10
;; (:max n)
(list-validate:validate-value (v 10 (:max 20)) v) ; => 10
;; (:between a b)
(list-validate:validate-value (v 10 (:between 1 20)) v) ; => 10
;; (:min-length n)
(list-validate:validate-value (v "abcd" (:min-length 3)) v) ; => "abcd"
;; (:length n)
(list-validate:validate-value (v "abcd" (:length 4)) v) ; => "abcd"
;; (:max-length n)
(list-validate:validate-value (v #(1 2 3) (:max-length 3)) v) ; => #(1 2 3)
;; (:one-of values)
(list-validate:validate-value (v :draft (:one-of (:draft :published))) v) ; => :DRAFT
;; (:list ...)
(list-validate:validate-value (v '(1 "a") (:list :integer :string)) v) ; => (1 "a")
;; (:alist ...)
(list-validate:validate-value (v '((:id . 10) ("name" . "alice"))
                                  (:alist :id :integer "name" :string))
  v) ; => ((:ID . 10) ("name" . "alice"))
;; (:plist ...)
(list-validate:validate-value (v '(:id 10 "name" "alice")
                                  (:plist :id :integer "name" :string))
  v) ; => (:ID 10 "name" "alice")
```

### Parser 例

```lisp
;; :identity
(list-validate:parse-value (v "x" :identity) v) ; => "x"
;; :drop
(list-validate:parse-value (v "x" :drop) v) ; => NIL
;; (:default x)
(list-validate:parse-value (v nil (:default "fallback")) v) ; => "fallback"
;; :integer
(list-validate:parse-value (v "10" :integer) v) ; => 10
;; :float
(list-validate:parse-value (v "1.5" :float) v) ; => 1.5
;; :number
(list-validate:parse-value (v "2.5" :number) v) ; => 2.5
;; :boolean
(list-validate:parse-value (v "true" :boolean) v) ; => T
;; :keyword
(list-validate:parse-value (v "name" :keyword) v) ; => :NAME
;; :symbol
(list-validate:parse-value (v "name" :symbol) v) ; => NAME
;; :string
(list-validate:parse-value (v 123 :string) v) ; => "123"
;; :trim
(list-validate:parse-value (v "  abc  " :trim) v) ; => "abc"
;; :null-if-empty
(list-validate:parse-value (v "   " :null-if-empty) v) ; => NIL
;; :upper-string
(list-validate:parse-value (v "abC" :upper-string) v) ; => "ABC"
;; :lower-string
(list-validate:parse-value (v "AbC" :lower-string) v) ; => "abc"
;; (:split sep)
(list-validate:parse-value (v "a,b,c" (:split ",")) v) ; => ("a" "b" "c")
;; (:join sep)
(list-validate:parse-value (v '("a" "b" "c") (:join "-")) v) ; => "a-b-c"
;; (:or ...)
(list-validate:parse-value (v "10" (:or :integer :string)) v) ; => 10
;; (:and ...)
(list-validate:parse-value (v "10" (:and :integer :string)) v) ; => "10"
;; (:chain ...)
(list-validate:parse-value (v " 10 " (:chain :trim :integer)) v) ; => 10
;; (:not ...)
(list-validate:parse-value (v 10 (:not :string)) v) ; => 10
;; (:list ...)
(list-validate:parse-value (v '("10" "a") (:list :integer :string)) v) ; => (10 "a")
;; (:map ...)
(list-validate:parse-value (v #("1" "2" "3") (:map :integer)) v) ; => (1 2 3)
```

## エラー条件

- `list-validate:validation-error`
  - reader: `validation-error-field`, `validation-error-value`, `validation-error-rules`
- `list-validate:validation-parse-error`
  - reader: `validation-parse-error-field`, `validation-parse-error-value`, `validation-parse-error-rules`

## ライセンス

`CC0-1.0`
