(defpackage list-validate/tests/validate
  (:use :cl
        :rove))
(in-package :list-validate/tests/validate)

(deftest validate-success
  (testing "validate が正常系で値を束縛する"
    (ok (equal
         (list-validate:validate-alist ((id :id :required :integer)
                                  (name "name" :required (:max-length 32)))
           '((:id . 10) ("name" . "alice"))
           (list id name))
         '(10 "alice")))))

(deftest validate-error
  (testing "validate が異常系で validation-error を送出する"
    (ok (handler-case
            (progn
              (list-validate:validate-alist ((id :id :required :integer))
                '((:id . "10"))
                id)
              nil)
          (list-validate:validation-error () t)))))

(deftest validate-without-rules
  (testing "validate はルール未指定時に pass する"
    (ok (equal
         (list-validate:validate-alist ((id :id))
           '((:id . 10))
           id)
         10))))

(deftest parse-success
  (testing "parse が文字列を整数に変換する"
    (ok (equal
         (list-validate:parse-alist ((id :id :integer)
                               (name "name" :string))
           '((:id . "10") ("name" . "alice"))
           (list id name))
         '(10 "alice")))))

(deftest parse-error
  (testing "parse が変換失敗時に validation-parse-error を送出する"
    (ok (handler-case
            (progn
              (list-validate:parse-alist ((id :id :integer))
                '((:id . "abc"))
                id)
              nil)
          (list-validate:validation-parse-error () t)))))

(deftest parse-without-rules
  (testing "parse はルール未指定時に :identity として動作する"
    (ok (equal
         (list-validate:parse-alist ((id :id))
           '((:id . "10"))
           id)
         "10"))))

(deftest parse+validate-success
  (testing "parse+validate が parse->validate を実行する"
    (ok (equal
         (list-validate:parse+validate-alist ((id :id (:parse :integer) (:validate :required :integer))
                                         (name "name" (:parse :string) (:validate :required (:max-length 32))))
           '((:id . "10") ("name" . "alice"))
           (list id name))
         '(10 "alice")))))

(deftest parse+validate-without-validate
  (testing ":validate 未指定でも pass する"
    (ok (equal
         (list-validate:parse+validate-alist ((id :id (:parse :integer))
                                         (name "name" (:parse :string)))
           '((:id . "10") ("name" . "alice"))
           (list id name))
         '(10 "alice")))))

(deftest parse+validate-validation-error
  (testing "parse+validate が validate 失敗で validation-error を送出する"
    (ok (handler-case
            (progn
              (list-validate:parse+validate-alist ((name "name" (:parse :string) (:validate (:max-length 3))))
                '(("name" . "alice"))
                name)
              nil)
          (list-validate:validation-error () t)))))

(deftest define-validator-detects-empty-arg-redefinition
  (testing "define-validator は0引数ルールの不整合再定義を検出する"
    (let ((tag (intern (format nil "TMP-VALIDATOR-~A" (gensym)) :keyword)))
      (eval `(list-validate:define-validator ,tag (v) t))
      (ok (handler-case
              (progn
                (eval `(list-validate:define-validator (,tag min-value) (v)
                         (declare (ignore min-value))
                         t))
                nil)
            (error () t))))))

(deftest define-validator-around-qualifier
  (testing "define-validator は :around 修飾子を受け付ける"
    (let ((tag (intern (format nil "TMP-AROUND-VALIDATOR-~A" (gensym)) :keyword)))
      (eval `(list-validate:define-validator ,tag (v) (integerp v)))
      (eval `(list-validate:define-validator ,tag :around (v)
               (and (numberp v) (next-validator))))
      (ok (equal
           (eval `(list-validate:validate-alist ((v :v ,tag))
                    '((:v . 10))
                  v))
           10))
      (ok (handler-case
              (progn
                (eval `(list-validate:validate-alist ((v :v ,tag))
                         '((:v . "10"))
                       v))
                nil)
            (list-validate:validation-error () t))))))

(deftest define-parser-around-qualifier
  (testing "define-parser は :around 修飾子と next-parser を受け付ける"
    (let ((tag (intern (format nil "TMP-AROUND-PARSER-~A" (gensym)) :keyword)))
      (eval `(list-validate:define-parser ,tag (v)
               (if (integerp v)
                   (values t v)
                   (values nil nil))))
      (eval `(list-validate:define-parser ,tag :around (v)
               (if (numberp v)
                   (next-parser)
                   (values nil nil))))
      (ok (equal
           (eval `(list-validate:parse-alist ((v :v ,tag))
                    '((:v . 10))
                  v))
           10))
      (ok (handler-case
              (progn
                (eval `(list-validate:parse-alist ((v :v ,tag))
                         '((:v . "10"))
                       v))
                nil)
            (list-validate:validation-parse-error () t))))))

(deftest define-parser-detects-empty-arg-redefinition
  (testing "define-parser は0引数ルールの不整合再定義を検出する"
    (let ((tag (intern (format nil "TMP-PARSER-~A" (gensym)) :keyword)))
      (eval `(list-validate:define-parser ,tag (v) (values t v)))
      (ok (handler-case
              (progn
                (eval `(list-validate:define-parser (,tag base) (v)
                         (declare (ignore base))
                         (values t v)))
                nil)
            (error () t))))))

(deftest validate-alist-prefers-first-duplicate
  (testing "validate-alist は重複キー時に先頭要素を使う"
    (ok (equal
         (list-validate:validate-alist ((id :id :integer))
           '((:id . 10) (:id . "20"))
           id)
         10))))

(deftest parse-alist-prefers-first-duplicate
  (testing "parse-alist は重複キー時に先頭要素を使う"
    (ok (equal
         (list-validate:parse-alist ((id :id :integer))
           '((:id . "10") (:id . "abc"))
           id)
         10))))

(deftest validate-plist-prefers-first-duplicate
  (testing "validate-plist は重複キー時に先頭要素を使う"
    (ok (equal
         (list-validate:validate-plist ((id :id :integer))
           '(:id 10 :id "20")
           id)
         10))))

(deftest parse-plist-prefers-first-duplicate
  (testing "parse-plist は重複キー時に先頭要素を使う"
    (ok (equal
         (list-validate:parse-plist ((id :id :integer))
           '(:id "10" :id "abc")
           id)
         10))))

(deftest parse+validate-plist-success
  (testing "parse+validate-plist が parse->validate を実行する"
    (ok (equal
         (list-validate:parse+validate-plist ((id :id (:parse :integer) (:validate :integer)))
           '(:id "10")
           id)
         10))))

(deftest validate-upper-lower-string-rules
  (testing "upper-string/lower-string validator が大文字小文字を検証する"
    (ok (equal
         (list-validate:validate-alist ((code :code :upper-string)
                                   (name :name :lower-string))
           '((:code . "ABC") (:name . "alice"))
           (list code name))
         '("ABC" "alice")))
    (ok (handler-case
            (progn
              (list-validate:validate-alist ((code :code :upper-string))
                '((:code . "AbC"))
                code)
              nil)
          (list-validate:validation-error () t)))))

(deftest parse-upper-lower-string-rules
  (testing "upper-string/lower-string parser が文字列を変換する"
    (ok (equal
         (list-validate:parse-alist ((code :code :upper-string)
                                (name :name :lower-string))
           '((:code . "aBc") (:name . "ALIce"))
           (list code name))
         '("ABC" "alice")))
    (ok (handler-case
            (progn
              (list-validate:parse-alist ((code :code :upper-string))
                '((:code . 123))
                code)
              nil)
              (list-validate:validation-parse-error () t)))))

(deftest validate-list-by-definition-order
  (testing "validate-list は定義順で要素を束縛する"
    (ok (equal
         (list-validate:validate-list ((id :integer)
                                 (name))
           '(10 "alice")
           (list id name))
         '(10 "alice")))))

(deftest parse-list-by-definition-order
  (testing "parse-list は定義順で要素をパースする"
    (ok (equal
         (list-validate:parse-list ((id :integer)
                              (name :string))
           '("10" "alice")
           (list id name))
         '(10 "alice")))))

(deftest parse+validate-list-by-definition-order
  (testing "parse+validate-list は定義順で parse->validate を実行する"
    (ok (equal
         (list-validate:parse+validate-list ((id (:parse :integer) (:validate :required :integer))
                                        (name (:parse :string) (:validate :required (:max-length 32))))
           '("10" "alice")
           (list id name))
         '(10 "alice")))))

(deftest validate-list-rest-pattern
  (testing "validate-list は &rest パターンを検証して束縛する"
    (ok (equal
         (list-validate:validate-list ((head :integer)
                                  (tail (&rest :integer)))
           '(1 2 3)
           (list head tail))
         '(1 (2 3))))
    (ok (handler-case
            (progn
              (list-validate:validate-list ((head :integer)
                                       (tail (&rest :integer)))
                '(1 2 "x")
                (list head tail))
              nil)
          (list-validate:validation-error () t)))))

(deftest parse-list-rest-pattern
  (testing "parse-list は &rest パターンを順番にパースする"
    (ok (equal
         (list-validate:parse-list ((head :integer)
                               (tail (&rest :integer)))
           '("1" "2" "3")
           (list head tail))
         '(1 (2 3))))
    (ok (handler-case
            (progn
              (list-validate:parse-list ((head :integer)
                                    (tail (&rest :integer)))
                '("1" "x")
                (list head tail))
              nil)
          (list-validate:validation-parse-error () t)))))

(deftest parse+validate-list-rest-pattern
  (testing "parse+validate-list は &rest パターンで parse->validate を実行する"
    (ok (equal
         (list-validate:parse+validate-list ((head (:parse :integer) (:validate :integer))
                                        (tail (&rest (:parse :integer) (:validate :integer))))
           '("1" "2" "3")
           (list head tail))
         '(1 (2 3))))
    (ok (handler-case
            (progn
              (list-validate:parse+validate-list ((head (:parse :integer) (:validate :integer))
                                             (tail (&rest (:parse :integer) (:validate :integer))))
                '("1" "2" "x")
                (list head tail))
              nil)
          (list-validate:validation-parse-error () t)))))

(deftest list-rest-pattern-shape-errors
  (testing "&rest の定義が不正な場合はエラーになる"
    (ok (handler-case
            (progn
              (macroexpand-1
               '(list-validate:validate-list ((a :integer)
                                         (b (&rest :integer))
                                         (c (&rest :string)))
                  '(1 2 3)
                  (list a b c)))
              nil)
          (error () t)))
    (ok (handler-case
            (progn
              (macroexpand-1
               '(list-validate:parse-list ((a (&rest :integer))
                                      (b :integer))
                  '(1 2 3)
                  (list a b)))
              nil)
          (error () t)))))

(deftest list-rules-handle-short-and-long-input
  (testing "list系APIは要素不足をnil、余剰要素を無視する"
    (ok (equal
         (list-validate:parse-list ((id :integer)
                              (name))
           '("10")
           (list id name))
         '(10 nil)))
    (ok (equal
         (list-validate:validate-list ((id :integer))
           '(10 "ignored")
           id)
         10))))

(deftest validation-error-readers
  (testing "validation-error のreaderで各フィールドを取得できる"
    (handler-case
        (progn
          (list-validate:validate-alist ((id :id :integer))
            '((:id . "x"))
            id)
          (fail "validation-error が送出されるべき"))
      (list-validate:validation-error (e)
        (ok (equal :id (list-validate:validation-error-field e)))
        (ok (equal "x" (list-validate:validation-error-value e)))
        (ok (equal '(:integer) (list-validate:validation-error-rules e)))))))

(deftest validation-parse-error-readers
  (testing "validation-parse-error のreaderで各フィールドを取得できる"
    (handler-case
        (progn
          (list-validate:parse-alist ((id :id :integer))
            '((:id . "x"))
            id)
          (fail "validation-parse-error が送出されるべき"))
      (list-validate:validation-parse-error (e)
        (ok (equal :id (list-validate:validation-parse-error-field e)))
        (ok (equal "x" (list-validate:validation-parse-error-value e)))
        (ok (equal '(:integer) (list-validate:validation-parse-error-rules e)))))))

(deftest validator-or-not-rules
  (testing "validator :or/:not ルールが動作する"
    (ok (equal
         (list-validate:validate-alist ((v :v (:or :integer (:max-length 3))))
           '((:v . 10))
           v)
         10))
    (ok (equal
         (list-validate:validate-alist ((v :v (:not :integer)))
           '((:v . "abc"))
           v)
         "abc"))
    (ok (handler-case
            (progn
              (list-validate:validate-alist ((v :v (:or :integer (:max-length 3))))
                '((:v . "abcd"))
                v)
              nil)
          (list-validate:validation-error () t)))))

(deftest validator-and-rules
  (testing "validator :and ルールが動作する"
    (ok (equal
         (list-validate:validate-alist ((v :v (:and :required :integer)))
           '((:v . 10))
           v)
         10))
    (ok (handler-case
            (progn
              (list-validate:validate-alist ((v :v (:and :required :integer)))
                '((:v . "10"))
                v)
              nil)
          (list-validate:validation-error () t)))))

(deftest validator-string-number-float-rules
  (testing "validator :string/:number/:float ルールが動作する"
    (ok (equal
         (list-validate:validate-alist ((s :s :string)
                                        (n :n :number)
                                        (f :f :float))
           '((:s . "abc") (:n . 10) (:f . 1.5))
           (list s n f))
         '("abc" 10 1.5)))
    (ok (handler-case
            (progn
              (list-validate:validate-alist ((f :f :float))
                '((:f . 1))
                f)
              nil)
          (list-validate:validation-error () t)))))

(deftest validator-min-max-between-rules
  (testing "validator :min/:max/:between ルールが動作する"
    (ok (equal
         (list-validate:validate-alist ((v :v (:min 1) (:max 10) (:between 1 10)))
           '((:v . 5))
           v)
         5))
    (ok (handler-case
            (progn
              (list-validate:validate-alist ((v :v (:between 1 10)))
                '((:v . 11))
                v)
              nil)
          (list-validate:validation-error () t)))))

(deftest validator-length-rules
  (testing "validator :min-length/:length/:max-length ルールが動作する"
    (ok (equal
         (list-validate:validate-alist ((v :v (:min-length 2) (:length 3) (:max-length 4)))
           '((:v . "abc"))
           v)
         "abc"))
    (ok (handler-case
            (progn
              (list-validate:validate-alist ((v :v (:length 3)))
                '((:v . "ab"))
                v)
              nil)
          (list-validate:validation-error () t)))))

(deftest validator-one-of-rule
  (testing "validator :one-of ルールが動作する"
    (ok (equal
         (list-validate:validate-alist ((v :v (:one-of ("draft" "published"))))
           '((:v . "draft"))
           v)
         "draft"))
    (ok (handler-case
            (progn
              (list-validate:validate-alist ((v :v (:one-of ("draft" "published"))))
                '((:v . "archived"))
                v)
              nil)
          (list-validate:validation-error () t)))))

(deftest parser-or-not-rules
  (testing "parser :or/:not ルールが動作する"
    (ok (equal
         (list-validate:parse-alist ((v :v (:or :integer :string)))
           '((:v . "10"))
           v)
         10))
    (ok (equal
         (list-validate:parse-alist ((v :v (:not :string)))
           '((:v . 10))
           v)
         10))
    (ok (handler-case
            (progn
              (list-validate:parse-alist ((v :v (:not :string)))
                '((:v . "abc"))
                v)
              nil)
              (list-validate:validation-parse-error () t)))))

(deftest optional-rule-order
  (testing ":optional は前方指定時のみ後続ルールをスキップできる"
    (ok (equal
         (list-validate:validate-alist ((id :id :optional :integer))
           '((:id . nil))
           id)
         nil))
    (ok (handler-case
            (progn
              (list-validate:validate-alist ((id :id :integer :optional))
                '((:id . nil))
                id)
              nil)
          (list-validate:validation-error () t)))))

(deftest default-rule-order
  (testing "(:default ...) は前方指定時のみ後続パーサーに値を渡せる"
    (ok (equal
         (list-validate:parse-alist ((id :id (:default "10") :integer))
           '()
           id)
         10))
    (ok (handler-case
            (progn
              (list-validate:parse-alist ((id :id :integer (:default "10")))
                '()
                id)
              nil)
              (list-validate:validation-parse-error () t)))))

(deftest null-validator-rule
  (testing ":null は nil のみを許容する"
    (ok (equal
         (list-validate:validate-alist ((v :v :null))
           '((:v . nil))
           v)
         nil))
    (ok (handler-case
            (progn
              (list-validate:validate-alist ((v :v :null))
                '((:v . 1))
                v)
              nil)
          (list-validate:validation-error () t)))))

(deftest drop-parser-rule
  (testing ":drop は値を捨てて nil を返す"
    (ok (equal
         (list-validate:parse-alist ((v :v :drop))
           '((:v . "anything"))
           v)
         nil))
    (ok (handler-case
            (progn
              (list-validate:parse-alist ((v :v :drop :integer))
                '((:v . "10"))
                v)
              nil)
          (list-validate:validation-parse-error () t)))))
