(in-package #:list-validate)

(define-condition validation-error (simple-error)
  ((field :initarg :field
          :reader validation-error-field)
   (value :initarg :value
          :reader validation-error-value)
   (rules :initarg :rules
          :reader validation-error-rules))
  (:report (lambda (c s)
             (format s "Validation Error: field = ~S, value = ~S, rules = ~S"
                     (validation-error-field c)
                     (validation-error-value c)
                     (validation-error-rules c)))))

(define-condition validation-parse-error (simple-error)
  ((field :initarg :field
          :reader validation-parse-error-field)
   (value :initarg :value
          :reader validation-parse-error-value)
   (rules :initarg :rules
          :reader validation-parse-error-rules))
  (:report (lambda (c s)
             (format s "Parse Error: field = ~S, value = ~S, rules = ~S"
                     (validation-parse-error-field c)
                     (validation-parse-error-value c)
                     (validation-parse-error-rules c)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun rule-tag (rule)
    (if (consp rule)
        (car rule)
        rule)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun rule-args (rule)
    (and (consp rule)
         (cdr rule))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse+validate-rules (vkr)
    (let ((sections (cddr vkr))
          (parse-rules nil)
          (validate-rules nil))
      (dolist (section sections)
        (unless (and (consp section)
                     (symbolp (car section)))
          (error "parse+validate section must be a list: ~S" section))
        (ecase (car section)
          (:parse
           (setf parse-rules (append parse-rules (cdr section))))
          (:validate
           (setf validate-rules (append validate-rules (cdr section))))))
      (values (if parse-rules parse-rules '(:identity))
              validate-rules))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse+validate-list-rules (vr)
    (let ((sections (cdr vr))
          (parse-rules nil)
          (validate-rules nil))
      (dolist (section sections)
        (unless (and (consp section)
                     (symbolp (car section)))
          (error "parse+validate section must be a list: ~S" section))
        (ecase (car section)
          (:parse
           (setf parse-rules (append parse-rules (cdr section))))
          (:validate
           (setf validate-rules (append validate-rules (cdr section))))))
      (values (if parse-rules parse-rules '(:identity))
              validate-rules))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun list-rest-entry-p (vr)
    (and (consp vr)
         (consp (cdr vr))
         (consp (cadr vr))
         (eq (caadr vr) '&rest)))

  (defun list-entry-variable (vr)
    (car vr))

  (defun list-entry-rules (vr)
    (cdr vr))

  (defun list-rest-entry-rules (vr)
    (let ((rest-spec (cadr vr)))
      (unless (null (cddr vr))
        (error "&rest entry must be in the form (var (&rest ...)): ~S" vr))
      (cdr rest-spec)))

  (defun normalize-rules-with-default (rules default-rules)
    (if rules rules default-rules))

  (defun build-alist-basic-configs (var-key-rules default-rules gensym-prefix)
    (mapcar (lambda (vkr)
              (let ((rules (cddr vkr)))
                (list (car vkr)
                      (cadr vkr)
                      (normalize-rules-with-default rules default-rules)
                      (gensym gensym-prefix))))
            var-key-rules))

  (defun build-basic-processor-bindings (configs sym-index rules-index builder-fn)
    (mapcar (lambda (cfg)
              `(,(nth sym-index cfg)
                (load-time-value (,builder-fn ',(nth rules-index cfg)) t)))
            configs))

  (defun build-basic-processor-bindings-with-rest
      (configs rest-config sym-index rules-index builder-fn)
    (append
     (build-basic-processor-bindings configs sym-index rules-index builder-fn)
     (when rest-config
       (list
        `(,(nth sym-index rest-config)
          (load-time-value (,builder-fn ',(nth rules-index rest-config)) t))))))

  (defun build-list-basic-configs
      (normal-rules rest-entry default-rules gensym-prefix rest-gensym-prefix)
    (let* ((index -1)
           (configs (mapcar (lambda (vr)
                              (incf index)
                              (let ((rules (list-entry-rules vr)))
                                (list (list-entry-variable vr)
                                      index
                                      (normalize-rules-with-default
                                       rules
                                       default-rules)
                                      (gensym gensym-prefix))))
                            normal-rules))
           (rest-start (length normal-rules))
           (rest-config (when rest-entry
                          (let ((rules (list-rest-entry-rules rest-entry)))
                            (list (list-entry-variable rest-entry)
                                  rest-start
                                  (normalize-rules-with-default
                                   rules
                                   default-rules)
                                  (gensym rest-gensym-prefix))))))
      (values configs rest-config)))

  (defun split-list-rules-with-rest (var-rules)
    (let* ((rest-entries (remove-if-not #'list-rest-entry-p var-rules))
           (rest-count (length rest-entries)))
      (when (> rest-count 1)
        (error "Only one &rest entry is allowed: ~S" var-rules))
      (if (zerop rest-count)
          (values var-rules nil)
          (let ((rest-entry (car rest-entries)))
            (unless (eq rest-entry (car (last var-rules)))
              (error "&rest entry must be the last rule: ~S" var-rules))
            (values (butlast var-rules) rest-entry)))))

  (defun build-validate-list-configs (normal-rules rest-entry)
    (build-list-basic-configs normal-rules
                              rest-entry
                              '(:pass)
                              "VALIDATOR-"
                              "REST-VALIDATOR-"))

  (defun build-validate-list-value-bindings (values-sym configs rest-config)
    (append
     (mapcar (lambda (cfg)
               `(,(first cfg)
                 (%validate-value
                     (value
                      (nth ,(second cfg) ,values-sym)
                      ,(second cfg)
                      ,@(third cfg))
                   value)))
             configs)
     (when rest-config
       (list
        `(,(first rest-config)
          (let ((values (nthcdr ,(second rest-config) ,values-sym))
                (index ,(second rest-config)))
            (dolist (value values values)
              (%validate-value
                  (current value index ,@(third rest-config))
                current)
              (incf index))))))))

  (defun build-parse+validate-list-configs (normal-rules rest-entry)
    (let* ((index -1)
           (configs (mapcar (lambda (vr)
                              (incf index)
                              (multiple-value-bind (parse-rules validate-rules)
                                  (parse+validate-list-rules vr)
                                (list (list-entry-variable vr)
                                      index
                                      parse-rules
                                      validate-rules
                                      (gensym "PARSER-")
                                      (gensym "VALIDATOR-"))))
                            normal-rules))
           (rest-start (length normal-rules))
           (rest-config (when rest-entry
                          (multiple-value-bind (parse-rules validate-rules)
                              (parse+validate-list-rules
                               (cons (list-entry-variable rest-entry)
                                     (list-rest-entry-rules rest-entry)))
                            (list (list-entry-variable rest-entry)
                                  rest-start
                                  parse-rules
                                  validate-rules
                                  (gensym "REST-PARSER-")
                                  (gensym "REST-VALIDATOR-"))))))
      (values configs rest-config)))

  (defun build-parse+validate-list-parser-bindings (configs rest-config)
    (append
     (mapcar (lambda (cfg)
               `(,(fifth cfg)
                 (load-time-value (build-parser ',(third cfg)) t)))
             configs)
     (when rest-config
       (list
        `(,(fifth rest-config)
          (load-time-value (build-parser ',(third rest-config)) t))))))

  (defun build-parse+validate-list-validator-bindings (configs rest-config)
    (append
     (mapcar (lambda (cfg)
               `(,(sixth cfg)
                 ,(if (fourth cfg)
                      `(load-time-value (build-validator ',(fourth cfg)) t)
                      nil)))
             configs)
     (when rest-config
       (list
        `(,(sixth rest-config)
          ,(if (fourth rest-config)
               `(load-time-value (build-validator ',(fourth rest-config)) t)
               nil))))))

  (defun build-parse+validate-list-value-bindings (values-sym configs rest-config)
    (append
     (mapcar
      (lambda (cfg)
        `(,(first cfg)
          (let ((value (nth ,(second cfg) ,values-sym)))
            (multiple-value-bind (ok parsed)
                (funcall ,(fifth cfg) value)
              (unless ok
                (error 'validation-parse-error
                       :field ,(second cfg)
                       :value value
                       :rules ',(third cfg)))
              (when ,(sixth cfg)
                (unless (funcall ,(sixth cfg) parsed)
                  (error 'validation-error
                         :field ,(second cfg)
                         :value parsed
                         :rules ',(fourth cfg))))
              parsed))))
      configs)
     (when rest-config
       (list
        `(,(first rest-config)
          (let ((values (nthcdr ,(second rest-config) ,values-sym)))
            (loop for value in values
                  for index from ,(second rest-config)
                  collect
                  (multiple-value-bind (ok parsed)
                      (funcall ,(fifth rest-config) value)
                    (unless ok
                      (error 'validation-parse-error
                             :field index
                             :value value
                             :rules ',(third rest-config)))
                    (when ,(sixth rest-config)
                      (unless (funcall ,(sixth rest-config) parsed)
                        (error 'validation-error
                               :field index
                               :value parsed
                               :rules ',(fourth rest-config))))
                    parsed))))))))

  (defun build-validate-alist-configs (var-key-rules)
    (build-alist-basic-configs var-key-rules '(:pass) "VALIDATOR-"))

  (defun build-validate-alist-value-bindings (alist configs)
    (mapcar (lambda (cfg)
              `(,(first cfg)
                (%validate-value
                    (value
                     (cdr (assoc ',(second cfg)
                                 ,alist
                                 :test 'equal))
                     ',(second cfg)
                     ,@(third cfg))
                  value)))
            configs))

  (defun build-parse-alist-configs (var-key-rules)
    (build-alist-basic-configs var-key-rules '(:identity) "PARSER-"))

  (defun build-parse-alist-parser-bindings (configs)
    (build-basic-processor-bindings configs 3 2 'build-parser))

  (defun build-parse-alist-value-bindings (sym ok parsed alist configs)
    (mapcar
     (lambda (cfg)
       `(,(first cfg)
         (let ((,sym (cdr (assoc ',(second cfg)
                                 ,alist
                                 :test 'equal))))
           (multiple-value-bind (,ok ,parsed)
               (funcall ,(fourth cfg) ,sym)
             (unless ,ok
               (error 'validation-parse-error
                      :field ',(second cfg)
                      :value ,sym
                      :rules ',(third cfg)))
             ,parsed))))
     configs))

  (defun build-parse+validate-alist-configs (var-key-rules)
    (mapcar
     (lambda (vkr)
       (multiple-value-bind (parse-rules validate-rules)
           (parse+validate-rules vkr)
         (list (car vkr)
               (cadr vkr)
               parse-rules
               validate-rules
               (gensym "PARSER-")
               (gensym "VALIDATOR-"))))
     var-key-rules))

  (defun build-parse+validate-alist-parser-bindings (configs)
    (build-basic-processor-bindings configs 4 2 'build-parser))

  (defun build-parse+validate-alist-validator-bindings (configs)
    (mapcar (lambda (cfg)
              `(,(sixth cfg)
                ,(if (fourth cfg)
                     `(load-time-value (build-validator ',(fourth cfg)) t)
                     nil)))
            configs))

  (defun build-parse+validate-alist-value-bindings (sym ok parsed alist configs)
    (mapcar
     (lambda (cfg)
       `(,(first cfg)
         (let ((,sym (cdr (assoc ',(second cfg)
                                 ,alist
                                 :test 'equal))))
           (multiple-value-bind (,ok ,parsed)
               (funcall ,(fifth cfg) ,sym)
             (unless ,ok
               (error 'validation-parse-error
                      :field ',(second cfg)
                      :value ,sym
                      :rules ',(third cfg)))
             (when ,(sixth cfg)
               (unless (funcall ,(sixth cfg) ,parsed)
                 (error 'validation-error
                        :field ',(second cfg)
                        :value ,parsed
                        :rules ',(fourth cfg))))
             ,parsed))))
     configs))

  (defun build-parse-list-configs (normal-rules rest-entry)
    (build-list-basic-configs normal-rules
                              rest-entry
                              '(:identity)
                              "PARSER-"
                              "REST-PARSER-"))

  (defun build-parse-list-parser-bindings (configs rest-config)
    (build-basic-processor-bindings-with-rest
     configs
     rest-config
     3
     2
     'build-parser))

  (defun build-parse-list-value-bindings (values-sym configs rest-config)
    (append
     (mapcar (lambda (cfg)
               `(,(first cfg)
                 (let ((value (nth ,(second cfg) ,values-sym)))
                   (multiple-value-bind (ok parsed)
                       (funcall ,(fourth cfg) value)
                     (unless ok
                       (error 'validation-parse-error
                              :field ,(second cfg)
                              :value value
                              :rules ',(third cfg)))
                     parsed))))
             configs)
     (when rest-config
       (list
        `(,(first rest-config)
          (let ((values (nthcdr ,(second rest-config) ,values-sym)))
            (loop for value in values
                  for index from ,(second rest-config)
                  collect
                  (multiple-value-bind (ok parsed)
                      (funcall ,(fourth rest-config) value)
                    (unless ok
                      (error 'validation-parse-error
                             :field index
                             :value value
                             :rules ',(third rest-config)))
                    parsed))))))))
  )

(defclass <validator-rule> () ())
(defclass <parser-rule> () ())

(defgeneric apply-validator-rule (rule value))
(defgeneric apply-parser-rule (rule value))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *validate-rule-class-table* (make-hash-table :test 'eq))
  (defparameter *parse-rule-class-table* (make-hash-table :test 'eq))
  (defparameter *validate-rule-args-table* (make-hash-table :test 'eq))
  (defparameter *parse-rule-args-table* (make-hash-table :test 'eq))
  (defparameter *validate-rule-arg-style-table* (make-hash-table :test 'eq))
  (defparameter *parse-rule-arg-style-table* (make-hash-table :test 'eq)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symbol->keyword (sym)
    (intern (string-upcase (symbol-name sym)) :keyword)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-rule-class-symbol (tag suffix)
    (intern (format nil "~A~A"
                    tag
                    suffix)
            (find-package :list-validate.internal))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun normalize-method-qualifier (qualifier)
    (case qualifier
      ((nil) nil)
      ((:around around) :around)
      ((:before before) :before)
      ((:after after) :after)
      (t (error "Unsupported method qualifier: ~S" qualifier)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-rule-method-definition (args)
    (let ((head (car args))
          (second (cadr args)))
      (cond
        ((listp head)
         (values nil head (cdr args)))
        ((and second
              (member head '(:around :before :after around before after) :test #'eq)
              (listp second))
         (values (normalize-method-qualifier head) second (cddr args)))
        (t
         (error "Invalid rule method definition: ~S" args))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-rule-signature (rule)
    (cond
      ((symbolp rule)
       (values rule nil :fixed))
      ((and (consp rule)
            (symbolp (car rule)))
       (let ((tag (car rule))
             (signature (cdr rule)))
         (cond
           ((null signature)
            (values tag nil :fixed))
           ((and (= (length signature) 2)
                 (eq (first signature) '&rest)
                 (symbolp (second signature)))
            (values tag (list (second signature)) :rest-into-single-slot))
           ((member '&rest signature :test #'eq)
            (error "Only (:tag &rest var) is supported: ~S" rule))
           (t
            (values tag signature :fixed)))))
      (t
       (error "Invalid rule signature: ~S" rule)))))

(defun make-rule-instance (rule class-table args-table arg-style-table kind)
  (let* ((tag (rule-tag rule))
         (args (rule-args rule))
         (class (gethash tag class-table nil))
         (arg-names (gethash tag args-table nil))
         (arg-style (gethash tag arg-style-table :fixed))
         (normalized-args
           (if (eq arg-style :rest-into-single-slot)
               (if (and (= (length args) 1)
                        (listp (car args)))
                   (list (car args))
                   (list args))
               args)))
    (when (null class)
      (error "Undefined ~A rule: ~S" kind rule))
    (unless (= (length normalized-args) (length arg-names))
      (error "Invalid ~A rule args: ~S (expected ~D, got ~D)"
             kind
             rule
             (length arg-names)
             (length normalized-args)))
    (apply #'make-instance
           class
           (loop for arg-name in arg-names
                 for arg-value in normalized-args
                 append (list (symbol->keyword arg-name)
                              arg-value)))))

(defun make-validator-rule-instance (rule)
  (make-rule-instance rule
                      *validate-rule-class-table*
                      *validate-rule-args-table*
                      *validate-rule-arg-style-table*
                      "validate"))

(defun make-parser-rule-instance (rule)
  (make-rule-instance rule
                      *parse-rule-class-table*
                      *parse-rule-args-table*
                      *parse-rule-arg-style-table*
                      "parse"))

(defmacro define-validator (rule &rest method-definition)
  "バリデーションルールを定義して登録する。

使用例:
(define-validator :required (val)
  (not (or (null val)
           (and (stringp val)
                (zerop (length val))))))

(define-validator (:min min-value) (val)
  (and (numberp val)
       (<= min-value val)))

(define-validator :integer :around (val)
  (if (null val)
      nil
      (next-validator)))

(define-validator (:and &rest rules) (val validate)
  (every #'(lambda (rule)
             (funcall validate rule val))
         rules))"
  (multiple-value-bind (qualifier lambda-list body)
      (parse-rule-method-definition method-definition)
    (unless (and (listp lambda-list)
                 (<= 1 (length lambda-list) 2))
      (error "define-validator lambda list must contain one or two variables: ~S"
             lambda-list))
    (multiple-value-bind (tag args arg-style)
        (parse-rule-signature rule)
      (let* ((arg (first lambda-list))
             (resolver-arg (or (second lambda-list)
                               (gensym "VALIDATE-")))
             (class-name (make-rule-class-symbol tag "-VALIDATOR-RULE"))
             (next-fn-sym (intern "NEXT-VALIDATOR" *package*))
             (slot-defs (mapcar #'(lambda (name)
                                    `(,name :initarg ,(symbol->keyword name)))
                                args))
             (missing-args (gensym "MISSING-VALIDATOR-RULE-ARGS-"))
             (missing-style (gensym "MISSING-VALIDATOR-RULE-ARG-STYLE-")))
        `(progn
           (eval-when (:compile-toplevel :load-toplevel :execute)
             (let ((,missing-args (gensym "MISSING-VALIDATOR-RULE-ARGS-"))
                   (,missing-style (gensym "MISSING-VALIDATOR-RULE-ARG-STYLE-")))
               (let ((known-args (gethash ',tag *validate-rule-args-table* ,missing-args))
                     (known-arg-style (gethash ',tag
                                               *validate-rule-arg-style-table*
                                               ,missing-style)))
                 (when (and (not (eq known-args ,missing-args))
                            (not (equal known-args ',args)))
                   (error "Validator rule ~S already defined with different args: ~S"
                          ',tag
                          known-args))
                 (when (and (not (eq known-arg-style ,missing-style))
                            (not (eq known-arg-style ',arg-style)))
                   (error "Validator rule ~S already defined with different arg style: ~S"
                          ',tag
                          known-arg-style)))
               (unless (find-class ',class-name nil)
                 (defclass ,class-name (<validator-rule>)
                   ,slot-defs))
               (setf (gethash ',tag *validate-rule-class-table*) ',class-name)
               (setf (gethash ',tag *validate-rule-args-table*) ',args)
               (setf (gethash ',tag *validate-rule-arg-style-table*) ',arg-style)))
           (defmethod apply-validator-rule
               ,@(when qualifier (list qualifier))
               ((rule ,class-name) ,arg)
             (declare (ignorable ,arg))
             (flet ((,next-fn-sym ()
                      (call-next-method)))
               (declare (ignorable #',next-fn-sym))
               (let ((,resolver-arg
                       #'(lambda (sub-rule sub-value)
                           (funcall (get-validator1 sub-rule) sub-value))))
                 (declare (ignorable ,resolver-arg))
                  ,(if args
                      `(block ,tag
                         (with-slots ,args rule
                           (declare (ignorable ,@args))
                           ,@body))
                      `(block ,tag
                         ,@body))))))))))

(defmacro define-parser (rule &rest method-definition)
  "パースルールを定義して登録する。

使用例:
(define-parser :identity (val)
  (values t val))

(define-parser :integer (val)
  (typecase val
    (integer (values t val))
    (string
     (let ((parsed (ignore-errors (parse-integer val :junk-allowed nil))))
       (if parsed
           (values t parsed)
           (values nil nil))))
    (t (values nil nil))))

(define-parser :integer :around (val)
  (if (numberp val)
      (next-parser)
      (values nil nil)))

(define-parser (:or &rest rules) (val parse)
  (dolist (rule rules (values nil nil))
    (multiple-value-bind (ok parsed)
        (funcall parse rule val)
      (when ok
        (return (values t parsed))))))"
  (multiple-value-bind (qualifier lambda-list body)
      (parse-rule-method-definition method-definition)
    (unless (and (listp lambda-list)
                 (<= 1 (length lambda-list) 2))
      (error "define-parser lambda list must contain one or two variables: ~S"
             lambda-list))
    (multiple-value-bind (tag args arg-style)
        (parse-rule-signature rule)
      (let* ((arg (first lambda-list))
             (resolver-arg (or (second lambda-list)
                               (gensym "PARSE-")))
             (class-name (make-rule-class-symbol tag "-PARSER-RULE"))
             (next-fn-sym (intern "NEXT-PARSER" *package*))
             (slot-defs (mapcar #'(lambda (name)
                                    `(,name :initarg ,(symbol->keyword name)))
                                args))
             (missing-args (gensym "MISSING-PARSER-RULE-ARGS-"))
             (missing-style (gensym "MISSING-PARSER-RULE-ARG-STYLE-")))
        `(progn
           (eval-when (:compile-toplevel :load-toplevel :execute)
             (let ((,missing-args (gensym "MISSING-PARSER-RULE-ARGS-"))
                   (,missing-style (gensym "MISSING-PARSER-RULE-ARG-STYLE-")))
               (let ((known-args (gethash ',tag *parse-rule-args-table* ,missing-args))
                     (known-arg-style (gethash ',tag
                                               *parse-rule-arg-style-table*
                                               ,missing-style)))
                 (when (and (not (eq known-args ,missing-args))
                            (not (equal known-args ',args)))
                   (error "Parser rule ~S already defined with different args: ~S"
                          ',tag
                          known-args))
                 (when (and (not (eq known-arg-style ,missing-style))
                            (not (eq known-arg-style ',arg-style)))
                   (error "Parser rule ~S already defined with different arg style: ~S"
                          ',tag
                          known-arg-style)))
               (unless (find-class ',class-name nil)
                 (defclass ,class-name (<parser-rule>)
                   ,slot-defs))
               (setf (gethash ',tag *parse-rule-class-table*) ',class-name)
               (setf (gethash ',tag *parse-rule-args-table*) ',args)
               (setf (gethash ',tag *parse-rule-arg-style-table*) ',arg-style)))
           (defmethod apply-parser-rule
               ,@(when qualifier (list qualifier))
               ((rule ,class-name) ,arg)
             (declare (ignorable ,arg))
             (flet ((,next-fn-sym ()
                      (call-next-method)))
               (declare (ignorable #',next-fn-sym))
               (let ((,resolver-arg
                       #'(lambda (sub-rule sub-value)
                           (funcall (get-parser1 sub-rule) sub-value))))
                 (declare (ignorable ,resolver-arg))
                  ,(if args
                      `(block ,tag
                         (with-slots ,args rule
                           (declare (ignorable ,@args))
                           ,@body))
                      `(block ,tag
                         ,@body))))))))))

(defun get-validator1 (rule)
  (let ((rule-instance (make-validator-rule-instance rule)))
    (lambda (val)
      (apply-validator-rule rule-instance val))))

(defun get-parser1 (rule)
  (let ((rule-instance (make-parser-rule-instance rule)))
    (lambda (val)
      (apply-parser-rule rule-instance val))))

(defun build-validator (rules)
  (lambda (val)
    (labels ((rec (rest-rules)
               (unless rest-rules
                 (return-from rec t))
               (let ((rule (car rest-rules)))
                 (when (eq (rule-tag rule) :optional)
                   (return-from rec
                     (or (null val) (rec (cdr rest-rules)))))
                 (and (funcall (get-validator1 rule) val)
                      (rec (cdr rest-rules))))))
      (rec rules))))

(defun build-parser (rules)
  (let ((parsers (mapcar #'get-parser1 rules)))
    (lambda (val)
      (labels ((rec (current rest-parsers)
                 (unless rest-parsers
                   (return-from rec (values t current)))
                 (multiple-value-bind (ok parsed)
                     (funcall (car rest-parsers) current)
                   (if ok
                       (rec parsed (cdr rest-parsers))
                       (values nil nil)))))
        (rec val parsers)))))

(defun validate-fn (val rules)
  (let ((validator (build-validator rules)))
    (funcall validator val)))

(defun normalize-validate-rules (rules)
  (if rules
      rules
      '(:pass)))

(defun normalize-parse-rules (rules)
  (if rules
      rules
      '(:identity)))

(defun parse-fn (val rules)
  (funcall (build-parser rules) val))

(defun plist->alist (plist)
  (unless (and (listp plist)
               (evenp (length plist)))
    (error "plist must be an even-length list: ~S" plist))
  (loop for cell on plist by #'cddr
        collect (cons (car cell) (cadr cell))))

(defmacro validate-alist (var-key-rules alist &body body)
  "alistをバリデーションする。
(validate-alist ((id :id :required :integer)
                 (name \"name\" :required (:max-length 32))
                 (password \"password\" :required))
    '((:id . 10) (\"name\" . \"your name\") (\"password\" . \"password\"))
  (list id name password)) => (10 \"your name\" \"password\")
(validate-alist ((id :id))
    '((:id . 10))
  id) => 10

各呼び出し位置ごとに、ルールはマクロ展開時/ロード時に固定される。
ルールは前方から順番に評価される。"
  (let ((configs (build-validate-alist-configs var-key-rules)))
    `(let* ,(build-validate-alist-value-bindings alist configs)
       ,@body)))

(defmacro validate-plist (var-key-rules plist &body body)
  "plistをバリデーションする。重複キーは先頭要素を採用する。
(validate-plist ((id :id :required :integer)
                 (name \"name\" :required (:max-length 32))
                 (password \"password\" :required))
    '(:id 10 \"name\" \"your name\" \"password\" \"password\")
  (list id name password)) => (10 \"your name\" \"password\")
(validate-plist ((id :id))
    '(:id 10)
  id) => 10"
  (let ((alist (gensym "ALIST-")))
    `(let ((,alist (plist->alist ,plist)))
       (validate-alist ,var-key-rules ,alist
         ,@body))))

(defmacro validate-list (var-rules list-values &body body)
  "listを定義順でバリデーションする。
(validate-list ((id :required :integer)
                (name :required (:max-length 32)))
  '(10 \"your name\")
  (list id name)) => (10 \"your name\")
(validate-list ((id :integer))
  '(10)
  id) => 10
(validate-list ((head :integer)
                (tail (&rest :integer)))
  '(1 2 3)
  (list head tail)) => (1 (2 3))

要素不足時はnilを使い、余剰要素は無視する。
各呼び出し位置ごとに、ルールはマクロ展開時/ロード時に固定される。
ルールは前方から順番に評価される。"
  (let ((values-sym (gensym "VALUES-")))
    (multiple-value-bind (normal-rules rest-entry)
        (split-list-rules-with-rest var-rules)
      (multiple-value-bind (configs rest-config)
          (build-validate-list-configs normal-rules rest-entry)
        (let ((value-bindings
                (build-validate-list-value-bindings values-sym
                                                    configs
                                                    rest-config)))
          `(let ((,values-sym ,list-values))
             (let* ,value-bindings
               ,@body)))))))

(defmacro %validate-value ((var value field &rest rules) &body body)
  (let* ((normalized-rules (if rules rules '(:pass)))
         (validator-sym (gensym "VALIDATOR-")))
    `(let ((,validator-sym (load-time-value (build-validator ',normalized-rules) t)))
       (let ((,var ,value))
         (unless (funcall ,validator-sym ,var)
           (error 'validation-error
                  :field ,field
                  :value ,var
                  :rules ',normalized-rules))
         ,@body))))

(defmacro validate-value ((var value &rest rules) &body body)
  "単一値をバリデーションする。
(validate-value (id 10 :required :integer)
  id) => 10
(validate-value (id 10)
  id) => 10

各呼び出し位置ごとに、ルールはマクロ展開時/ロード時に固定される。
ルールは前方から順番に評価される。"
  `(%validate-value (,var ,value ',var ,@rules)
     ,@body))

(defmacro parse-alist (var-key-rules alist &body body)
  "宣言的なパーサールールでalistの値をパースする。
(parse-alist ((id :id :integer)
              (name \"name\" :string))
  '((:id . \"10\") (\"name\" . \"your name\"))
  (list id name)) => (10 \"your name\")
(parse-alist ((id :id))
  '((:id . \"10\"))
  id) => \"10\"

各呼び出し位置ごとに、ルールはマクロ展開時/ロード時に固定される。
ルールは前方から順番に評価される。"
  (let ((sym (gensym))
        (parsed (gensym))
        (ok (gensym)))
    (let ((configs (build-parse-alist-configs var-key-rules)))
      `(let ,(build-parse-alist-parser-bindings configs)
         (let* ,(build-parse-alist-value-bindings sym ok parsed alist configs)
           ,@body)))))

(defmacro parse-plist (var-key-rules plist &body body)
  "宣言的なパーサールールでplistの値をパースする。重複キーは先頭要素を採用する。
(parse-plist ((id :id :integer)
              (name \"name\" :string))
  '(:id \"10\" \"name\" \"your name\")
  (list id name)) => (10 \"your name\")
(parse-plist ((id :id))
  '(:id \"10\")
  id) => \"10\""
  (let ((alist (gensym "ALIST-")))
    `(let ((,alist (plist->alist ,plist)))
       (parse-alist ,var-key-rules ,alist
         ,@body))))

(defmacro parse-list (var-rules list-values &body body)
  "listを定義順でパースする。
(parse-list ((id :integer)
             (name :string))
  '(\"10\" \"your name\")
  (list id name)) => (10 \"your name\")
(parse-list ((id))
  '(\"10\")
  id) => \"10\"
(parse-list ((head :integer)
             (tail (&rest :integer)))
  '(\"1\" \"2\" \"3\")
  (list head tail)) => (1 (2 3))

要素不足時はnilを使い、余剰要素は無視する。
各呼び出し位置ごとに、ルールはマクロ展開時/ロード時に固定される。
ルールは前方から順番に評価される。"
  (let ((values-sym (gensym "VALUES-")))
    (multiple-value-bind (normal-rules rest-entry)
        (split-list-rules-with-rest var-rules)
      (multiple-value-bind (configs rest-config)
          (build-parse-list-configs normal-rules rest-entry)
        (let ((parser-bindings
                (build-parse-list-parser-bindings configs rest-config))
              (value-bindings
                (build-parse-list-value-bindings values-sym configs rest-config)))
          `(let ((,values-sym ,list-values))
             (let ,parser-bindings
               (let* ,value-bindings
                 ,@body))))))))

(defmacro parse-value ((var value &rest rules) &body body)
  "単一値をパースする。
(parse-value (id \"10\" :integer)
  id) => 10
(parse-value (id \"10\")
  id) => \"10\"

各呼び出し位置ごとに、ルールはマクロ展開時/ロード時に固定される。
ルールは前方から順番に評価される。"
  (let* ((normalized-rules (if rules rules '(:identity)))
         (parser-sym (gensym "PARSER-"))
         (ok-sym (gensym "OK-"))
         (parsed-sym (gensym "PARSED-")))
    `(let ((,parser-sym (load-time-value (build-parser ',normalized-rules) t)))
       (let ((,var ,value))
         (multiple-value-bind (,ok-sym ,parsed-sym)
             (funcall ,parser-sym ,var)
           (unless ,ok-sym
             (error 'validation-parse-error
                    :field ',var
                    :value ,var
                    :rules ',normalized-rules))
           (let ((,var ,parsed-sym))
             ,@body))))))

(defmacro parse+validate-alist (var-key-rules alist &body body)
  "alistの値をパースしてからバリデーションする。
(parse+validate-alist ((id :id (:parse :integer) (:validate :required :integer))
                 (name \"name\" (:parse :string) (:validate :required (:max-length 32))))
  '((:id . \"10\") (\"name\" . \"your name\"))
  (list id name)) => (10 \"your name\")

各呼び出し位置ごとに、ルールはマクロ展開時/ロード時に固定される。"
  (let ((sym (gensym))
        (parsed (gensym))
        (ok (gensym)))
    (let ((configs (build-parse+validate-alist-configs var-key-rules)))
      `(let ,(append (build-parse+validate-alist-parser-bindings configs)
                     (build-parse+validate-alist-validator-bindings configs))
         (let* ,(build-parse+validate-alist-value-bindings
                 sym ok parsed alist configs)
           ,@body)))))

(defmacro parse+validate-plist (var-key-rules plist &body body)
  "plistの値をパースしてからバリデーションする。重複キーは先頭要素を採用する。
(parse+validate-plist ((id :id (:parse :integer) (:validate :required :integer))
                       (name \"name\" (:parse :string) (:validate :required (:max-length 32))))
  '(:id \"10\" \"name\" \"your name\")
  (list id name)) => (10 \"your name\")
(parse+validate-plist ((id :id (:parse :integer)))
  '(:id \"10\")
  id) => 10"
  (let ((alist (gensym "ALIST-")))
    `(let ((,alist (plist->alist ,plist)))
       (parse+validate-alist ,var-key-rules ,alist
         ,@body))))

(defmacro parse+validate-list (var-rules list-values &body body)
  "listを定義順でパースしてからバリデーションする。
(parse+validate-list ((id (:parse :integer) (:validate :required :integer))
                      (name (:parse :string) (:validate :required (:max-length 32))))
  '(\"10\" \"your name\")
  (list id name)) => (10 \"your name\")
(parse+validate-list ((id (:parse :integer)))
  '(\"10\")
  id) => 10
(parse+validate-list ((head (:parse :integer) (:validate :integer))
                      (tail (&rest (:parse :integer) (:validate :integer))))
  '(\"1\" \"2\" \"3\")
  (list head tail)) => (1 (2 3))

要素不足時はnilを使い、余剰要素は無視する。
各呼び出し位置ごとに、ルールはマクロ展開時/ロード時に固定される。"
  (let ((values-sym (gensym "VALUES-")))
    (multiple-value-bind (normal-rules rest-entry)
        (split-list-rules-with-rest var-rules)
      (multiple-value-bind (configs rest-config)
          (build-parse+validate-list-configs normal-rules rest-entry)
        (let ((parser-bindings
                (build-parse+validate-list-parser-bindings configs rest-config))
              (validator-bindings
                (build-parse+validate-list-validator-bindings configs rest-config))
              (value-bindings
                (build-parse+validate-list-value-bindings values-sym
                                                          configs
                                                          rest-config)))
          `(let ((,values-sym ,list-values))
             (let ,(append parser-bindings validator-bindings)
               (let* ,value-bindings
                 ,@body))))))))

(defmacro parse+validate-value ((var value &rest sections) &body body)
  "単一値をパースしてからバリデーションする。
(parse+validate-value (id \"10\" (:parse :integer) (:validate :required :integer))
  id) => 10
(parse+validate-value (id \"10\" (:parse :integer))
  id) => 10

各呼び出し位置ごとに、ルールはマクロ展開時/ロード時に固定される。"
  (multiple-value-bind (parse-rules validate-rules)
      (parse+validate-rules (list* var nil sections))
    (let ((parser-sym (gensym "PARSER-"))
          (validator-sym (gensym "VALIDATOR-"))
          (ok-sym (gensym "OK-"))
          (parsed-sym (gensym "PARSED-")))
      `(let ((,parser-sym (load-time-value (build-parser ',parse-rules) t))
             (,validator-sym ,(if validate-rules
                                  `(load-time-value
                                     (build-validator ',validate-rules)
                                     t)
                                  nil)))
         (let ((,var ,value))
           (multiple-value-bind (,ok-sym ,parsed-sym)
               (funcall ,parser-sym ,var)
             (unless ,ok-sym
               (error 'validation-parse-error
                      :field ',var
                      :value ,var
                      :rules ',parse-rules))
             (when (and ,validator-sym
                        (not (funcall ,validator-sym ,parsed-sym)))
               (error 'validation-error
                      :field ',var
                      :value ,parsed-sym
                      :rules ',validate-rules))
             (let ((,var ,parsed-sym))
               ,@body)))))))
