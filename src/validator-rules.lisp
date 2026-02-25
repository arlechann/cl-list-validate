(in-package #:list-validate)

(define-validator :required (val)
  (not (or (null val)
           (and (stringp val)
                (zerop (length val))))))

(define-validator :pass (_)
  t)

(define-validator :optional (_)
  t)

(define-validator :integer (val)
  (integerp val))

(define-validator :string (val)
  (stringp val))

(define-validator :number (val)
  (numberp val))

(define-validator :float (val)
  (floatp val))

(define-validator (:list &rest rules) (val validate)
  (and (listp val)
       (or (null rules)
           (and (= (length val) (length rules))
                (loop for element in val
                      for rule in rules
                      always (funcall validate rule element))))))

(define-validator (:alist &rest rules) (val validate)
  (and (listp val)
       (every #'consp val)
       (or (null rules)
           (and (evenp (length rules))
                (loop for (key rule) on rules by #'cddr
                      always (funcall validate
                                      rule
                                      (cdr (assoc key val :test #'equal))))))))

(define-validator (:plist &rest rules) (val validate)
  (labels ((plistp (x)
             (and (listp x)
                  (evenp (length x))))
           (first-plist-value (plist key)
             (loop for cell on plist by #'cddr
                   when (equal (car cell) key)
                     do (return (cadr cell))
                   finally (return nil))))
    (and (plistp val)
         (or (null rules)
             (and (evenp (length rules))
                  (loop for (key rule) on rules by #'cddr
                        always (funcall validate
                                        rule
                                        (first-plist-value val key))))))))

(define-validator :null (val)
  (null val))

(define-validator :upper-string (val)
  (and (stringp val)
       (string= val (string-upcase val))))

(define-validator :lower-string (val)
  (and (stringp val)
       (string= val (string-downcase val))))

(define-validator (:or &rest rules) (val validate)
  (some #'(lambda (rule)
            (funcall validate rule val))
        rules))

(define-validator (:and &rest rules) (val validate)
  (every #'(lambda (rule)
             (funcall validate rule val))
         rules))

(define-validator (:not rule) (val validate)
  (not (funcall validate rule val)))

(define-validator (:min min-value) (val)
  (and (numberp val)
       (<= min-value val)))

(define-validator (:max max-value) (val)
  (and (numberp val)
       (<= val max-value)))

(define-validator (:between min-value max-value) (val)
  (and (numberp val)
       (<= min-value val)
       (<= val max-value)))

(define-validator (:min-length min-length) (val)
  (and (typep val 'sequence)
       (<= min-length (length val))))

(define-validator (:length exact-length) (val)
  (and (stringp val)
       (= (length val) exact-length)))

(define-validator (:max-length max-length) (val)
  (and (typep val 'sequence)
       (<= (length val) max-length)))

(define-validator (:one-of values) (val)
  (member val values :test #'equal))
