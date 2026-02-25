(in-package #:list-validate)

(defun %parse-float-string (s)
  (let ((*read-eval* nil))
    (multiple-value-bind (obj pos)
        (ignore-errors (read-from-string s nil nil))
      (if (and obj
               (= pos (length s))
               (floatp obj))
          (values t obj)
          (values nil nil)))))

(defun %parse-number-string (s)
  (let ((int (ignore-errors (parse-integer s :junk-allowed nil))))
    (when int
      (return-from %parse-number-string (values t int))))
  (%parse-float-string s))

(defun %split-string (s sep)
  (let* ((sep-string
           (etypecase sep
             (character (string sep))
             (string sep)))
         (sep-len (length sep-string)))
    (when (zerop sep-len)
      (error "separator must not be empty string"))
    (let ((start 0)
          (parts nil))
      (loop
        for pos = (search sep-string s :start2 start)
        do (if pos
               (progn
                 (push (subseq s start pos) parts)
                 (setf start (+ pos sep-len)))
               (progn
                 (push (subseq s start) parts)
                 (return (nreverse parts))))))))

(defun %string-join (parts sep)
  (with-output-to-string (out)
    (loop for part in parts
          for first = t then nil
          do (unless first
               (princ sep out))
             (princ part out))))

(define-parser :identity (val)
  (values t val))

(define-parser :drop (_)
  (values t nil))

(define-parser (:default default-value) (val)
  (values t (if (null val) default-value val)))

(define-parser :integer (val)
  (typecase val
    (integer
     (values t val))
    (string
     (let ((parsed (ignore-errors (parse-integer val :junk-allowed nil))))
       (if parsed
           (values t parsed)
           (values nil nil))))
    (t
     (values nil nil))))

(define-parser :float (val)
  (typecase val
    (float
     (values t val))
    (string
     (%parse-float-string val))
    (t
     (values nil nil))))

(define-parser :number (val)
  (typecase val
    (number
     (values t val))
    (string
     (%parse-number-string val))
    (t
     (values nil nil))))

(define-parser :boolean (val)
  (typecase val
    (null
     (values t nil))
    (symbol
     (cond
       ((eq val t) (values t t))
       ((eq val :true) (values t t))
       ((eq val :false) (values t nil))
       (t (values nil nil))))
    (number
     (cond
       ((= val 1) (values t t))
       ((= val 0) (values t nil))
       (t (values nil nil))))
    (string
     (let ((normalized (string-downcase
                        (string-trim '(#\Space #\Tab #\Newline #\Return)
                                     val))))
       (cond
         ((or (string= normalized "true")
              (string= normalized "1")
              (string= normalized "yes")
              (string= normalized "on"))
          (values t t))
         ((or (string= normalized "false")
              (string= normalized "0")
              (string= normalized "no")
              (string= normalized "off"))
          (values t nil))
         (t
          (values nil nil)))))
    (t
     (values nil nil))))

(define-parser :keyword (val)
  (typecase val
    (keyword
     (values t val))
    (symbol
     (values t (intern (string-upcase (symbol-name val)) :keyword)))
    (string
     (values t (intern (string-upcase val) :keyword)))
    (t
     (values nil nil))))

(define-parser :symbol (val)
  (typecase val
    (symbol
     (values t val))
    (string
     (values t (intern (string-upcase val))))
    (t
     (values nil nil))))

(define-parser :string (val)
  (if (stringp val)
      (values t val)
      (handler-case
          (values t (princ-to-string val))
        (error () (values nil nil)))))

(define-parser :trim (val)
  (if (stringp val)
      (values t (string-trim '(#\Space #\Tab #\Newline #\Return) val))
      (values nil nil)))

(define-parser :null-if-empty (val)
  (typecase val
    (null
     (values t nil))
    (string
     (if (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) val)))
         (values t nil)
         (values t val)))
    (t
     (values t val))))

(define-parser (:split sep) (val)
  (if (stringp val)
      (handler-case
          (values t (%split-string val sep))
        (error () (values nil nil)))
      (values nil nil)))

(define-parser (:join sep) (val)
  (if (typep val 'sequence)
      (values t (%string-join (coerce val 'list) sep))
      (values nil nil)))

(define-parser :upper-string (val)
  (if (stringp val)
      (values t (string-upcase val))
      (values nil nil)))

(define-parser :lower-string (val)
  (if (stringp val)
      (values t (string-downcase val))
      (values nil nil)))

(define-parser (:or &rest rules) (val parse)
  (dolist (rule rules (values nil nil))
    (multiple-value-bind (ok parsed)
        (funcall parse rule val)
      (when ok
        (return (values t parsed))))))

(define-parser (:and &rest rules) (val parse)
  (let ((current val))
    (dolist (rule rules (values t current))
      (multiple-value-bind (ok parsed)
          (funcall parse rule current)
        (unless ok
          (return (values nil nil)))
        (setf current parsed)))))

(define-parser (:chain &rest rules) (val parse)
  (let ((current val))
    (dolist (rule rules (values t current))
      (multiple-value-bind (ok parsed)
          (funcall parse rule current)
        (unless ok
          (return (values nil nil)))
        (setf current parsed)))))

(define-parser (:list &rest rules) (val parse)
  (let ((list-value (cond
                      ((listp val) val)
                      ((typep val 'sequence) (coerce val 'list))
                      (t nil))))
    (unless list-value
      (return-from :list (values nil nil)))
    (unless rules
      (return-from :list (values t list-value)))
    (when (/= (length list-value) (length rules))
      (return-from :list (values nil nil)))
    (let ((parsed-elements nil))
      (loop for element in list-value
            for rule in rules
            do (multiple-value-bind (ok parsed)
                   (funcall parse rule element)
                 (unless ok
                   (return-from :list (values nil nil)))
                 (push parsed parsed-elements)))
      (values t (nreverse parsed-elements)))))

(define-parser (:map rule) (val parse)
  (unless (typep val 'sequence)
    (return-from :map (values nil nil)))
  (let ((results nil))
    (loop for element in (coerce val 'list)
          do (multiple-value-bind (ok parsed)
                 (funcall parse rule element)
               (unless ok
                 (return-from :map (values nil nil)))
               (push parsed results)))
    (values t (nreverse results))))

(define-parser (:not rule) (val parse)
  (multiple-value-bind (ok parsed)
      (funcall parse rule val)
    (declare (ignore parsed))
    (if ok
        (values nil nil)
        (values t val))))
