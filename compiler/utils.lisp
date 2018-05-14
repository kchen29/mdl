(defmacro do-stream ((var stream) (test &optional (result nil result-supplied-p)) &body body)
  "Concise way to iterate over STREAM. Iterates over each character; stops when eof is found or
   TEST is true, and returns RESULT. Performs BODY after each time VAR is set."
  `(do ((,var (read-char ,stream nil nil)
              (read-char ,stream nil nil)))
       ,(if result-supplied-p
            `((or (null ,var) ,test) ,result)
            `((or (null ,var) ,test)))
     ,@body))

(defun whitespace-p (char)
  "Returns true if CHAR is a whitespace character."
  (or (not (graphic-char-p char)) (char= char #\Space)))

(defun name= (sym1 sym2)
  "Returns if SYM1 and SYM2 have string= symbol names."
  (string= (symbol-name sym1) (symbol-name sym2)))
