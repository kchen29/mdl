;;;;compiler for MDL. Refer to MDL.spec

(defvar commands '(push pop
                   move scale rotate
                   sphere torus box line
                   display save))

(defun classifier (str)
  "Classifies each substring as a token."
  (cond
    ((member (make-symbol (string-upcase str)) commands :test #'name=) (make-symbol (string-upcase str)))
    ((char= #\/ (char str 0)) '#:comment)
    ((alpha-char-p (char str 0)) '#:symbol)
    ((numberp (read-from-string str)) '#:number)
    (t (error "~a does not indicate a token" str))))

(defun compile-mdl (file)
  "Compiles FILE to an image."
  (let ((token-list (lexify file #'classifier)))
    (print token-list)))
