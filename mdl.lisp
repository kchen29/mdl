;;;;compiler for MDL. Refer to MDL.spec

(defvar commands '(push pop
                   move scale rotate
                   sphere torus box line
                   display save))

(defun classifier (str)
  "Classifies each substring as a token."
  (cond
    ((member (make-symbol (string-upcase str)) commands :test #'name=) (make-symbol (string-upcase str)))
    ((char= #\/ (char str 0)) nil)
    ((alpha-char-p (char str 0)) '#:symbol)
    ((numberp (read-from-string str)) (let ((x '#:number))
                                        (setf (symbol-value x) (read-from-string str))
                                        x))
    (t (error "~a does not indicate a token" str))))

;; (defmacro match (token-list &body matches)
;;   )

(defun compile-mdl (file)
  "Compiles FILE to an image."
  ;; (do ((token-list (lexify file #'classifier)))
  ;;     ((not token-list))
  ;;   (match token-list
  ;;     (())))
  (print (lexify file #'classifier))
  )
