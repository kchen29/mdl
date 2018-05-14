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


(defmacro match (tokens-var &body matches)
  `(cond
     ,@(loop for match-form in matches
             if (atom (car match-form))
               do (setf (car match-form) (list (car match-form)))
             collect (macroexpand-1 `(match-single ,tokens-var ,match-form)))
     (t (error "Parser error. Token list: ~a" ,tokens-var))))

(defmacro match-single (tokens-var (patterns &rest actions))
  (let ((temp (gensym)))
    `((let ((,temp ,tokens-var))
        (and ,@(loop for pattern in patterns
                     if (and (listp pattern) (eq (car pattern) '&opt))
                       collect `(if (name= ',(cadr pattern) (car ,temp))
                                    (pop ,temp)
                                    t)
                     else
                       collect `(name= ',pattern (pop ,temp)))))
      (let ,(loop for pattern in patterns
                  for i = 0 then (1+ i)
                  collect `(,(concat-symbol 'a i)
                            ,(if (and (listp pattern) (eq (car pattern) '&opt))
                                 `(if (name= ',(cadr pattern) (car ,tokens-var))
                                      (symbol-value (pop ,tokens-var))
                                      nil)
                                 `(symbol-value (pop ,tokens-var)))))
        ,@actions))))

(defun compile-mdl (file)
  "Compiles FILE to an image."
  (do ((token-list (lexify file #'classifier)))
      ((not token-list))
    (match token-list
      (pop (stack-pop))
      (push (stack-push (copy-matrix (stack-peek))))
      ((move number number number) (update-current-stack (make-translate a1 a2 a3)))
      ((scale number number number))
      ((rotate symbol number))
      ((sphere (&opt symbol) number number number number (&opt symbol))
       (add-sphere polygons 10 a2 a3 a4 a5)
       (post-add-polygons))
      (display (display t))
      ((save symbol) (save a1)))))
