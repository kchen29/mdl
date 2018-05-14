;;;;compiler compiler

;;;lexer
(defun lexify (file classifier)
  "Given CLASSIFIER, classify each token in FILE, producing a token list.
   CLASSIFIER should return uninterned symbols.
   If skip is returned, don't add the token and skip the line.
   If the returned symbol is unbound, then give it the value of the substring."
  (with-open-file (stream file)
    (collect-to
     (do ((sub (read-token stream) (read-token stream)))
         ((string= "" sub))
       (let ((sym (funcall classifier sub)))
         (if (name= 'skip sym)
             (read-line stream)
             (progn
               (unless (boundp sym)
                 (setf (symbol-value sym) sub))
               (collect sym))))))))

(defun read-token (stream)
  "Reads a token from STREAM."
  (with-output-to-string (s-stream)
    ;;remove all whitespace
    (do-stream (char stream) ((not (whitespace-p char))
                              (when (characterp char)
                                (unread-char char stream))))
    ;;then read token
    (do-stream (char stream) ((whitespace-p char)
                              (when (characterp char)
                                (unread-char char stream)))
      (princ char s-stream))))

;;;primitive parser
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

(defmacro match (tokens-var &body matches)
  `(cond
     ,@(loop for match-form in matches
             if (atom (car match-form))
               do (setf (car match-form) (list (car match-form)))
             collect (macroexpand-1 `(match-single ,tokens-var ,match-form)))
     (t (error "Parser error. Token list: ~a" ,tokens-var))))
