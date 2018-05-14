(defun lexify (file classifier)
  "Given CLASSIFIER, classify each token in FILE, producing a token list.
   CLASSIFIER should return uninterned symbols. If nil is returned, don't add the token.
   If the returned symbol is unbound, then give it the value of the substring."
  (with-open-file (stream file)
    (collect-to
     (do ((sub (read-token stream) (read-token stream)))
         ((string= "" sub))
       (let ((sym (funcall classifier sub)))
         (when sym
           (unless (boundp sym)
             (setf (symbol-value sym) sub))
           (collect sym)))))))

(defun read-token (stream)
  "Reads a token from STREAM."
  (with-output-to-string (s-stream)
    ;;remove all whitespace
    (do-stream (char stream) ((not (whitespace-p char))
                              (when (characterp char)
                                (unread-char char stream))))
    ;;then read token
    (do-stream (char stream) ((whitespace-p char))
      (princ char s-stream))))
