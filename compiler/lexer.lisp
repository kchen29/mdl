(defun lexify (file classifier)
  "Given CLASSIFIER, classify each token in FILE, producing a token list.
   CLASSIFIER should return uninterned symbols."
  (with-open-file (stream file)
    (loop for sub = (read-token stream)
          until (string= "" sub)
          collect (let ((sym (funcall classifier sub)))
                    (setf (symbol-value sym) sub)
                    sym))))

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
