;;;;compiler for MDL. Refer to MDL.spec

(defvar commands '(push pop
                   move scale rotate
                   sphere torus box line
                   display save))

(defun classifier (str)
  "Classifies each substring as a token."
  (cond
    ((member (make-symbol (string-upcase str)) commands :test #'name=) (make-symbol (string-upcase str)))
    ((char= #\/ (char str 0)) (make-symbol "SKIP"))
    ((alpha-char-p (char str 0)) (make-symbol "SYMBOL"))
    ((numberp (read-from-string str)) (let ((x (make-symbol "NUMBER")))
                                        (setf (symbol-value x) (read-from-string str))
                                        x))
    (t (error "~a does not indicate a token" str))))

(defun compile-mdl (file)
  "Compiles FILE to an image."
  (let ((stack (list (make-transform-matrix)))
        (edges (make-matrix))
        (polygons (make-matrix)))
    (labels ((post-add-lines ()
               (matrix-multiply (car stack) edges)
               (draw-lines edges '(255 0 255))
               (clear-matrix edges))
             (post-add-polygons ()
               (matrix-multiply (car stack) polygons)
               (draw-polygons polygons)
               (clear-matrix polygons))
             (update-current-stack (transform)
               (push (matrix-multiply (pop stack) transform) stack)))
      (do ((token-list (lexify file #'classifier)))
          ((not token-list))
        (match token-list
          (push (push (copy-matrix (car stack)) stack))
          (pop (pop stack))
          ((move number number number)
           (update-current-stack (make-translate a1 a2 a3)))
          ((scale number number number)
           (update-current-stack (make-scale a1 a2 a3)))
          ((rotate symbol number)
           (update-current-stack (make-rotate (concat-symbol a1) a2)))
          ((sphere (&opt symbol) number number number number (&opt symbol))
           (add-sphere polygons 10 a2 a3 a4 a5)
           (post-add-polygons))
          ((torus (&opt symbol) number number number number number (&opt symbol))
           (add-torus polygons 10 a2 a3 a4 a5 a6)
           (post-add-polygons))
          ((box (&opt symbol) number number number number number number (&opt symbol))
           (add-box polygons a2 a3 a4 a5 a6 a7)
           (post-add-polygons))
          ((line (&opt symbol) number number number (&opt symbol) number number number (&opt symbol))
           (add-edge edges a2 a3 a4 a6 a7 a8)
           (post-add-lines))
          (display (display t))
          ((save symbol) (save a1)))))))
