(defsystem "engine"
  :components
  ((:file "utils")
   (:file "display" :depends-on ("utils"))
   (:file "matrix" :depends-on ("utils"))
   (:file "edges" :depends-on ("matrix"))
   (:file "gmath")
   (:file "draw" :depends-on ("display" "edges" "gmath"))
   (:file "lexer" :depends-on ("utils"))
   (:file "mdl" :depends-on ("draw" "lexer"))))
