(defsystem "engine"
  :depends-on ("compiler")
  :components
  ((:file "utils")
   (:file "display" :depends-on ("utils"))
   (:file "matrix" :depends-on ("utils"))
   (:file "edges" :depends-on ("matrix"))
   (:file "gmath")
   (:file "draw" :depends-on ("display" "edges" "gmath"))
   (:file "mdl" :depends-on ("draw"))))
