;;;; hack-assembler.asd

(asdf:defsystem #:hack-assembler
  :description "Describe hack-assembler here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:str #:parse-number #:cl-ppcre #:trivia #:trivia.ppcre #:iterate)
  :components ((:file "package")
               (:file "hack-assembler")))
