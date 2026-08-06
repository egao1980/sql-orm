;;;; Minimal sql-orm demo — ros -l examples/demo.lisp
(require :asdf)
(asdf:load-system "sql-backend-sqlite3")
(asdf:load-system "sql-query-sqlite3")
(asdf:load-system "sql-orm")

(defpackage #:sql-orm/demo
  (:use #:cl #:sql-orm))

(in-package #:sql-orm/demo)

(defmodel user ()
  (id :integer :primary-key t :autoincrement t)
  (name :text :not-null t)
  (:table users)
  (:compute greeting (self)
    (format nil "hi, ~A" (name self))))

(with-orm-connection (c :driver :sqlite3 :database-name ":memory:")
  (ensure-schema c 'user)
  (let ((u (persist (make-instance 'user :name "ada"))))
    (format t "~&~A id=~A~%" (greeting u) (id u))
    (format t "~&rows: ~S~%"
            (mapcar #'name (select-instances 'user :where (:= :name "ada"))))))
