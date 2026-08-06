(defsystem "sql-orm"
  :version "0.1.0"
  :description "Lispy CLOS ORM for cl-stack — models, relations, calc fields, schema diff on sql-query"
  :author "egao1980"
  :license "MIT"
  :depends-on ("sql-protocol" "sql-query")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "meta")
               (:file "defmodel")
               (:file "persistence")
               (:file "relations")
               (:file "schema"))
  :in-order-to ((test-op (test-op "sql-orm/tests"))))

(defsystem "sql-orm/tests"
  :depends-on ("sql-orm"
               "sql-backend-sqlite3"
               "sql-query-sqlite3"
               "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "orm-test")
               (:file "schema-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
