(defsystem "sql-orm"
  :version "0.2.0"
  :description "Lispy CLOS ORM for cl-stack — models, relations, calc fields, schema diff on sql-query"
  :author "egao1980"
  :license "MIT"
  :depends-on ("sql-protocol" "sql-query")
  :properties
  (:cl-repo
   (:ci (:with ("sql-backend-sqlite3" "sql-query-sqlite3" "sql-query-postgres")
         :load-before-test ("sql-backend-sqlite3" "sql-query-sqlite3"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "meta")
               (:file "defmodel")
               (:file "persistence")
               (:file "relations")
               (:file "schema")
               (:file "algebra")
               (:file "reflect"))
  :in-order-to ((test-op (test-op "sql-orm/tests"))))

(defsystem "sql-orm/tests"
  :depends-on ("sql-orm"
               "sql-backend-sqlite3"
               "sql-query-sqlite3"
               "sql-query-postgres"
               "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "orm-test")
               (:file "schema-test")
               (:file "s1-fixtures")
               (:file "algebra-test")
               (:file "reflect-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
