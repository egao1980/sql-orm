(defpackage #:sql-orm/tests
  (:use #:cl #:rove #:sql-orm)
  (:import-from #:sql-query #:compile-sql)
  (:import-from #:sql-query-sqlite3 #:make-sqlite3-dialect))

(in-package #:sql-orm/tests)
