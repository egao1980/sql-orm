(in-package #:sql-orm)

;;; sql-orm 0.2.0 live-schema reflection and dialect-specific DDL (S1).
;;; Dispatch is on the dialect keyword (SCHEMA-DIALECT-KEY), so sql-orm needs
;;; neither sql-query-sqlite3 nor sql-query-postgres at load time.

(defgeneric reflection-query (dialect-key kind &key table schema)
  (:documentation
   "SQL string and params (values sql params) that read KIND from the catalog.

:SQLITE3 kinds :TABLES, :COLUMNS, :INDEXES (TABLE required for the last two)
and :INDEX-COLUMNS (TABLE = the index name).
:POSTGRES kinds :TABLES and :COLUMNS, both over every base table of SCHEMA
(default \"public\"), params = (SCHEMA), placeholders $1.")
  (:method (dialect-key kind &key table schema)
    (declare (ignore dialect-key kind table schema))
    (%s1-pending "reflection-query")))

(defgeneric reflect-schema-using (dialect-key connection &key exclude schema)
  (:documentation
   "Snapshot of the live database behind CONNECTION, read with the
REFLECTION-QUERY statements of DIALECT-KEY through SQL-PROTOCOL:EXECUTE and
SQL-PROTOCOL:FETCH-ALL, outside any transaction. The default method signals
REFLECTION-UNSUPPORTED.")
  (:method (dialect-key connection &key exclude schema)
    (declare (ignore dialect-key connection exclude schema))
    (%s1-pending "reflect-schema-using")))

(defun reflect-schema (connection &key exclude dialect schema)
  "Snapshot (alist of (table-name-string . column-infos)) of the live database.

EXCLUDE is a list of table names (STRING-EQUAL) or a predicate on the table
name string. DIALECT (nil = the connection's dialect) selects the method of
REFLECT-SCHEMA-USING. SCHEMA is the PostgreSQL schema (default \"public\");
SQLite ignores it. Tables are ordered by name, columns by position."
  (declare (ignore connection exclude dialect schema))
  (%s1-pending "reflect-schema"))

(defgeneric dialect-schema-op-statements (dialect-key op direction connection)
  (:documentation
   "DDL AST list that runs OP in DIRECTION (:UPGRADE or :DOWNGRADE) on
CONNECTION. UPGRADE-SCHEMA / DOWNGRADE-SCHEMA call it per op, just before
running that op. Default: SCHEMA-OP-UPGRADE / SCHEMA-OP-DOWNGRADE.
(:SQLITE3 ALTER-COLUMN-OP): table rebuild from REFLECT-SCHEMA of CONNECTION.")
  (:method (dialect-key op direction connection)
    (declare (ignore dialect-key op direction connection))
    (%s1-pending "dialect-schema-op-statements")))
