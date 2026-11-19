(in-package #:sql-orm)

;;; sql-orm 0.2.0 live-schema reflection and dialect-specific DDL (S1).
;;; Dispatch is on the dialect keyword (SCHEMA-DIALECT-KEY), so sql-orm needs
;;; neither sql-query-sqlite3 nor sql-query-postgres at load time.

;;; ---------------------------------------------------------------------------
;;; Catalog queries
;;; ---------------------------------------------------------------------------

(defgeneric reflection-query (dialect-key kind &key table schema)
  (:documentation
   "SQL string and params (values sql params) that read KIND from the catalog.

:SQLITE3 kinds :TABLES, :COLUMNS, :INDEXES (TABLE required for the last two)
and :INDEX-COLUMNS (TABLE = the index name).
:POSTGRES kinds :TABLES and :COLUMNS, both over every base table of SCHEMA
(default \"public\"), params = (SCHEMA), placeholders $1.")
  (:method (dialect-key kind &key table schema)
    (declare (ignore table schema))
    (error 'reflection-unsupported
           :dialect dialect-key
           :message (format nil "no catalog query for ~S" kind))))

(defun %ident (name)
  "Identifier string as sql-query emits it (symbols downcased)."
  (if (symbolp name) (string-downcase (symbol-name name)) name))

;;; SQLite

(defmethod reflection-query ((dialect-key (eql :sqlite3)) (kind (eql :tables)) &key table schema)
  (declare (ignore table schema))
  (values "SELECT name, sql FROM sqlite_master WHERE type = 'table' AND substr(name, 1, 7) <> 'sqlite_' ORDER BY name"
          '()))

(defmethod reflection-query ((dialect-key (eql :sqlite3)) (kind (eql :columns)) &key table schema)
  (declare (ignore schema))
  (values "SELECT cid, name, type, \"notnull\" AS not_null, dflt_value, pk FROM pragma_table_info(?) ORDER BY cid"
          (list (%ident table))))

(defmethod reflection-query ((dialect-key (eql :sqlite3)) (kind (eql :indexes)) &key table schema)
  (declare (ignore schema))
  (values "SELECT name, \"unique\" AS is_unique, origin FROM pragma_index_list(?)"
          (list (%ident table))))

(defmethod reflection-query ((dialect-key (eql :sqlite3)) (kind (eql :index-columns)) &key table schema)
  (declare (ignore schema))
  (values "SELECT name FROM pragma_index_info(?)"
          (list (%ident table))))

;;; PostgreSQL

(defparameter *%postgres-tables-sql*
  "SELECT table_name FROM information_schema.tables WHERE table_schema = $1 AND table_type = 'BASE TABLE' ORDER BY table_name")

(defparameter *%postgres-columns-sql*
  "SELECT c.relname AS table_name,
       a.attname AS column_name,
       format_type(a.atttypid, a.atttypmod) AS data_type,
       a.attnotnull AS not_null,
       pg_get_expr(d.adbin, d.adrelid) AS column_default,
       a.attidentity AS identity,
       EXISTS (SELECT 1 FROM pg_catalog.pg_index i
               WHERE i.indrelid = c.oid AND i.indisprimary
                 AND i.indnatts = 1 AND i.indkey[0] = a.attnum) AS is_primary_key,
       EXISTS (SELECT 1 FROM pg_catalog.pg_index i
               WHERE i.indrelid = c.oid AND i.indisunique AND NOT i.indisprimary
                 AND i.indnatts = 1 AND i.indkey[0] = a.attnum) AS is_unique
FROM pg_catalog.pg_class c
JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
JOIN pg_catalog.pg_attribute a ON a.attrelid = c.oid
LEFT JOIN pg_catalog.pg_attrdef d ON d.adrelid = c.oid AND d.adnum = a.attnum
WHERE c.relkind = 'r' AND n.nspname = $1 AND a.attnum > 0 AND NOT a.attisdropped
ORDER BY c.relname, a.attnum")

(defmethod reflection-query ((dialect-key (eql :postgres)) (kind (eql :tables)) &key table schema)
  (declare (ignore table))
  (values *%postgres-tables-sql* (list (or schema "public"))))

(defmethod reflection-query ((dialect-key (eql :postgres)) (kind (eql :columns)) &key table schema)
  (declare (ignore table))
  (values *%postgres-columns-sql* (list (or schema "public"))))

;;; ---------------------------------------------------------------------------
;;; Row helpers
;;; ---------------------------------------------------------------------------

(defun %sql-null-p (value)
  (or (null value) (eq value :null)))

(defun %truthy (value)
  "Catalog boolean: T/NIL, 1/0, or SQL NULL (false)."
  (and (not (%sql-null-p value)) (not (eql value 0)) t))

(defun %exclude-predicate (exclude)
  "EXCLUDE (NIL, a list of names compared STRING-EQUAL, or a predicate on the
table-name string) as a predicate."
  (cond ((null exclude) (constantly nil))
        ((listp exclude) (lambda (name) (member name exclude :test #'string-equal)))
        (t (lambda (name) (funcall exclude name)))))

(defun %catalog-rows (connection dialect-key kind &rest args)
  "The only database access in reflection: EXECUTE + FETCH-ALL, no transaction."
  (multiple-value-bind (sql params) (apply #'reflection-query dialect-key kind args)
    (sql-protocol:fetch-all (sql-protocol:execute connection sql params))))

;;; ---------------------------------------------------------------------------
;;; Reflection
;;; ---------------------------------------------------------------------------

(defgeneric reflect-schema-using (dialect-key connection &key exclude schema)
  (:documentation
   "Snapshot of the live database behind CONNECTION, read with the
REFLECTION-QUERY statements of DIALECT-KEY through SQL-PROTOCOL:EXECUTE and
SQL-PROTOCOL:FETCH-ALL, outside any transaction. The default method signals
REFLECTION-UNSUPPORTED.")
  (:method (dialect-key connection &key exclude schema)
    (declare (ignore connection exclude schema))
    (error 'reflection-unsupported
           :dialect dialect-key
           :message "no REFLECT-SCHEMA-USING method for this dialect key")))

(defun %sqlite-columns (connection table autoincrement-table-p)
  (let ((unique-names
          (loop for index in (%catalog-rows connection :sqlite3 :indexes :table table)
                when (and (%truthy (getf index :is_unique))
                          (equal (getf index :origin) "u"))
                  append (let ((cols (%catalog-rows connection :sqlite3 :index-columns
                                                    :table (getf index :name))))
                           (when (= 1 (length cols))
                             (list (getf (first cols) :name)))))))
    (loop for row in (%catalog-rows connection :sqlite3 :columns :table table)
          for name = (getf row :name)
          for pk = (let ((p (getf row :pk))) (and (integerp p) (plusp p)))
          for type = (getf row :type)
          for default = (getf row :dflt_value)
          collect (make-column-info
                   :name name
                   :type (if (or (%sql-null-p type) (equal type "")) nil type)
                   :primary-key pk
                   :autoincrement (and pk autoincrement-table-p)
                   :not-null (%truthy (getf row :not_null))
                   :unique (and (member name unique-names :test #'string=) t)
                   :default (if (%sql-null-p default)
                                nil
                                (sql-query:sql-raw (princ-to-string default)))))))

(defmethod reflect-schema-using ((dialect-key (eql :sqlite3)) connection &key exclude schema)
  (declare (ignore schema))
  (let ((skip (%exclude-predicate exclude)))
    (loop for row in (%catalog-rows connection :sqlite3 :tables)
          for name = (getf row :name)
          for ddl = (getf row :sql)
          unless (or (funcall skip name)
                     (eql 0 (search "sqlite_" name :test #'char-equal)))
            collect (cons name
                          (%sqlite-columns
                           connection name
                           (and (stringp ddl)
                                (search "AUTOINCREMENT" ddl :test #'char-equal)
                                t))))))

(defun %type-name-p (string)
  "STRING reads as a PostgreSQL type name: letters, digits, _ . \" space [ ]
and a balanced (n[,m])."
  (and (plusp (length string))
       (every (lambda (ch) (or (alphanumericp ch) (find ch "_. \"[](),"))) string)
       (= (cl:count #\( string) (cl:count #\) string))))

(defun %strip-trailing-casts (text)
  "Remove trailing ::type casts outside quoted literals and parentheses:
'a::b'::character varying → 'a::b'; 'x'::text::varchar → 'x'; now() unchanged."
  (loop
    (let ((cut (let ((in-quote nil) (depth 0) (last nil) (n (length text)))
                 (dotimes (i n last)
                   (let ((ch (char text i)))
                     (cond ((char= ch #\') (setf in-quote (not in-quote)))
                           (in-quote)
                           ((char= ch #\() (incf depth))
                           ((char= ch #\)) (decf depth))
                           ((and (zerop depth) (char= ch #\:)
                                 (< (1+ i) n) (char= (char text (1+ i)) #\:))
                            (setf last i))))))))
      (if (and cut (%type-name-p (subseq text (+ cut 2))))
          (setf text (string-right-trim " " (subseq text 0 cut)))
          (return text)))))

(defun %postgres-column (row)
  (let* ((raw (getf row :column_default))
         (default (if (%sql-null-p raw) nil raw))
         (nextval (and default (eql 0 (search "nextval(" default :test #'char-equal))))
         (identity (let ((i (getf row :identity)))
                     (if (characterp i) (string i) i))))
    (make-column-info
     :name (getf row :column_name)
     :type (getf row :data_type)
     :primary-key (%truthy (getf row :is_primary_key))
     :unique (%truthy (getf row :is_unique))
     :not-null (%truthy (getf row :not_null))
     :autoincrement (or nextval
                        (and (stringp identity)
                             (member identity '("a" "d") :test #'string=)
                             t))
     :default (and default (not nextval)
                   (sql-query:sql-raw (%strip-trailing-casts default))))))

(defmethod reflect-schema-using ((dialect-key (eql :postgres)) connection &key exclude schema)
  (let* ((skip (%exclude-predicate exclude))
         (tables (remove-if skip
                            (mapcar (lambda (row) (getf row :table_name))
                                    (%catalog-rows connection :postgres :tables
                                                   :schema schema))))
         (rows (%catalog-rows connection :postgres :columns :schema schema)))
    (mapcar (lambda (table)
              (cons table
                    (loop for row in rows
                          when (equal (getf row :table_name) table)
                            collect (%postgres-column row))))
            tables)))

(defun reflect-schema (connection &key exclude dialect schema)
  "Snapshot (alist of (table-name-string . column-infos)) of the live database.

EXCLUDE is a list of table names (STRING-EQUAL) or a predicate on the table
name string. DIALECT (nil = the connection's dialect) selects the method of
REFLECT-SCHEMA-USING. SCHEMA is the PostgreSQL schema (default \"public\");
SQLite ignores it. Tables are ordered by name, columns by position."
  (let* ((key (schema-dialect-key (or dialect (dialect-for-connection connection))))
         (skip (%exclude-predicate exclude))
         (snapshot (reflect-schema-using key connection :exclude skip :schema schema)))
    (stable-sort (remove-if (lambda (entry) (funcall skip (car entry)))
                            (copy-list snapshot))
                 #'string< :key (lambda (entry) (string (car entry))))))

;;; ---------------------------------------------------------------------------
;;; Dialect DDL
;;; ---------------------------------------------------------------------------

(defgeneric dialect-schema-op-statements (dialect-key op direction connection)
  (:documentation
   "DDL AST list that runs OP in DIRECTION (:UPGRADE or :DOWNGRADE) on
CONNECTION. UPGRADE-SCHEMA / DOWNGRADE-SCHEMA call it per op, just before
running that op. Default: SCHEMA-OP-UPGRADE / SCHEMA-OP-DOWNGRADE.
(:SQLITE3 ALTER-COLUMN-OP): table rebuild from REFLECT-SCHEMA of CONNECTION.")
  (:method (dialect-key op direction connection)
    (declare (ignore dialect-key connection))
    (ecase direction
      (:upgrade (schema-op-upgrade op))
      (:downgrade (schema-op-downgrade op)))))

(defmethod dialect-schema-op-statements ((dialect-key (eql :sqlite3)) (op alter-column-op)
                                         direction connection)
  "SQLite has no ALTER COLUMN: rebuild the table from its live definition with
the column altered (CREATE <t>__sql_orm_rebuild, INSERT … SELECT, DROP, RENAME).
Runs inside the caller's transaction; reflection opens none."
  (let* ((step (ecase direction
                 (:upgrade op)
                 (:downgrade (invert-schema-op op))))
         (wanted (%ident (schema-op-table-name op)))
         (live (or (first (reflect-schema connection
                                          :dialect :sqlite3
                                          :exclude (lambda (name)
                                                     (not (string-equal name wanted)))))
                   (error 'snapshot-conflict
                          :op op
                          :message (format nil "table ~A is not in the live schema" wanted))))
         (table (car live))
         ;; The fold signals SNAPSHOT-CONFLICT when the live table lacks the column.
         (old-columns (cdr live))
         (new-columns (cdr (first (apply-ops-to-snapshot (list live) step))))
         (tmp (format nil "~A__sql_orm_rebuild" table))
         (names (mapcar (lambda (c) (%keywordize (column-info-name c))) old-columns)))
    (list (%create-table-stmt tmp new-columns)
          (insert-into tmp
                       (apply #'columns names)
                       (select (apply #'columns names) (from table)))
          (drop-table table)
          (alter-table tmp (sql-query:rename-to table)))))
