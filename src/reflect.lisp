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
and :INDEX-COLUMNS (TABLE = the index name, columns in key order). For the
alter-column rebuild, with TABLE: :TABLE-SQL (its CREATE TABLE text),
:FOREIGN-KEYS (its own, pragma_foreign_key_list), :TABLE-OBJECTS (its indexes
and triggers that have SQL text, in creation order), :REFERENCING (foreign
keys of every table into it); and :FOREIGN-KEYS-ENABLED (PRAGMA foreign_keys).
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
  (values "SELECT name FROM pragma_index_info(?) ORDER BY seqno"
          (list (%ident table))))

;;; Read by the alter-column rebuild (issue #13).

(defmethod reflection-query ((dialect-key (eql :sqlite3)) (kind (eql :table-sql)) &key table schema)
  (declare (ignore schema))
  (values "SELECT sql FROM sqlite_master WHERE type = 'table' AND name = ? COLLATE NOCASE"
          (list (%ident table))))

(defmethod reflection-query ((dialect-key (eql :sqlite3)) (kind (eql :foreign-keys)) &key table schema)
  (declare (ignore schema))
  (values "SELECT id, seq, \"table\" AS parent, \"from\" AS from_column, \"to\" AS to_column, on_update, on_delete FROM pragma_foreign_key_list(?) ORDER BY id, seq"
          (list (%ident table))))

(defmethod reflection-query ((dialect-key (eql :sqlite3)) (kind (eql :table-objects)) &key table schema)
  (declare (ignore schema))
  (values "SELECT type, name, sql FROM sqlite_master WHERE type IN ('index', 'trigger') AND tbl_name = ? COLLATE NOCASE AND sql IS NOT NULL ORDER BY rowid"
          (list (%ident table))))

(defmethod reflection-query ((dialect-key (eql :sqlite3)) (kind (eql :referencing)) &key table schema)
  (declare (ignore schema))
  (values "SELECT m.name AS child, f.\"from\" AS from_column FROM sqlite_master m JOIN pragma_foreign_key_list(m.name) f WHERE m.type = 'table' AND f.\"table\" = ? COLLATE NOCASE ORDER BY m.name, f.id, f.seq"
          (list (%ident table))))

(defmethod reflection-query ((dialect-key (eql :sqlite3)) (kind (eql :foreign-keys-enabled)) &key table schema)
  (declare (ignore table schema))
  (values "SELECT foreign_keys FROM pragma_foreign_keys" '()))

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
(:SQLITE3 ALTER-COLUMN-OP): table rebuild from the live table on CONNECTION.")
  (:method (dialect-key op direction connection)
    (declare (ignore dialect-key connection))
    (ecase direction
      (:upgrade (schema-op-upgrade op))
      (:downgrade (schema-op-downgrade op)))))

(defmethod dialect-schema-op-statements (dialect-key (op alter-column-op) direction connection)
  "The portable ALTER COLUMN form, types and defaults compared as DIALECT-KEY
renders them (an unregistered key compares as ANSI)."
  (declare (ignore connection))
  (let ((from (schema-op-from-column op))
        (to (schema-op-to-column op)))
    (ecase direction
      (:upgrade (%alter-column-statements (schema-op-table-name op) from to dialect-key))
      (:downgrade (%alter-column-statements (schema-op-table-name op) to from dialect-key)))))

;;; ---------------------------------------------------------------------------
;;; SQLite alter-column: table rebuild (issue #13)
;;; ---------------------------------------------------------------------------
;;;
;;; SQLite has no ALTER COLUMN, so the table is rebuilt (sqlite.org/lang_altertable.html,
;;; "other kinds of table schema changes"). The column list comes from
;;; REFLECT-SCHEMA; what COLUMN-INFO does not model is reflected separately and
;;; kept:
;;;
;;;   foreign keys            pragma_foreign_key_list → table FOREIGN KEY constraints
;;;   composite PRIMARY KEY   pragma_table_info pk order → table PRIMARY KEY (…)
;;;   multi-column UNIQUE     pragma_index_list origin "u" → table UNIQUE (…)
;;;   CHECK constraints       the CREATE TABLE text → table CHECK (…), names kept
;;;   indexes, triggers       their sqlite_master SQL, re-run after the rename
;;;
;;; Views are not dropped and keep working. What the rebuild cannot keep makes
;;; it signal SQLITE-REBUILD-UNSUPPORTED (restart CONTINUE) before anything
;;; runs: COLLATE, generated columns, ON CONFLICT, DEFERRABLE, WITHOUT ROWID,
;;; STRICT, and foreign keys into the table while PRAGMA foreign_keys is ON
;;; (DROP TABLE would run their ON DELETE actions; turn foreign_keys OFF
;;; outside the transaction, as SQLite's procedure does).

(defun %ddl-quoted-end (sql start close)
  "Index after the quoted token of SQL at START that CLOSE ends. A doubled
CLOSE is an escape, except in [ ]."
  (let ((n (length sql))
        (i (1+ start)))
    (loop
      (let ((p (position close sql :start i)))
        (cond ((null p) (return n))
              ((and (char/= close #\]) (< (1+ p) n) (char= (char sql (1+ p)) close))
               (setf i (+ p 2)))
              (t (return (1+ p))))))))

(defun %ddl-word-char-p (ch)
  (or (alphanumericp ch) (char= ch #\_) (char= ch #\$)))

(defun %sqlite-ddl-tokens (sql)
  "Tokens of the SQLite DDL text SQL as (kind start end depth), comments and
whitespace skipped. KIND is :WORD, :QUOTED (\" ` [ ] identifier), :STRING
('…'), :OPEN, :CLOSE or :OTHER. DEPTH is the parenthesis depth outside the
token: 1 inside the column list of a CREATE TABLE."
  (let ((tokens '())
        (depth 0)
        (i 0)
        (n (length sql)))
    (flet ((emit (kind start end)
             (push (list kind start end depth) tokens)
             (setf i end)))
      (loop while (< i n)
            do (let ((ch (char sql i))
                     (next (and (< (1+ i) n) (char sql (1+ i)))))
                 (cond
                   ((member ch '(#\Space #\Tab #\Newline #\Return #\Page #\Linefeed))
                    (incf i))
                   ((and (char= ch #\-) (eql next #\-))
                    (setf i (or (position #\Newline sql :start i) n)))
                   ((and (char= ch #\/) (eql next #\*))
                    (setf i (let ((e (search "*/" sql :start2 (+ i 2))))
                              (if e (+ e 2) n))))
                   ((find ch "'\"`[")
                    (emit (if (char= ch #\') :string :quoted)
                          i (%ddl-quoted-end sql i (if (char= ch #\[) #\] ch))))
                   ((char= ch #\()
                    (emit :open i (1+ i))
                    (incf depth))
                   ((char= ch #\))
                    (decf depth)
                    (emit :close i (1+ i)))
                   ((%ddl-word-char-p ch)
                    (emit :word i (or (position-if-not #'%ddl-word-char-p sql :start i) n)))
                   (t (emit :other i (1+ i)))))))
    (nreverse tokens)))

(defun %ddl-unquote (text)
  "Identifier TEXT without its quotes (\"a\"\"b\" → a\"b)."
  (let ((open (and (plusp (length text)) (char text 0))))
    (case open
      ((#\" #\`)
       (let ((inner (subseq text 1 (max 1 (1- (length text)))))
             (doubled (coerce (list open open) 'string)))
         (with-output-to-string (out)
           (loop with i = 0
                 while (< i (length inner))
                 do (if (and (< (1+ i) (length inner))
                             (string= doubled inner :start2 i :end2 (+ i 2)))
                        (progn (write-char open out) (incf i 2))
                        (progn (write-char (char inner i) out) (incf i)))))))
      (#\[ (subseq text 1 (max 1 (1- (length text)))))
      (t text))))

(defun %sqlite-table-features (sql)
  "Read the CREATE TABLE text SQL → (values CHECKS LOST). CHECKS lists
(name . expression-text) for every column or table CHECK constraint, NAME
NIL when unnamed. LOST lists what a rebuild from COLUMN-INFOs cannot keep."
  (let* ((tokens (coerce (%sqlite-ddl-tokens sql) 'vector))
         (n (length tokens))
         (checks '())
         (lost '())
         (after-columns nil))
    (labels ((tok (k) (aref tokens k))
             (kind (k) (first (tok k)))
             (text (k) (subseq sql (second (tok k)) (third (tok k))))
             (word (k) (and (< -1 k n) (eq (kind k) :word) (string-upcase (text k))))
             (lose (what) (pushnew what lost :test #'string=))
             (check-at (k)
               ;; Token K is CHECK at depth 1; K+1 opens its expression.
               (when (and (< (1+ k) n) (eq (kind (1+ k)) :open))
                 (let ((close (loop for j from (+ k 2) below n
                                    when (and (eq (kind j) :close) (= 1 (fourth (tok j))))
                                      return j)))
                   (when close
                     (push (cons (and (>= k 2)
                                      (equal (word (- k 2)) "CONSTRAINT")
                                      (member (kind (1- k)) '(:word :quoted))
                                      (%ddl-unquote (text (1- k))))
                                 (string-trim " " (subseq sql (third (tok (1+ k)))
                                                          (second (tok close)))))
                           checks))))))
      (dotimes (k n)
        (let ((depth (fourth (tok k)))
              (w (word k)))
          (when (and (eq (kind k) :close) (zerop depth))
            (setf after-columns t))
          (when w
            (cond
              ((= depth 1)
               (cond ((string= w "CHECK") (check-at k))
                     ((string= w "COLLATE") (lose "COLLATE clause"))
                     ((or (string= w "GENERATED") (string= w "AS")) (lose "generated column"))
                     ((string= w "CONFLICT") (lose "ON CONFLICT clause"))
                     ((string= w "DEFERRABLE") (lose "DEFERRABLE foreign key"))))
              ((and (zerop depth) after-columns)
               (cond ((string= w "WITHOUT") (lose "WITHOUT ROWID"))
                     ((string= w "STRICT") (lose "STRICT table"))))))))
      (values (nreverse checks) (nreverse lost)))))

(defun %referential-action (text)
  "pragma_foreign_key_list action text → sql-query action keyword (NO ACTION → NIL)."
  (let ((u (and (stringp text) (string-upcase text))))
    (cond ((null u) nil)
          ((string= u "CASCADE") :cascade)
          ((string= u "SET NULL") :set-null)
          ((string= u "SET DEFAULT") :set-default)
          ((string= u "RESTRICT") :restrict)
          (t nil))))

(defun %col-node (name)
  (sql-query:col (%keywordize name)))

(defun %sqlite-foreign-key-constraints (connection table)
  "TABLE's own foreign keys as sql-query FOREIGN KEY table constraints."
  (let ((groups '()))
    (dolist (row (%catalog-rows connection :sqlite3 :foreign-keys :table table))
      (let ((group (assoc (getf row :id) groups)))
        (if group
            (push row (cdr group))
            (push (list (getf row :id) row) groups))))
    ;; SQLite numbers foreign keys last-declared first: declare them in
    ;; descending id order so the rebuilt table numbers them the same.
    (loop for (nil . rows) in groups
          for parts = (reverse rows)
          for first = (first parts)
          collect (sql-query:foreign-key
                   (mapcar (lambda (r) (%col-node (getf r :from_column))) parts)
                   :references (cons (getf first :parent)
                                     (loop for r in parts
                                           for to = (getf r :to_column)
                                           unless (%sql-null-p to)
                                             collect (%col-node to)))
                   :on-delete (%referential-action (getf first :on_delete))
                   :on-update (%referential-action (getf first :on_update))))))

(defun %sqlite-key-constraints (connection table)
  "→ (values CONSTRAINTS COMPOSITE-PK-P): a table PRIMARY KEY when TABLE's key
has several columns (in key order), and a table UNIQUE per multi-column
UNIQUE constraint."
  (let* ((pk-columns
           (mapcar (lambda (r) (getf r :name))
                   (sort (remove-if-not (lambda (r)
                                          (let ((p (getf r :pk))) (and (integerp p) (plusp p))))
                                        (%catalog-rows connection :sqlite3 :columns :table table))
                         #'< :key (lambda (r) (getf r :pk)))))
         (composite (and (rest pk-columns) t))
         (out '()))
    (when composite
      (push (apply #'sql-query:primary-key (mapcar #'%col-node pk-columns)) out))
    (dolist (index (%catalog-rows connection :sqlite3 :indexes :table table))
      (when (and (%truthy (getf index :is_unique))
                 (equal (getf index :origin) "u"))
        (let ((cols (mapcar (lambda (r) (getf r :name))
                            (%catalog-rows connection :sqlite3 :index-columns
                                           :table (getf index :name)))))
          (when (rest cols)
            (push (apply #'sql-query:unique-key (mapcar #'%col-node cols)) out)))))
    (values (nreverse out) composite)))

(defun %sqlite-incoming-losses (connection table)
  "With PRAGMA foreign_keys ON, a description per foreign key into TABLE."
  (when (%truthy (getf (first (%catalog-rows connection :sqlite3 :foreign-keys-enabled))
                       :foreign_keys))
    (loop for row in (%catalog-rows connection :sqlite3 :referencing :table table)
          collect (format nil "foreign key ~A(~A) into ~A while PRAGMA foreign_keys is ON"
                          (getf row :child) (getf row :from_column) table))))

(defmethod dialect-schema-op-statements ((dialect-key (eql :sqlite3)) (op alter-column-op)
                                         direction connection)
  "SQLite has no ALTER COLUMN: rebuild the table from its live definition with
the column altered — CREATE <t>__sql_orm_rebuild (columns, then the table's
PRIMARY KEY / UNIQUE / CHECK / FOREIGN KEY constraints), INSERT … SELECT, DROP,
RENAME, then the table's indexes and triggers from their sqlite_master SQL.
Signals SQLITE-REBUILD-UNSUPPORTED (restart CONTINUE: rebuild anyway) when the
table has what the rebuild cannot keep. Runs inside the caller's transaction;
reflection opens none."
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
         (names (mapcar (lambda (c) (%keywordize (column-info-name c))) old-columns))
         (ddl (getf (first (%catalog-rows connection :sqlite3 :table-sql :table table)) :sql)))
    (multiple-value-bind (checks lost) (if (stringp ddl)
                                           (%sqlite-table-features ddl)
                                           (values '() '()))
      (let ((lost (append lost (%sqlite-incoming-losses connection table))))
        (when lost
          (restart-case
              (error 'sqlite-rebuild-unsupported
                     :table table :objects lost :op op
                     :message (format nil "rebuilding ~A would lose: ~{~A~^; ~}" table lost))
            (continue ()
              :report "Rebuild the table anyway, losing the listed objects."
              nil))))
      (multiple-value-bind (keys composite-pk) (%sqlite-key-constraints connection table)
        (let ((columns (if composite-pk
                           ;; The key is a table constraint; column PRIMARY KEY would be a second one.
                           (mapcar (lambda (c)
                                     (let ((c (copy-column-info c)))
                                       (setf (column-info-primary-key c) nil)
                                       c))
                                   new-columns)
                           new-columns))
              (objects (%catalog-rows connection :sqlite3 :table-objects :table table)))
          (append
           (list (apply #'create-table tmp
                        (append (mapcar #'%column→table-column columns)
                                keys
                                (mapcar (lambda (check)
                                          (sql-query:check (sql-query:sql-raw (cdr check))
                                                           :name (car check)))
                                        checks)
                                (%sqlite-foreign-key-constraints connection table)))
                 (insert-into tmp
                              (apply #'columns names)
                              (select (apply #'columns names) (from table)))
                 (drop-table table)
                 (alter-table tmp (sql-query:rename-to table)))
           (mapcar (lambda (row) (sql-query:sql-raw (getf row :sql))) objects)))))))
