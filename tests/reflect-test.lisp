(in-package #:sql-orm/tests)

;;; sql-orm 0.2.0 S1 — reflection (SQLite live in memory; PostgreSQL through
;;; generated queries and a canned connection) and alter-column on SQLite.

(defmacro with-sqlite ((var) &body body)
  `(with-orm-connection (,var :driver :sqlite3 :database-name ":memory:")
     ,@body))

(defun rows (connection sql)
  (sql-protocol:fetch-all (sql-protocol:execute connection sql)))

;;; ---------------------------------------------------------------------------
;;; SQLite: reflect-schema after upgrade-schema = the snapshot
;;; ---------------------------------------------------------------------------
(deftest-parametrize reflect-after-create
    (id :ids (corpus-ids :live t) :rows (corpus-ids :live t))
  "Both sides of every live pair, created from nothing, reflect back equal."
  (multiple-value-bind (a b) (corpus-pair id)
    (dolist (snapshot (list a b))
      (with-sqlite (c)
        (upgrade-schema c (diff-schema nil snapshot))
        (let ((live (reflect-schema c)))
          (ok (schema-snapshot-equal live snapshot :dialect :sqlite3))
          (ok (every (lambda (e) (stringp (car e))) live) "reflected table names are strings")
          (ok (equal (mapcar #'car live)
                     (sort (mapcar #'car live) #'string<))
              "tables ordered by name"))))))

(deftest-parametrize reflect-after-complete-diff
    (id :ids (corpus-ids :live t) :rows (corpus-ids :live t))
  "upgrade a; upgrade diff(a, b, :complete t) → b; downgrade it → a."
  (multiple-value-bind (a b) (corpus-pair id)
    (with-sqlite (c)
      (upgrade-schema c (diff-schema nil a))
      (let ((ops (diff-schema a b :complete t)))
        (upgrade-schema c ops)
        (ok (schema-snapshot-equal (reflect-schema c) b :dialect :sqlite3) "upgraded to b")
        (downgrade-schema c ops)
        (ok (schema-snapshot-equal (reflect-schema c) a :dialect :sqlite3) "downgraded to a")))))

(defun gadgets ()
  (tbl :gadgets
       (col "id" :integer :primary-key t :autoincrement t)
       (col "name" :text :not-null t :unique t)
       (col "qty" :integer :default 0)
       (col "label" :text :default "x")
       (col "ratio" :real)
       (col "data" :blob)))

(deftest reflect-sqlite-shape
  "Exact column-infos read from PRAGMA table_info / index_list / sqlite_master."
  (with-sqlite (c)
    (upgrade-schema c (diff-schema nil (list (gadgets))))
    (let* ((live (reflect-schema c))
           (entry (first live))
           (cols (cdr entry)))
      (ok (= 1 (length live)) "sqlite_sequence and sqlite_* tables are not reflected")
      (ok (equal "gadgets" (car entry)))
      (ok (equal (mapcar #'column-info-name cols)
                 '("id" "name" "qty" "label" "ratio" "data"))
          "columns in table order")
      (destructuring-bind (id name qty label ratio data) cols
        (ok (equal (column-info-type id) "INTEGER") "declared type, verbatim")
        (ok (column-info-primary-key id))
        (ok (column-info-autoincrement id) "AUTOINCREMENT read from sqlite_master")
        (ok (not (column-info-unique id)))
        (ok (null (column-info-default id)))
        (ok (equal (column-info-type name) "TEXT"))
        (ok (column-info-not-null name))
        (ok (column-info-unique name) "UNIQUE from a single-column origin-u index")
        (ok (not (column-info-primary-key name)))
        (ok (not (column-info-autoincrement name)))
        (ok (typep (column-info-default qty) 'sql-query:sql-node) "defaults are SQL nodes")
        (ok (equal (sql-text (column-info-default qty) :sqlite3) "0"))
        (ok (equal (sql-text (column-info-default label) :sqlite3) "'x'"))
        (ok (not (column-info-not-null qty)))
        (ok (equal (column-info-type ratio) "REAL"))
        (ok (equal (column-info-type data) "BLOB"))))))

(deftest reflect-exclude
  (with-sqlite (c)
    (upgrade-schema c (diff-schema nil (list (items-v1) (notes-v1) (tags-v1))))
    (ok (equal (mapcar #'car (reflect-schema c)) '("items" "notes" "tags")))
    (ok (equal (mapcar #'car (reflect-schema c :exclude '(:notes "TAGS"))) '("items"))
        "EXCLUDE names compare STRING-EQUAL")
    (ok (equal (mapcar #'car (reflect-schema c :exclude (lambda (name)
                                                          (char= (char name 0) #\i))))
               '("notes" "tags"))
        "EXCLUDE may be a predicate on the name string")))

(deftest reflect-sqlite-dialect-key
  (with-sqlite (c)
    (ok (eq (schema-dialect-key (sql-query:dialect-for-connection c)) :sqlite3))))

;;; ---------------------------------------------------------------------------
;;; SQLite: alter-column end to end (table rebuild)
;;; ---------------------------------------------------------------------------

(defun gadget-table (qty)
  (list (tbl :gadgets
             (col "id" :integer :primary-key t :autoincrement t)
             (col "name" :text :not-null t)
             qty)))

(deftest alter-column-sqlite-nullability-default
  (let* ((from (col "qty" :integer))
         (to (col "qty" :integer :not-null t :default 1))
         (base (gadget-table (col "qty" :integer)))
         (op (make-instance 'alter-column-op :table-name :gadgets :from from :to to)))
    (with-sqlite (c)
      (upgrade-schema c (diff-schema nil base))
      (sql-protocol:execute c "INSERT INTO gadgets (name, qty) VALUES (?, ?)" '("a" 5))
      (sql-protocol:execute c "INSERT INTO gadgets (name, qty) VALUES (?, ?)" '("b" 7))
      ;; upgrade (in a transaction, the default)
      (upgrade-schema c (list op))
      (ok (schema-snapshot-equal (reflect-schema c) (apply-ops-to-snapshot base (list op))
                                 :dialect :sqlite3)
          "live schema = folded snapshot")
      (ok (equal (mapcar (lambda (r) (list (getf r :name) (getf r :qty)))
                         (rows c "SELECT name, qty FROM gadgets ORDER BY id"))
                 '(("a" 5) ("b" 7)))
          "rows survive the rebuild")
      (ok (signals (sql-protocol:execute c "INSERT INTO gadgets (name, qty) VALUES ('n', NULL)")
                   'error)
          "NOT NULL now enforced")
      (sql-protocol:execute c "INSERT INTO gadgets (name) VALUES ('d')")
      (ok (eql 1 (getf (first (rows c "SELECT qty FROM gadgets WHERE name = 'd'")) :qty))
          "DEFAULT 1 now applies")
      ;; downgrade
      (downgrade-schema c (list op))
      (ok (schema-snapshot-equal (reflect-schema c) base :dialect :sqlite3) "back to base")
      (sql-protocol:execute c "INSERT INTO gadgets (name, qty) VALUES ('n', NULL)")
      (ok (equal (mapcar (lambda (r) (list (getf r :name) (getf r :qty)))
                         (rows c "SELECT name, qty FROM gadgets ORDER BY id"))
                 '(("a" 5) ("b" 7) ("d" 1) ("n" nil)))
          "rows survive the second rebuild; NULL allowed again")
      (sql-protocol:execute c "INSERT INTO gadgets (name) VALUES ('e')")
      (ok (null (getf (first (rows c "SELECT qty FROM gadgets WHERE name = 'e'")) :qty))
          "no default after downgrade"))))

(deftest alter-column-sqlite-type
  (let* ((from (col "qty" :integer))
         (to (col "qty" :text))
         (base (gadget-table (col "qty" :integer)))
         (op (make-instance 'alter-column-op :table-name :gadgets :from from :to to)))
    (with-sqlite (c)
      (upgrade-schema c (diff-schema nil base))
      (sql-protocol:execute c "INSERT INTO gadgets (name, qty) VALUES (?, ?)" '("a" 5))
      (upgrade-schema c (list op))
      (let ((qty (find "qty" (cdr (first (reflect-schema c)))
                       :key #'column-info-name :test #'string-equal)))
        (ok (equal (column-info-type qty) "TEXT")))
      (ok (equal (getf (first (rows c "SELECT typeof(qty) AS ty FROM gadgets")) :ty) "text")
          "TEXT affinity converted the stored integer")
      (ok (equal (getf (first (rows c "SELECT name FROM gadgets WHERE id = 1")) :name) "a")
          "primary key values kept")
      (downgrade-schema c (list op))
      (ok (schema-snapshot-equal (reflect-schema c) base :dialect :sqlite3))
      (ok (eql 5 (getf (first (rows c "SELECT qty FROM gadgets")) :qty))
          "INTEGER affinity converted it back")
      (sql-protocol:execute c "INSERT INTO gadgets (name, qty) VALUES ('b', 1)")
      (ok (eql 2 (getf (first (rows c "SELECT id FROM gadgets WHERE name = 'b'")) :id))
          "AUTOINCREMENT still continues from the kept rows"))))

(deftest alter-column-sqlite-rolls-back
  "A rebuild that fails (NULLs under a new NOT NULL) leaves the table as it was."
  (let* ((base (gadget-table (col "qty" :integer)))
         (op (make-instance 'alter-column-op :table-name :gadgets
                            :from (col "qty" :integer)
                            :to (col "qty" :integer :not-null t))))
    (with-sqlite (c)
      (upgrade-schema c (diff-schema nil base))
      (sql-protocol:execute c "INSERT INTO gadgets (name, qty) VALUES ('a', NULL)")
      (ok (signals (upgrade-schema c (list op)) 'error))
      (ok (schema-snapshot-equal (reflect-schema c) base :dialect :sqlite3))
      (ok (= 1 (length (rows c "SELECT * FROM gadgets"))))
      (ok (null (rows c "SELECT name FROM sqlite_master WHERE name <> 'gadgets' AND name NOT LIKE 'sqlite%'"))
          "no rebuild table left behind"))))

;;; ---------------------------------------------------------------------------
;;; PostgreSQL: generated queries (no server)
;;; ---------------------------------------------------------------------------

(deftest postgres-reflection-queries
  (multiple-value-bind (sql params) (reflection-query :postgres :tables)
    (ok (stringp sql))
    (ok (contains "information_schema.tables" sql))
    (ok (contains "BASE TABLE" sql))
    (ok (contains "table_schema = $1" sql))
    (ok (contains "ORDER BY" sql))
    (ok (not (contains "?" sql)) "PostgreSQL placeholders are $n")
    (ok (equal params '("public")) "default schema"))
  (multiple-value-bind (sql params) (reflection-query :postgres :tables :schema "app")
    (declare (ignore sql))
    (ok (equal params '("app"))))
  (multiple-value-bind (sql params) (reflection-query :postgres :columns)
    (ok (contains "pg_catalog.pg_attribute" sql))
    (ok (contains "format_type" sql))
    (ok (contains "attnum > 0" sql))
    (ok (contains "attisdropped" sql))
    (ok (contains "pg_get_expr" sql))
    (ok (contains "attidentity" sql))
    (ok (contains "indisprimary" sql))
    (ok (contains "indisunique" sql))
    (ok (contains "$1" sql))
    (ok (not (contains "?" sql)))
    (dolist (alias '("table_name" "column_name" "data_type" "not_null" "column_default"
                     "identity" "is_primary_key" "is_unique"))
      (ok (contains (format nil "AS ~A" alias) sql) alias))
    (ok (equal params '("public")))))

(defun pg-rows ()
  "What cl-dbi/cl-postgres return for the canned schema: plist keys upcased,
booleans T / NIL, SQL NULL as NIL or :NULL."
  (values
   '((:table_name "items") (:table_name "notes") (:table_name "sql_migrate_version"))
   '((:table_name "items" :column_name "id" :data_type "integer" :not_null t
      :column_default "nextval('items_id_seq'::regclass)" :identity ""
      :is_primary_key t :is_unique nil)
     (:table_name "items" :column_name "name" :data_type "character varying(64)" :not_null t
      :column_default :null :identity "" :is_primary_key nil :is_unique t)
     (:table_name "items" :column_name "qty" :data_type "integer" :not_null nil
      :column_default "0" :identity "" :is_primary_key nil :is_unique nil)
     (:table_name "items" :column_name "label" :data_type "text" :not_null nil
      :column_default "'a::b'::character varying" :identity "" :is_primary_key nil :is_unique nil)
     (:table_name "notes" :column_name "id" :data_type "bigint" :not_null t
      :column_default nil :identity "d" :is_primary_key t :is_unique nil)
     (:table_name "notes" :column_name "body" :data_type "timestamp with time zone" :not_null nil
      :column_default nil :identity "" :is_primary_key nil :is_unique nil)
     (:table_name "sql_migrate_version" :column_name "version_num" :data_type "text"
      :not_null t :column_default nil :identity "" :is_primary_key t :is_unique nil))))

(defun canned-postgres ()
  (multiple-value-bind (table-rows column-rows) (pg-rows)
    (make-instance 'canned-connection
                   :responses (list (cons (reflection-query :postgres :tables) table-rows)
                                    (cons (reflection-query :postgres :columns) column-rows)))))

(deftest postgres-reflect-canned
  (let* ((c (canned-postgres))
         (live (reflect-schema c :dialect :postgres :exclude '("sql_migrate_version")))
         (expected (list (tbl :items
                              (col "id" :integer :primary-key t :autoincrement t)
                              (col "name" (list :varchar 64) :not-null t :unique t)
                              (col "qty" :integer :default 0)
                              (col "label" :text :default "a::b"))
                         (tbl :notes
                              (col "id" :bigint :primary-key t :autoincrement t)
                              (col "body" :timestamptz)))))
    (ok (equal (mapcar #'car live) '("items" "notes")))
    (ok (schema-snapshot-equal live expected :dialect :postgres))
    (ok (= 2 (length (canned-log c))) "one :tables and one :columns query")
    (ok (every (lambda (entry) (equal (second entry) '("public"))) (canned-log c)))
    (let ((cols (cdr (first live))))
      (ok (equal (column-info-type (second cols)) "character varying(64)")
          "type is format_type output, verbatim")
      (ok (null (column-info-default (first cols))) "nextval default → autoincrement, no default")
      (ok (column-info-autoincrement (first cols)))
      (ok (null (column-info-default (second cols))) ":NULL is no default")
      (ok (equal (sql-text (column-info-default (fourth cols)) :postgres) "'a::b'")
          "trailing casts stripped, quoted :: kept"))
    (ok (column-info-autoincrement (first (cdr (second live)))) "identity column")))

(deftest reflection-unsupported-dialect
  (let ((c (make-instance 'canned-connection :responses '())))
    (ok (signals (reflect-schema c :dialect :ansi) 'reflection-unsupported))
    (handler-case (reflect-schema c :dialect :ansi)
      (reflection-unsupported (e)
        (ok (eq (reflection-unsupported-dialect e) :ansi))
        (ok (typep e 'orm-error))))))
