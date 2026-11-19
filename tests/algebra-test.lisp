(in-package #:sql-orm/tests)

;;; sql-orm 0.2.0 S1 — pure algebra: apply/diff round trip, inverses,
;;; alter-column-op, equality, footprints. No database.

;;; ---------------------------------------------------------------------------
;;; Laws over the corpus
;;; ---------------------------------------------------------------------------

(deftest-parametrize apply-diff-roundtrip
    (id :ids (corpus-ids) :rows (corpus-ids))
  "apply-ops-to-snapshot(a, diff-schema(a, b, :complete t)) = b, and A is untouched."
  (multiple-value-bind (a b) (corpus-pair id)
    (let* ((a-before (copy-snapshot a))
           (b-before (copy-snapshot b))
           (ops (diff-schema a b :complete t))
           (result (apply-ops-to-snapshot a ops)))
      (ok (schema-snapshot-equal result b))
      (ok (equalp a a-before) "input snapshot not mutated")
      (ok (equalp b b-before) "target snapshot not mutated")
      (let ((fresh (snapshot-column-infos result)))
        (ok (notany (lambda (c) (member c fresh :test #'eq))
                    (append (snapshot-column-infos a) (snapshot-column-infos b)))
            "result shares no column-info with the inputs")))))

(deftest-parametrize inverse-roundtrip
    (id :ids (corpus-ids) :rows (corpus-ids))
  "Folding the inverted ops (reverse order) over the result gives A back."
  (multiple-value-bind (a b) (corpus-pair id)
    (let* ((ops (diff-schema a b :complete t))
           (mig (make-instance 'schema-migration :ops ops))
           (there (apply-ops-to-snapshot a ops))
           (back (apply-ops-to-snapshot there (migration-downgrade-ops mig))))
      (ok (schema-snapshot-equal back a))
      (ok (schema-snapshot-equal (apply-ops-to-snapshot a mig) b)
          "a SCHEMA-MIGRATION is accepted as OPS"))))

(deftest-parametrize double-inverse
    (id :ids (corpus-ids) :rows (corpus-ids))
  "invert-schema-op is an involution on the corpus ops."
  (multiple-value-bind (a b) (corpus-pair id)
    (let* ((ops (diff-schema a b :complete t))
           (twice (mapcar (lambda (op) (invert-schema-op (invert-schema-op op))) ops)))
      (ok (equal (op-classes twice) (op-classes ops)))
      (ok (equal (mapcar #'schema-op-footprint twice)
                 (mapcar #'schema-op-footprint ops)))
      (ok (schema-snapshot-equal (apply-ops-to-snapshot a twice) b)))))

(deftest-parametrize diff-kinds
    (id :ids (corpus-ids) :rows (corpus-ids))
  "Locked op order for :complete t; the default (0.1) policy is unchanged."
  (destructuring-bind (a b complete-kinds default-kinds) (corpus-kinds id)
    (ok (equal (op-classes (diff-schema a b)) default-kinds) "default diff unchanged")
    (ok (equal (op-classes (diff-schema a b :complete t)) complete-kinds))))

(deftest diff-complete-ignores-drop-flags
  (multiple-value-bind (a b) (corpus-pair "multi-table")
    (ok (equal (op-classes (diff-schema a b :complete t))
               (op-classes (diff-schema a b :complete t
                                            :drop-tables nil :drop-columns nil))))))

(deftest diff-complete-dialect-equality
  "With :dialect, columns equal under that dialect are not altered."
  (let ((a (list (tbl :flags (col "id" :integer :primary-key t) (col "on" :boolean))))
        (b (list (tbl :flags (col "id" :integer :primary-key t) (col "on" :integer)))))
    (ok (equal (op-classes (diff-schema a b :complete t)) '(alter-column-op)))
    (ok (null (diff-schema a b :complete t :dialect :sqlite3))
        "BOOLEAN and INTEGER are both INTEGER on SQLite")))

;;; ---------------------------------------------------------------------------
;;; apply-ops-to-snapshot semantics
;;; ---------------------------------------------------------------------------

(defun %names (columns)
  (mapcar (lambda (c) (string-downcase (column-info-name c))) columns))

(defun %table-names (snapshot)
  (mapcar (lambda (e) (string-downcase (string (car e)))) snapshot))

(deftest apply-ops-positions
  (let* ((s (list (tbl :a (col "x" :integer) (col "y" :text))
                  (tbl :b (col "z" :text))))
         (to-x (col "x" :bigint :not-null t :default 0))
         (ops (list (make-instance 'create-table-op :table-name :c
                                   :columns (list (col "k" :integer)))
                    (make-instance 'add-column-op :table-name :a :column (col "w" :real))
                    (make-instance 'alter-column-op :table-name :a
                                   :from (col "x" :integer) :to to-x)
                    (make-instance 'drop-column-op :table-name :a :column (col "y" :text))))
         (r (apply-ops-to-snapshot s ops)))
    (ok (equal (%table-names r) '("a" "b" "c")) "create appends the table")
    (ok (equal (%names (cdr (first r))) '("x" "w")) "add appends; alter keeps position")
    (let ((x (first (cdr (first r)))))
      (ok (eq (column-info-type x) :bigint))
      (ok (column-info-not-null x))
      (ok (eql (column-info-default x) 0))
      (ok (not (eq x to-x)) "alter stores a copy of TO"))
    (let ((r2 (apply-ops-to-snapshot
               r (list (make-instance 'drop-table-op :table-name "A"
                                      :columns (cdr (first r)))))))
      (ok (equal (%table-names r2) '("b" "c")) "drop matches names STRING-EQUAL"))
    (ok (equal (%table-names s) '("a" "b")) "input untouched")
    (ok (equal (%names (cdr (first s))) '("x" "y")))))

(defun conflict-rows ()
  "Rows are thunks returning (values snapshot op): :ROWS values are spliced
in as forms, and building an ALTER-COLUMN-OP runs its validation."
  (flet ((case-of (make-op)
           (lambda () (values (list (items-v1)) (funcall make-op)))))
    (list
     (case-of (lambda () (make-instance 'create-table-op :table-name :items
                                                         :columns (list (col "id" :integer)))))
     (case-of (lambda () (make-instance 'drop-table-op :table-name :ghosts :columns '())))
     (case-of (lambda () (make-instance 'add-column-op :table-name :items
                                                       :column (col "QTY" :integer))))
     (case-of (lambda () (make-instance 'add-column-op :table-name :ghosts
                                                       :column (col "x" :integer))))
     (case-of (lambda () (make-instance 'drop-column-op :table-name :items
                                                        :column (col "nope" :text))))
     (case-of (lambda () (make-instance 'alter-column-op :table-name :items
                                                         :from (col "nope" :text)
                                                         :to (col "nope" :integer)))))))

(deftest-parametrize apply-ops-conflict
    (make-case
     :ids '("create-existing" "drop-missing-table" "add-existing-column"
            "add-to-missing-table" "drop-missing-column" "alter-missing-column")
     :rows (conflict-rows))
  (multiple-value-bind (snapshot op) (funcall make-case)
    (ok (signals (apply-ops-to-snapshot snapshot (list op)) 'snapshot-conflict))
    (handler-case (progn (apply-ops-to-snapshot snapshot (list op))
                         (ok nil "no condition signalled"))
      (snapshot-conflict (c)
        (ok (eq (snapshot-conflict-op c) op))
        (ok (typep c 'orm-error))))))

;;; ---------------------------------------------------------------------------
;;; alter-column-op
;;; ---------------------------------------------------------------------------

(defun invalid-alter-rows ()
  (list
   (list (col "a" :integer) (col "b" :integer))
   (list (col "a" :integer) (col "a" :integer :primary-key t))
   (list (col "a" :integer) (col "a" :integer :unique t))
   (list (col "a" :integer :primary-key t) (col "a" :integer :primary-key t :autoincrement t))))

(deftest-parametrize alter-column-op-validation
    ((from to) :ids '("name" "primary-key" "unique" "autoincrement") :rows (invalid-alter-rows))
  (ok (signals (make-instance 'alter-column-op :table-name :t :from from :to to)
               'invalid-schema-op)))

(deftest alter-column-op-shape
  (let* ((from (col "qty" :integer))
         (to (col "QTY" :bigint :not-null t :default 1))
         (op (make-instance 'alter-column-op :table-name :items :from from :to to))
         (inv (invert-schema-op op)))
    (ok (typep op 'schema-op))
    (ok (string-equal (schema-op-column-name op) "qty"))
    (ok (eq (schema-op-from-column op) from))
    (ok (eq (schema-op-to-column op) to))
    (ok (typep inv 'alter-column-op))
    (ok (eq (column-info-type (schema-op-from-column inv)) :bigint) "inverse swaps FROM and TO")
    (ok (eq (column-info-type (schema-op-to-column inv)) :integer))
    (ok (not (eq (schema-op-to-column inv) from)) "inverse copies its columns")))

(deftest alter-column-op-ddl
  "Portable (ANSI / PostgreSQL) DDL: one action per changed attribute."
  (let* ((op (make-instance 'alter-column-op :table-name :items
                            :from (col "qty" :integer)
                            :to (col "qty" :bigint :not-null t :default 1)))
         (up (schema-op-upgrade op))
         (down (schema-op-downgrade op))
         (up-sql (ops-sql up))
         (down-sql (ops-sql down)))
    (ok (every (lambda (s) (typep s 'sql-query:alter-table-statement)) up))
    (ok (every (lambda (s) (typep s 'sql-query:alter-table-statement)) down))
    (ok (contains "ALTER TABLE \"items\"" up-sql))
    (ok (contains "SET DATA TYPE BIGINT" up-sql))
    (ok (contains "SET NOT NULL" up-sql))
    (ok (contains "SET DEFAULT 1" up-sql))
    (ok (contains "SET DATA TYPE INTEGER" down-sql))
    (ok (contains "DROP NOT NULL" down-sql))
    (ok (contains "DROP DEFAULT" down-sql))
    (ok (contains "SET DATA TYPE BIGINT" (ops-sql up :postgres)) "PostgreSQL accepts SET DATA TYPE"))
  (let* ((op (make-instance 'alter-column-op :table-name :items
                            :from (col "label" :text :default "a")
                            :to (col "label" :text :default "b")))
         (sql (ops-sql (schema-op-upgrade op))))
    (ok (contains "SET DEFAULT 'b'" sql) "string default is a literal, not a column")
    (ok (not (contains "DATA TYPE" sql)) "unchanged type emits no SET DATA TYPE")
    (ok (not (contains "NOT NULL" sql)) "unchanged nullability emits nothing")))

;;; ---------------------------------------------------------------------------
;;; Equality
;;; ---------------------------------------------------------------------------

(defun column-equal-rows ()
  (list
   (list (col "a" :text) (col "a" :string) nil t)
   (list (col "a" :text) (col "a" "TEXT") :sqlite3 t)
   (list (col "a" :text) (col "a" "TEXT") nil nil)
   (list (col "a" :boolean) (col "a" "INTEGER") :sqlite3 t)
   (list (col "a" (list :varchar 64)) (col "a" "character varying(64)") :postgres t)
   (list (col "a" :timestamptz) (col "a" "timestamp with time zone") :postgres t)
   (list (col "a" :integer) (col "a" :text) nil nil)
   (list (col "a" :integer :primary-key t) (col "a" :integer :primary-key t :not-null t) nil t)
   (list (col "a" :integer) (col "a" :integer :not-null t) nil nil)
   (list (col "a" :integer :default 0) (col "a" "INTEGER" :default (sql-query:sql-raw "0")) :sqlite3 t)
   (list (col "a" :text :default "x") (col "a" "TEXT" :default (sql-query:sql-raw "'x'")) :sqlite3 t)
   (list (col "a" :boolean :default t) (col "a" "INTEGER" :default (sql-query:sql-raw "1")) :sqlite3 t)
   (list (col "a" :integer :default 0) (col "a" :integer) nil nil)
   (list (col "a" :integer :default 0) (col "a" :integer :default 1) nil nil)
   (list (col "QTY" :integer) (col "qty" :integer) nil t)
   (list (col "a" :integer) (col "b" :integer) nil nil)
   (list (col "a" :integer) (col "a" :integer :unique t) nil nil)
   (list (col "a" :integer :primary-key t) (col "a" :integer :primary-key t :autoincrement t) nil nil)))

(deftest-parametrize column-info-equality
    ((a b dialect expected)
     :ids '("text-string-ansi" "text-sqlite" "text-ansi-vs-raw" "boolean-sqlite"
           "varchar-postgres" "timestamptz-postgres" "type-differs" "pk-implies-not-null"
           "not-null-differs" "default-int-raw" "default-string-raw" "default-bool-raw"
           "default-vs-none" "default-differs" "name-case" "name-differs"
           "unique-differs" "autoincrement-differs")
     :rows (column-equal-rows))
  (ok (eq expected (and (column-info-equal a b :dialect dialect) t)))
  (ok (eq expected (and (column-info-equal b a :dialect dialect) t)) "symmetric"))

(deftest snapshot-equality
  (ok (schema-snapshot-equal (list (items-v1) (notes-v1)) (list (notes-v1) (items-v1)))
      "table order is not significant")
  (multiple-value-bind (a b) (corpus-pair "reorder-only")
    (ok (schema-snapshot-equal a b) "column order is not significant"))
  (multiple-value-bind (a b) (corpus-pair "name-case")
    (ok (schema-snapshot-equal a b) "names compare STRING-EQUAL"))
  (ok (not (schema-snapshot-equal (list (items-v1)) (list (items-v1) (notes-v1)))))
  (ok (not (schema-snapshot-equal (list (items-v1)) (list (items-with (col "sku" :text))))))
  (ok (not (schema-snapshot-equal (list (items-v1)) (list (items-without "qty")))))
  (ok (schema-snapshot-equal '() '())))

(defun canonical-rows ()
  '((:ansi " integer " "INTEGER")
    (:ansi "varchar ( 64 )" "VARCHAR(64)")
    (:ansi "double   precision" "DOUBLE PRECISION")
    (:ansi "numeric(10, 2)" "NUMERIC(10,2)")
    (:sqlite3 "text" "TEXT")
    (:postgres "character varying(64)" "VARCHAR(64)")
    (:postgres "character(3)" "CHAR(3)")
    (:postgres "timestamp with time zone" "TIMESTAMPTZ")
    (:postgres "timestamp without time zone" "TIMESTAMP")
    (:postgres "time without time zone" "TIME")
    (:postgres "int4" "INTEGER")
    (:postgres "int" "INTEGER")
    (:postgres "int8" "BIGINT")
    (:postgres "int2" "SMALLINT")
    (:postgres "bool" "BOOLEAN")
    (:postgres "float8" "DOUBLE PRECISION")
    (:postgres "float4" "REAL")
    (:postgres "numeric(10, 2)" "NUMERIC(10,2)")
    (:postgres "TEXT" "TEXT")))

(deftest-parametrize canonical-type
    ((key type-sql expected) :rows (canonical-rows))
  (ok (string= (canonical-type-sql key type-sql) expected)))

(deftest dialect-keys
  (ok (eq (schema-dialect-key nil) :ansi))
  (ok (eq (schema-dialect-key :postgres) :postgres))
  (ok (eq (schema-dialect-key (dialect :sqlite3)) :sqlite3))
  (ok (eq (schema-dialect-key (dialect :postgres)) :postgres))
  (ok (eq (schema-dialect-key (sql-query:make-ansi-dialect)) :ansi)))

;;; ---------------------------------------------------------------------------
;;; Footprints
;;; ---------------------------------------------------------------------------

(defun footprint-rows ()
  "Rows are thunks returning (values op expected-footprint)."
  (list
   (lambda () (values (make-instance 'create-table-op :table-name :items
                                                      :columns (cdr (items-v1)))
                      (list "table:items")))
   (lambda () (values (make-instance 'drop-table-op :table-name "Items"
                                                    :columns (cdr (items-v1)))
                      (list "table:items")))
   (lambda () (values (make-instance 'add-column-op :table-name :items
                                                    :column (col "SKU" :text))
                      (list "column:items.sku")))
   (lambda () (values (make-instance 'drop-column-op :table-name :widgets
                                                     :column (col "color" :text))
                      (list "column:widgets.color")))
   (lambda () (values (make-instance 'alter-column-op :table-name :items
                                                      :from (col "qty" :integer)
                                                      :to (col "qty" :text))
                      (list "column:items.qty")))))

(deftest-parametrize footprints
    (make-case :ids '("create-table" "drop-table" "add-column" "drop-column" "alter-column")
     :rows (footprint-rows))
  (multiple-value-bind (op expected) (funcall make-case)
    (ok (equal (schema-op-footprint op) expected))
    (ok (equal (schema-op-footprint (invert-schema-op op)) expected)
        "inverse has the same footprint")
    (let ((fp (schema-op-footprint op)))
      (ok (not (eq fp (schema-op-footprint op))) "fresh list each call"))))
