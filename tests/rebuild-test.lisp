(in-package #:sql-orm/tests)

;;; sql-orm 0.3.0 — the SQLite alter-column rebuild keeps what the table had
;;; (issue #13): foreign keys, CHECK constraints, composite PRIMARY KEY /
;;; UNIQUE, indexes and triggers. What it cannot keep is refused with
;;; SQLITE-REBUILD-UNSUPPORTED (restart CONTINUE). Tables here are made with
;;; raw SQL: sql-orm DDL does not emit these objects.

(defun exec* (c &rest statements)
  (dolist (sql statements)
    (sql-protocol:execute c sql)))

(defun table-objects (c table)
  "(type name sql) of TABLE's indexes and triggers that have SQL, by name."
  (mapcar (lambda (r) (list (getf r :type) (getf r :name) (getf r :sql)))
          (rows c (format nil "SELECT type, name, sql FROM sqlite_master WHERE tbl_name = '~A' AND type IN ('index', 'trigger') AND sql IS NOT NULL ORDER BY name" table))))

(defun foreign-keys (c table)
  (rows c (format nil "SELECT id, seq, \"table\" AS parent, \"from\" AS child_col, \"to\" AS parent_col, on_update, on_delete, \"match\" AS m FROM pragma_foreign_key_list('~A') ORDER BY id, seq" table)))

(defun qty-op (table)
  "qty INTEGER → qty INTEGER NOT NULL DEFAULT 0 on TABLE."
  (make-instance 'alter-column-op :table-name table
                                  :from (col "qty" :integer)
                                  :to (col "qty" :integer :not-null t :default 0)))

(defun count-rows (c table)
  (getf (first (rows c (format nil "SELECT count(*) AS n FROM ~A" table))) :n))

(defun fails-p (c sql)
  (handler-case (progn (sql-protocol:execute c sql) nil)
    (error () t)))

(defun make-parents-children (c)
  (exec* c
         "CREATE TABLE parents (id INTEGER PRIMARY KEY, name TEXT, qty INTEGER)"
         "CREATE TABLE children (id INTEGER PRIMARY KEY, parent_id INTEGER NOT NULL REFERENCES parents (id) ON DELETE CASCADE, other_id INTEGER, qty INTEGER, FOREIGN KEY (other_id) REFERENCES parents (id) ON UPDATE SET NULL)"
         "INSERT INTO parents (id, name, qty) VALUES (1, 'p', 1)"
         "INSERT INTO children (id, parent_id, qty) VALUES (1, 1, 5)"))

(deftest rebuild-sqlite-keeps-foreign-keys
  (with-sqlite (c)
    (make-parents-children c)
    (let ((before (foreign-keys c "children")))
      (ok (= 2 (length before)))
      (upgrade-schema c (list (qty-op "children")))
      (ok (equalp before (foreign-keys c "children")) "same foreign keys after upgrade")
      (downgrade-schema c (list (qty-op "children")))
      (ok (equalp before (foreign-keys c "children")) "same foreign keys after downgrade"))
    (ok (= 1 (count-rows c "children")))
    (exec* c "PRAGMA foreign_keys = ON")
    (ok (fails-p c "INSERT INTO children (id, parent_id) VALUES (2, 99)") "enforced")
    (exec* c "DELETE FROM parents")
    (ok (= 0 (count-rows c "children")) "ON DELETE CASCADE kept")))

(deftest rebuild-sqlite-keeps-check-constraints
  (with-sqlite (c)
    (exec* c "CREATE TABLE prices (id INTEGER PRIMARY KEY, qty INTEGER CHECK (qty >= 0), price REAL, note TEXT DEFAULT 'check (x)', CONSTRAINT positive_price CHECK (price > 0 AND (price < 1000)))"
           "INSERT INTO prices (qty, price) VALUES (1, 2.5)")
    (upgrade-schema c (list (qty-op "prices")))
    (ok (fails-p c "INSERT INTO prices (qty, price) VALUES (-1, 1)") "column CHECK kept")
    (ok (fails-p c "INSERT INTO prices (qty, price) VALUES (1, 0)") "table CHECK kept")
    (ok (fails-p c "INSERT INTO prices (qty, price) VALUES (1, 5000)") "nested parens kept")
    (ok (not (fails-p c "INSERT INTO prices (qty, price) VALUES (2, 3)")))
    (ok (fails-p c "INSERT INTO prices (qty, price) VALUES (NULL, 3)") "the alter applied")
    (ok (search "positive_price"
                (getf (first (rows c "SELECT sql FROM sqlite_master WHERE name = 'prices'")) :sql))
        "constraint name kept")
    (downgrade-schema c (list (qty-op "prices")))
    (ok (fails-p c "INSERT INTO prices (qty, price) VALUES (-1, 1)") "kept after downgrade")
    (ok (= 2 (count-rows c "prices")))))

(deftest rebuild-sqlite-keeps-indexes-and-triggers
  (with-sqlite (c)
    (exec* c "CREATE TABLE gadgets (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL, qty INTEGER)"
           "CREATE TABLE gadget_log (gadget_id INTEGER, note TEXT)"
           "CREATE INDEX gadgets_name ON gadgets (name)"
           "CREATE INDEX gadgets_name_qty ON gadgets (name, qty DESC)"
           "CREATE UNIQUE INDEX gadgets_lower_name ON gadgets (lower(name))"
           "CREATE INDEX gadgets_big ON gadgets (qty) WHERE qty > 10"
           "CREATE TRIGGER gadgets_ins AFTER INSERT ON gadgets BEGIN INSERT INTO gadget_log VALUES (NEW.id, 'ins'); END"
           "INSERT INTO gadgets (name, qty) VALUES ('a', 1)")
    (let ((before (table-objects c "gadgets")))
      (ok (= 5 (length before)))
      (upgrade-schema c (list (qty-op "gadgets")))
      (ok (equal before (table-objects c "gadgets")) "indexes and trigger re-created as they were")
      (exec* c "INSERT INTO gadgets (name, qty) VALUES ('b', 2)")
      (ok (= 2 (count-rows c "gadget_log")) "trigger still fires")
      (ok (fails-p c "INSERT INTO gadgets (name, qty) VALUES ('A', 3)") "unique expression index kept")
      (downgrade-schema c (list (qty-op "gadgets")))
      (ok (equal before (table-objects c "gadgets")) "and after downgrade"))
    (ok (= 2 (count-rows c "gadgets")))))

(deftest rebuild-sqlite-keeps-composite-keys
  (with-sqlite (c)
    (exec* c "CREATE TABLE pairs (a INTEGER NOT NULL, b TEXT NOT NULL, c INTEGER, d INTEGER, qty INTEGER, PRIMARY KEY (a, b), UNIQUE (c, d))"
           "INSERT INTO pairs (a, b, c, d, qty) VALUES (1, 'x', 1, 1, 1)")
    (upgrade-schema c (list (qty-op "pairs")))
    (ok (equal '(("a" 1) ("b" 2))
               (mapcar (lambda (r) (list (getf r :name) (getf r :pk)))
                       (rows c "SELECT name, pk FROM pragma_table_info('pairs') WHERE pk > 0 ORDER BY pk")))
        "composite primary key kept, in key order")
    (ok (fails-p c "INSERT INTO pairs (a, b, c, d) VALUES (1, 'x', 2, 2)") "primary key enforced")
    (ok (not (fails-p c "INSERT INTO pairs (a, b, c, d) VALUES (1, 'y', 2, 2)")))
    (ok (fails-p c "INSERT INTO pairs (a, b, c, d) VALUES (2, 'z', 1, 1)") "composite UNIQUE kept")
    (ok (= 2 (count-rows c "pairs")))))

(deftest rebuild-sqlite-refuses-what-it-cannot-keep
  (with-sqlite (c)
    (exec* c "CREATE TABLE tags (id INTEGER PRIMARY KEY, label TEXT COLLATE NOCASE, qty INTEGER)"
           "INSERT INTO tags (label, qty) VALUES ('a', 1)")
    (let ((ddl (rows c "SELECT sql FROM sqlite_master WHERE name = 'tags'"))
          (e (handler-case (upgrade-schema c (list (qty-op "tags")))
               (sqlite-rebuild-unsupported (e) e))))
      (ok (typep e 'sqlite-rebuild-unsupported))
      (ok (typep e 'orm-error))
      (ok (equal "tags" (sqlite-rebuild-unsupported-table e)))
      (ok (find-if (lambda (s) (search "COLLATE" s)) (sqlite-rebuild-unsupported-objects e)))
      (ok (search "COLLATE" (princ-to-string e)))
      (ok (equal ddl (rows c "SELECT sql FROM sqlite_master WHERE name = 'tags'")) "table untouched"))
    ;; CONTINUE: rebuild anyway, losing the listed objects.
    (let ((restarts nil))
      (handler-bind ((sqlite-rebuild-unsupported
                       (lambda (e)
                         (setf restarts (mapcar #'restart-name (compute-restarts e)))
                         (continue e))))
        (upgrade-schema c (list (qty-op "tags"))))
      (ok (member 'continue restarts))
      (ok (fails-p c "INSERT INTO tags (label, qty) VALUES ('b', NULL)") "the alter applied")
      (ok (= 1 (count-rows c "tags"))))))

(deftest-parametrize rebuild-sqlite-refuses-table-features
    ((ddl word)
     ("CREATE TABLE t (id INTEGER PRIMARY KEY, qty INTEGER, d INTEGER GENERATED ALWAYS AS (qty * 2))" "generated")
     ("CREATE TABLE t (id INTEGER PRIMARY KEY, qty INTEGER) WITHOUT ROWID" "WITHOUT ROWID")
     ("CREATE TABLE t (id INTEGER PRIMARY KEY ON CONFLICT REPLACE, qty INTEGER)" "ON CONFLICT")
     ("CREATE TABLE t (id INTEGER PRIMARY KEY, p INTEGER REFERENCES t (id) DEFERRABLE INITIALLY DEFERRED, qty INTEGER)" "DEFERRABLE"))
  (with-sqlite (c)
    (exec* c ddl)
    (let ((e (handler-case (upgrade-schema c (list (qty-op "t")))
               (sqlite-rebuild-unsupported (e) e))))
      (ok (typep e 'sqlite-rebuild-unsupported) ddl)
      (ok (and (typep e 'sqlite-rebuild-unsupported)
               (search word (princ-to-string e) :test #'char-equal))
          word))))

(deftest rebuild-sqlite-referenced-table-with-foreign-keys-on
  (with-sqlite (c)
    (make-parents-children c)
    (exec* c "PRAGMA foreign_keys = ON")
    (let ((e (handler-case (upgrade-schema c (list (qty-op "parents")))
               (sqlite-rebuild-unsupported (e) e))))
      (ok (typep e 'sqlite-rebuild-unsupported)
          "dropping a referenced table with foreign_keys ON would cascade")
      (ok (and (typep e 'sqlite-rebuild-unsupported)
               (find-if (lambda (s) (search "children" s)) (sqlite-rebuild-unsupported-objects e)))))
    (ok (= 1 (count-rows c "children")) "child rows untouched")
    (exec* c "PRAGMA foreign_keys = OFF")
    (upgrade-schema c (list (qty-op "parents")))
    (ok (= 1 (count-rows c "children")) "foreign_keys OFF: rebuilt, child rows kept")
    (ok (= 2 (length (foreign-keys c "children"))))
    (ok (fails-p c "INSERT INTO parents (id, name, qty) VALUES (2, 'q', NULL)") "the alter applied")))

;;; Minor (#13): alter DDL compares types by their rendered SQL.

(deftest alter-column-type-compared-by-rendered-sql
  (let ((op (make-instance 'alter-column-op :table-name :items
                                            :from (col "qty" :integer)
                                            :to (col "qty" "integer" :not-null t))))
    (ok (= 1 (length (schema-op-upgrade op))) "no SET DATA TYPE for the same rendered type")
    (ok (= 1 (length (schema-op-downgrade op)))))
  (let ((op (make-instance 'alter-column-op :table-name :items
                                            :from (col "n" "int4")
                                            :to (col "n" :integer :default 1))))
    (ok (= 1 (length (dialect-schema-op-statements :postgres op :upgrade nil)))
        "PostgreSQL: int4 = INTEGER")
    (ok (= 2 (length (dialect-schema-op-statements :ansi op :upgrade nil)))
        "ANSI: int4 is another type"))
  (let ((op (make-instance 'alter-column-op :table-name :items
                                            :from (col "n" :integer)
                                            :to (col "n" :bigint))))
    (ok (= 1 (length (schema-op-upgrade op))) "a real type change still emits SET DATA TYPE")))
