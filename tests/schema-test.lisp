(in-package #:sql-orm/tests)

(deftest schema-diff-add-column
  (clrhash sql-orm::*model-registry*)
  (defmodel item-v1 ()
    (id :integer :primary-key t :autoincrement t)
    (name :text :not-null t)
    (:table items))
  (let ((from (schema-snapshot '(item-v1))))
    (clrhash sql-orm::*model-registry*)
    (defmodel item-v2 ()
      (id :integer :primary-key t :autoincrement t)
      (name :text :not-null t)
      (sku :text)
      (:table items))
    (let* ((to (schema-snapshot '(item-v2)))
           (ops (diff-schema from to))
           (op (first ops))
           (stmts (schema-op-upgrade op))
           (stmt (first stmts)))
      (ok (= 1 (length ops)))
      (ok (typep op 'add-column-op))
      (ok (string-equal "items" (schema-op-table-name op)))
      (ok (string-equal "sku" (column-info-name (schema-op-column op))))
      (ok (eq :text (column-info-type (schema-op-column op))))
      (ok (typep stmt 'sql-query:alter-table-statement))
      (ok (typep (invert-schema-op op) 'drop-column-op)))))

(deftest schema-diff-create-table
  (clrhash sql-orm::*model-registry*)
  (defmodel only-user ()
    (id :integer :primary-key t)
    (name :text)
    (:table users))
  (let ((from (schema-snapshot '(only-user))))
    (defmodel only-post ()
      (id :integer :primary-key t)
      (title :text)
      (:table posts))
    (let* ((to (schema-snapshot '(only-user only-post)))
           (ops (diff-schema from to))
           (op (first ops))
           (stmt (first (schema-op-upgrade op))))
      (ok (= 1 (length ops)))
      (ok (typep op 'create-table-op))
      (ok (string-equal "posts" (schema-op-table-name op)))
      (ok (find "title" (schema-op-columns op)
                :key #'column-info-name :test #'string-equal))
      (ok (typep stmt 'sql-query:create-table-statement))
      (ok (typep (invert-schema-op op) 'drop-table-op)))))

(deftest migration-upgrade-downgrade-roundtrip
  "Alembic-shaped: make-migration → upgrade → downgrade restores schema."
  (clrhash sql-orm::*model-registry*)
  (defmodel widget-v1 ()
    (id :integer :primary-key t :autoincrement t)
    (name :text :not-null t)
    (:table widgets))
  (let ((from (schema-snapshot '(widget-v1))))
    (clrhash sql-orm::*model-registry*)
    (defmodel widget-v2 ()
      (id :integer :primary-key t :autoincrement t)
      (name :text :not-null t)
      (color :text)
      (:table widgets))
    (let ((mig (make-migration from (schema-snapshot '(widget-v2))
                               :name "add-color"
                               :revision "0002"
                               :down-revision "0001")))
      (ok (equal "add-color" (schema-migration-name mig)))
      (ok (equal "0002" (schema-migration-revision mig)))
      (ok (= 1 (length (migration-upgrade-ops mig))))
      (ok (typep (first (migration-downgrade-ops mig)) 'drop-column-op))
      (with-orm-connection (c :driver :sqlite3 :database-name ":memory:")
        ;; base table (= v1)
        (upgrade-schema c (diff-schema nil from) :transaction nil)
        (sql-protocol:execute c "INSERT INTO widgets (name) VALUES (?)" '("w"))
        ;; upgrade adds color
        (upgrade-schema c mig)
        (sql-protocol:execute c "UPDATE widgets SET color = ? WHERE name = ?"
                              '("red" "w"))
        (let ((row (sql-protocol:fetch
                    (sql-protocol:execute c "SELECT color FROM widgets"))))
          (ok (equal "red" (getf row :color))))
        ;; downgrade drops color
        (downgrade-schema c mig)
        (ok (signals
             (sql-protocol:execute c "SELECT color FROM widgets")
             'error))
        ;; name column still there
        (let ((row (sql-protocol:fetch
                    (sql-protocol:execute c "SELECT name FROM widgets"))))
          (ok (equal "w" (getf row :name))))))))

(deftest drop-table-op-roundtrip
  (clrhash sql-orm::*model-registry*)
  (defmodel tmp ()
    (id :integer :primary-key t)
    (x :text)
    (:table tmps))
  (let* ((snap (schema-snapshot '(tmp)))
         (create (first (diff-schema nil snap)))
         (drop (invert-schema-op create)))
    (ok (typep create 'create-table-op))
    (ok (typep drop 'drop-table-op))
    (with-orm-connection (c :driver :sqlite3 :database-name ":memory:")
      (upgrade-schema c (list create) :transaction nil)
      (ok (sql-protocol:fetch
           (sql-protocol:execute c "SELECT name FROM sqlite_master WHERE name = 'tmps'")))
      (upgrade-schema c (list drop) :transaction nil)
      (ok (null
           (sql-protocol:fetch
            (sql-protocol:execute c "SELECT name FROM sqlite_master WHERE name = 'tmps'"))))
      ;; downgrade of drop = recreate
      (downgrade-schema c (list drop) :transaction nil)
      (ok (sql-protocol:fetch
           (sql-protocol:execute c "SELECT name FROM sqlite_master WHERE name = 'tmps'"))))))

(deftest ensure-schema-roundtrip
  (clrhash sql-orm::*model-registry*)
  (defmodel note ()
    (id :integer :primary-key t :autoincrement t)
    (body :text :not-null t)
    (:table notes))
  (with-orm-connection (c :driver :sqlite3 :database-name ":memory:")
    (ensure-schema c 'note)
    (persist (make-instance 'note :body "hi"))
    (ok (equal "hi" (body (find-instance 'note 1))))))
