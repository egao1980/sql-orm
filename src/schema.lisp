(in-package #:sql-orm)

;;; Schema as data. Diff compares model snapshots (column-info), never SQL text.
;;; Result = list of sql-query DDL AST nodes (create-table / alter-table / …).
;;; Compile/execute is a separate step. No versioned migration runner in wave-1.

(defun %column→table-column (col)
  (apply #'table-column
         (%keywordize (column-info-name col))
         :type (column-info-type col)
         :primary-key (column-info-primary-key col)
         :autoincrement (column-info-autoincrement col)
         :not-null (column-info-not-null col)
         :unique (column-info-unique col)
         (when (column-info-default col)
           (list :default (column-info-default col)))))

(defun model-sql-table (model-name)
  "Build a sql-query sql-table from model metadata."
  (let ((meta (find-model model-name)))
    (apply #'make-sql-table
           (model-class-table-name meta)
           (mapcar #'%column→table-column (model-class-columns meta)))))

(defun schema-snapshot (&optional (models (mapcar #'model-class-name (list-models))))
  "Alist of (table-name-keyword . list-of-column-info) for MODELS."
  (mapcar (lambda (m)
            (let ((meta (find-model m)))
              (cons (model-class-table-name meta)
                    (mapcar #'copy-column-info (model-class-columns meta)))))
          models))

(defun %find-table (snapshot table-name)
  (assoc table-name snapshot :test #'equal))

(defun %col-by-name (columns name)
  (find name columns :key #'column-info-name :test #'string-equal))

(defun diff-schema (from to &key (drop-tables nil) (drop-columns t))
  "Diff model snapshots FROM → TO at the structural level.

FROM/TO are alists from SCHEMA-SNAPSHOT (table → column-info list).
Returns sql-query DDL AST nodes (not SQL strings) — create-table-statement,
alter-table-statement (add-column / drop-column), optional drop-table-statement.
Callers compile/execute with sql-query as needed. Type alterations: wave-1 skip."
  (let ((ops '()))
    ;; create / alter tables present in TO
    (dolist (entry to)
      (destructuring-bind (table . cols) entry
        (let ((old (%find-table from table)))
          (if (null old)
              (push (apply #'create-table table
                           (mapcar #'%column→table-column cols))
                    ops)
              (let ((old-cols (cdr old)))
                ;; add columns
                (dolist (c cols)
                  (unless (%col-by-name old-cols (column-info-name c))
                    (push (alter-table table
                                       (apply #'add-column
                                              (%keywordize (column-info-name c))
                                              :type (column-info-type c)
                                              :primary-key (column-info-primary-key c)
                                              :autoincrement (column-info-autoincrement c)
                                              :not-null (column-info-not-null c)
                                              :unique (column-info-unique c)
                                              (when (column-info-default c)
                                                (list :default (column-info-default c)))))
                          ops)))
                ;; drop columns
                (when drop-columns
                  (dolist (c old-cols)
                    (unless (%col-by-name cols (column-info-name c))
                      (push (alter-table table
                                         (drop-column (%keywordize (column-info-name c))))
                            ops)))))))))
    ;; drop tables removed from TO
    (when drop-tables
      (dolist (entry from)
        (unless (%find-table to (car entry))
          (push (drop-table (car entry) :if-exists t) ops))))
    (nreverse ops)))

(defun ensure-schema (connection &rest model-names)
  "CREATE TABLE IF NOT EXISTS for each model (wave-1 bootstrap)."
  (let ((conn (orm-connection connection))
        (names (or model-names (mapcar #'model-class-name (list-models)))))
    (dolist (m names)
      (%exec conn (create-table-from (model-sql-table m) :if-not-exists t)))
    names))

(defun apply-schema-ops (connection ops)
  "Execute a list of DDL statements from DIFF-SCHEMA."
  (let ((conn (orm-connection connection)))
    (dolist (op ops)
      (%exec conn op))
    ops))
