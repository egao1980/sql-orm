(in-package #:sql-orm)

;;; Schema as data.
;;; Diff compares model snapshots (column-info), never SQL text.
;;; Result = reversible SCHEMA-OP objects; upgrade/downgrade emit sql-query DDL AST.
;;; A later Alembic-style package can version/order these; we only supply the op algebra.

;;; ---------------------------------------------------------------------------
;;; Reversible ops
;;; ---------------------------------------------------------------------------

(defclass schema-op ()
  ()
  (:documentation "Structural schema change. UPGRADE/DOWNGRADE → sql-query DDL AST."))

(defclass create-table-op (schema-op)
  ((table-name :initarg :table-name :reader schema-op-table-name)
   (columns :initarg :columns :reader schema-op-columns
            :documentation "List of COLUMN-INFO.")))

(defclass drop-table-op (schema-op)
  ((table-name :initarg :table-name :reader schema-op-table-name)
   (columns :initarg :columns :reader schema-op-columns
            :documentation "Columns required so downgrade can recreate the table.")))

(defclass add-column-op (schema-op)
  ((table-name :initarg :table-name :reader schema-op-table-name)
   (column :initarg :column :reader schema-op-column)))

(defclass drop-column-op (schema-op)
  ((table-name :initarg :table-name :reader schema-op-table-name)
   (column :initarg :column :reader schema-op-column
           :documentation "Full COLUMN-INFO so downgrade can re-add.")))

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

(defun %add-column-clause (col)
  (apply #'add-column
         (%keywordize (column-info-name col))
         :type (column-info-type col)
         :primary-key (column-info-primary-key col)
         :autoincrement (column-info-autoincrement col)
         :not-null (column-info-not-null col)
         :unique (column-info-unique col)
         (when (column-info-default col)
           (list :default (column-info-default col)))))

(defun %create-table-stmt (table-name columns)
  (apply #'create-table table-name
         (mapcar #'%column→table-column columns)))

(defgeneric schema-op-upgrade (op)
  (:documentation "Forward DDL AST nodes (list) for OP."))

(defgeneric schema-op-downgrade (op)
  (:documentation "Reverse DDL AST nodes (list) for OP."))

(defgeneric invert-schema-op (op)
  (:documentation "Op that is the structural inverse of OP (model-level)."))

(defmethod schema-op-upgrade ((op create-table-op))
  (list (%create-table-stmt (schema-op-table-name op) (schema-op-columns op))))

(defmethod schema-op-downgrade ((op create-table-op))
  (list (drop-table (schema-op-table-name op) :if-exists t)))

(defmethod invert-schema-op ((op create-table-op))
  (make-instance 'drop-table-op
                 :table-name (schema-op-table-name op)
                 :columns (mapcar #'copy-column-info (schema-op-columns op))))

(defmethod schema-op-upgrade ((op drop-table-op))
  (list (drop-table (schema-op-table-name op) :if-exists t)))

(defmethod schema-op-downgrade ((op drop-table-op))
  (list (%create-table-stmt (schema-op-table-name op) (schema-op-columns op))))

(defmethod invert-schema-op ((op drop-table-op))
  (make-instance 'create-table-op
                 :table-name (schema-op-table-name op)
                 :columns (mapcar #'copy-column-info (schema-op-columns op))))

(defmethod schema-op-upgrade ((op add-column-op))
  (list (alter-table (schema-op-table-name op)
                     (%add-column-clause (schema-op-column op)))))

(defmethod schema-op-downgrade ((op add-column-op))
  (list (alter-table (schema-op-table-name op)
                     (drop-column (%keywordize (column-info-name (schema-op-column op)))))))

(defmethod invert-schema-op ((op add-column-op))
  (make-instance 'drop-column-op
                 :table-name (schema-op-table-name op)
                 :column (copy-column-info (schema-op-column op))))

(defmethod schema-op-upgrade ((op drop-column-op))
  (list (alter-table (schema-op-table-name op)
                     (drop-column (%keywordize (column-info-name (schema-op-column op)))))))

(defmethod schema-op-downgrade ((op drop-column-op))
  (list (alter-table (schema-op-table-name op)
                     (%add-column-clause (schema-op-column op)))))

(defmethod invert-schema-op ((op drop-column-op))
  (make-instance 'add-column-op
                 :table-name (schema-op-table-name op)
                 :column (copy-column-info (schema-op-column op))))

(defun schema-ops-upgrade (ops)
  "Flatten UPGRADE AST for a list of SCHEMA-OPs (forward order)."
  (mapcan (lambda (op) (copy-list (schema-op-upgrade op))) ops))

(defun schema-ops-downgrade (ops)
  "Flatten DOWNGRADE AST for OPS, applying inverses in reverse order."
  (mapcan (lambda (op) (copy-list (schema-op-downgrade op)))
          (reverse ops)))

;;; ---------------------------------------------------------------------------
;;; Migration (thin handle for an Alembic-style layer)
;;; ---------------------------------------------------------------------------

(defclass schema-migration ()
  ((name :initarg :name :initform nil :accessor schema-migration-name)
   (ops :initarg :ops :accessor schema-migration-ops
        :documentation "Forward SCHEMA-OP list (upgrade direction).")
   (revision :initarg :revision :initform nil :accessor schema-migration-revision)
   (down-revision :initarg :down-revision :initform nil
                  :accessor schema-migration-down-revision
                  :documentation "Reserved for a versioned runner; unused here."))
  (:documentation
   "Ordered reversible ops. Runners store revision graph; we only carry the algebra."))

(defun make-migration (from to &key name revision down-revision
                                 (drop-tables nil) (drop-columns t))
  "Build a SCHEMA-MIGRATION whose upgrade ops are DIFF-SCHEMA of FROM → TO."
  (make-instance 'schema-migration
                 :name name
                 :revision revision
                 :down-revision down-revision
                 :ops (diff-schema from to
                                   :drop-tables drop-tables
                                   :drop-columns drop-columns)))

(defun migration-upgrade-ops (migration)
  (schema-migration-ops migration))

(defun migration-downgrade-ops (migration)
  (mapcar #'invert-schema-op (reverse (schema-migration-ops migration))))

;;; ---------------------------------------------------------------------------
;;; Snapshots + diff
;;; ---------------------------------------------------------------------------

(defun model-sql-table (model-name)
  "Build a sql-query sql-table from model metadata."
  (let ((meta (find-model model-name)))
    (apply #'make-sql-table
           (model-class-table-name meta)
           (mapcar #'%column→table-column (model-class-columns meta)))))

(defun schema-snapshot (&optional (models (mapcar #'model-class-name (list-models))))
  "Alist of (table-name . list-of-column-info) for MODELS."
  (mapcar (lambda (m)
            (let ((meta (find-model m)))
              (cons (model-class-table-name meta)
                    (mapcar #'copy-column-info (model-class-columns meta)))))
          models))

(defun %find-table (snapshot table-name)
  (assoc table-name snapshot :test #'string-equal))

(defun %col-by-name (columns name)
  (find name columns :key #'column-info-name :test #'string-equal))

(defun diff-schema (from to &key (drop-tables nil) (drop-columns t))
  "Diff model snapshots FROM → TO.

Returns a list of SCHEMA-OP (create-table-op / add-column-op / …), not SQL text
and not bare DDL statements. Use SCHEMA-OP-UPGRADE / SCHEMA-OPS-UPGRADE for AST,
UPGRADE-SCHEMA to execute, SCHEMA-OP-DOWNGRADE / DOWNGRADE-SCHEMA to roll back.
Type alterations: wave-1 skip."
  (let ((ops '()))
    (dolist (entry to)
      (destructuring-bind (table . cols) entry
        (let ((old (%find-table from table)))
          (if (null old)
              (push (make-instance 'create-table-op
                                   :table-name table
                                   :columns (mapcar #'copy-column-info cols))
                    ops)
              (let ((old-cols (cdr old)))
                (dolist (c cols)
                  (unless (%col-by-name old-cols (column-info-name c))
                    (push (make-instance 'add-column-op
                                         :table-name table
                                         :column (copy-column-info c))
                          ops)))
                (when drop-columns
                  (dolist (c old-cols)
                    (unless (%col-by-name cols (column-info-name c))
                      (push (make-instance 'drop-column-op
                                           :table-name table
                                           :column (copy-column-info c))
                            ops)))))))))
    (when drop-tables
      (dolist (entry from)
        (unless (%find-table to (car entry))
          (push (make-instance 'drop-table-op
                               :table-name (car entry)
                               :columns (mapcar #'copy-column-info (cdr entry)))
                ops))))
    (nreverse ops)))

;;; ---------------------------------------------------------------------------
;;; Execute
;;; ---------------------------------------------------------------------------

(defun ensure-schema (connection &rest model-names)
  "CREATE TABLE IF NOT EXISTS for each model (wave-1 bootstrap)."
  (let ((conn (orm-connection connection))
        (names (or model-names (mapcar #'model-class-name (list-models)))))
    (dolist (m names)
      (%exec conn (create-table-from (model-sql-table m) :if-not-exists t)))
    names))

(defun %coerce-ops (ops-or-migration)
  (ctypecase ops-or-migration
    (schema-migration (schema-migration-ops ops-or-migration))
    (list ops-or-migration)
    (schema-op (list ops-or-migration))))

(defun upgrade-schema (connection ops-or-migration &key (transaction t))
  "Apply forward SCHEMA-OPs (or a SCHEMA-MIGRATION) to CONNECTION."
  (let* ((conn (orm-connection connection))
         (ops (%coerce-ops ops-or-migration))
         (stmts (schema-ops-upgrade ops))
         (run (lambda ()
                (dolist (stmt stmts)
                  (%exec conn stmt))
                ops)))
    (if transaction
        (with-transaction (conn) (funcall run))
        (funcall run))))

(defun downgrade-schema (connection ops-or-migration &key (transaction t))
  "Roll back SCHEMA-OPs (or a SCHEMA-MIGRATION) — reverse order, each downgraded."
  (let* ((conn (orm-connection connection))
         (ops (%coerce-ops ops-or-migration))
         (stmts (schema-ops-downgrade ops))
         (run (lambda ()
                (dolist (stmt stmts)
                  (%exec conn stmt))
                ops)))
    (if transaction
        (with-transaction (conn) (funcall run))
        (funcall run))))

(defun apply-schema-ops (connection ops)
  "Deprecated alias for UPGRADE-SCHEMA (no transaction wrap for old callers)."
  (upgrade-schema connection ops :transaction nil))
