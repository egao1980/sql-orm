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

;;; Column attribute helpers shared by ALTER-COLUMN-OP validation, its DDL,
;;; equality (algebra.lisp) and the complete diff.

(declaim (inline %flag=))
(defun %flag= (a b)
  "A and B agree as booleans."
  (eq (not a) (not b)))

(defun %key-flags-equal (a b)
  "Column-infos A and B agree on PRIMARY-KEY, UNIQUE and AUTOINCREMENT."
  (and (%flag= (column-info-primary-key a) (column-info-primary-key b))
       (%flag= (column-info-unique a) (column-info-unique b))
       (%flag= (column-info-autoincrement a) (column-info-autoincrement b))))

(defun %effective-not-null (column)
  "NOT NULL as the database sees it: a primary key is implicitly NOT NULL."
  (and (or (column-info-not-null column) (column-info-primary-key column)) t))

(defun %node-or-lit (value)
  "VALUE as a SQL node: a SQL-NODE as itself, anything else as (LIT VALUE).
The same rule sql-query uses for a column-def DEFAULT."
  (if (typep value 'sql-query:sql-node) value (sql-query:lit value)))

(defclass alter-column-op (schema-op)
  ((table-name :initarg :table-name :reader schema-op-table-name)
   (from :initarg :from :reader schema-op-from-column
         :documentation "Complete COLUMN-INFO before the change.")
   (to :initarg :to :reader schema-op-to-column
       :documentation "Complete COLUMN-INFO after the change."))
  (:documentation
   "Change one column's type, nullability and/or default in place.
FROM and TO are complete COLUMN-INFOs with the same name, primary key, unique
and autoincrement flags; INITIALIZE-INSTANCE signals INVALID-SCHEMA-OP
otherwise. Columns whose key flags change are a DROP-COLUMN-OP + ADD-COLUMN-OP
pair (see DIFF-SCHEMA :COMPLETE)."))

(defmethod initialize-instance :after ((op alter-column-op) &key)
  (flet ((bad (fmt &rest args)
           (error 'invalid-schema-op :op op :message (apply #'format nil fmt args))))
    (unless (and (slot-boundp op 'from) (slot-boundp op 'to)
                 (column-info-p (schema-op-from-column op))
                 (column-info-p (schema-op-to-column op)))
      (bad "FROM and TO must both be COLUMN-INFOs"))
    (let ((from (schema-op-from-column op))
          (to (schema-op-to-column op)))
      (unless (string-equal (column-info-name from) (column-info-name to))
        (bad "FROM ~S and TO ~S name different columns"
             (column-info-name from) (column-info-name to)))
      (loop for (reader . what) in '((column-info-primary-key . "primary key")
                                     (column-info-unique . "unique")
                                     (column-info-autoincrement . "autoincrement"))
            unless (%flag= (funcall reader from) (funcall reader to))
              do (bad "FROM and TO differ in ~A; use DROP-COLUMN-OP + ADD-COLUMN-OP" what)))))

(defgeneric schema-op-column-name (op)
  (:documentation "Name of the column OP touches (ALTER-COLUMN-OP: the shared name of FROM and TO).")
  (:method ((op alter-column-op))
    (column-info-name (schema-op-from-column op)))
  (:method ((op add-column-op))
    (column-info-name (schema-op-column op)))
  (:method ((op drop-column-op))
    (column-info-name (schema-op-column op))))

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

(defun %alter-column-statements (table from to)
  "Portable DDL turning column FROM into TO on TABLE: one ALTER-TABLE-STATEMENT
per changed attribute, in the order type, nullability, default. The type is
compared structurally and kept as a spec, so each dialect renders its own name."
  (let ((column (%keywordize (column-info-name to)))
        (out '()))
    (unless (equal (column-info-type from) (column-info-type to))
      (push (alter-table table (sql-query:set-data-type column (column-info-type to))) out))
    (unless (%flag= (%effective-not-null from) (%effective-not-null to))
      (push (alter-table table (if (%effective-not-null to)
                                   (sql-query:set-not-null :column column)
                                   (sql-query:drop-not-null :column column)))
            out))
    (let ((df (column-info-default from))
          (dt (column-info-default to)))
      (unless (equal (%default-sql df nil) (%default-sql dt nil))
        (push (alter-table table (if dt
                                     (sql-query:set-default (%node-or-lit dt) :column column)
                                     (sql-query:drop-default :column column)))
              out)))
    (nreverse out)))

(defmethod schema-op-upgrade ((op alter-column-op))
  (%alter-column-statements (schema-op-table-name op)
                            (schema-op-from-column op)
                            (schema-op-to-column op)))

(defmethod schema-op-downgrade ((op alter-column-op))
  (%alter-column-statements (schema-op-table-name op)
                            (schema-op-to-column op)
                            (schema-op-from-column op)))

(defmethod invert-schema-op ((op alter-column-op))
  (make-instance 'alter-column-op
                 :table-name (schema-op-table-name op)
                 :from (copy-column-info (schema-op-to-column op))
                 :to (copy-column-info (schema-op-from-column op))))

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

(defun %diff-schema-complete (from to dialect)
  "DIFF-SCHEMA :COMPLETE T. Locked op order: tables in TO order (create, or
drop-columns in FROM order, add-columns in TO order, alter-columns in TO
order), then drop-tables in FROM order. A key-flag change is drop + add."
  (let ((d (%dialect-instance dialect))
        (key (schema-dialect-key dialect))
        (ops '()))
    (flet ((emit (op) (push op ops)))
      (dolist (entry to)
        (destructuring-bind (table . cols) entry
          (let ((old (%find-table from table)))
            (if (null old)
                (emit (make-instance 'create-table-op
                                     :table-name table
                                     :columns (mapcar #'copy-column-info cols)))
                ;; Column ops name the table as FROM has it: the existing table.
                (let ((tname (car old))
                      (old-cols (cdr old)))
                  (flet ((partner (c columns) (%col-by-name columns (column-info-name c))))
                    (dolist (c old-cols)
                      (let ((new (partner c cols)))
                        (when (or (null new) (not (%key-flags-equal c new)))
                          (emit (make-instance 'drop-column-op
                                               :table-name tname
                                               :column (copy-column-info c))))))
                    (dolist (c cols)
                      (let ((prev (partner c old-cols)))
                        (when (or (null prev) (not (%key-flags-equal prev c)))
                          (emit (make-instance 'add-column-op
                                               :table-name tname
                                               :column (copy-column-info c))))))
                    (dolist (c cols)
                      (let ((prev (partner c old-cols)))
                        (when (and prev
                                   (%key-flags-equal prev c)
                                   (not (%column-equal* prev c d key)))
                          (emit (make-instance 'alter-column-op
                                               :table-name tname
                                               :from (copy-column-info prev)
                                               :to (copy-column-info c))))))))))))
      (dolist (entry from)
        (unless (%find-table to (car entry))
          (emit (make-instance 'drop-table-op
                               :table-name (car entry)
                               :columns (mapcar #'copy-column-info (cdr entry)))))))
    (nreverse ops)))

(defun diff-schema (from to &key (drop-tables nil) (drop-columns t)
                               (complete nil) (dialect nil))
  "Diff model snapshots FROM → TO.

Returns a list of SCHEMA-OP (create-table-op / add-column-op / …), not SQL text
and not bare DDL statements. Use SCHEMA-OP-UPGRADE / SCHEMA-OPS-UPGRADE for AST,
UPGRADE-SCHEMA to execute, SCHEMA-OP-DOWNGRADE / DOWNGRADE-SCHEMA to roll back.

Default (COMPLETE nil): the 0.1 autogenerate policy, unchanged — creates, adds,
drops per DROP-TABLES / DROP-COLUMNS, no alterations.

COMPLETE t: the faithful diff — every drop and alter, so
(SCHEMA-SNAPSHOT-EQUAL (APPLY-OPS-TO-SNAPSHOT FROM ops) TO :DIALECT DIALECT).
DROP-TABLES and DROP-COLUMNS are ignored. DIALECT (nil = ANSI, a registered
keyword, or a SQL-DIALECT) is the equality used to detect changed columns."
  (when complete
    (return-from diff-schema
      (%diff-schema-complete from to dialect)))
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

(defun %run-schema-ops (connection ops-or-migration direction transaction)
  "Run each op in DIRECTION (:UPGRADE forward order, :DOWNGRADE reverse order).
Each op's statements come from DIALECT-SCHEMA-OP-STATEMENTS just before that op
runs, so a dialect method may read the live schema left by the earlier ops.
Returns the ops in forward order."
  (let* ((conn (orm-connection connection))
         (ops (%coerce-ops ops-or-migration))
         (key (schema-dialect-key (%dialect conn)))
         (run (lambda ()
                (dolist (op (ecase direction
                              (:upgrade ops)
                              (:downgrade (reverse ops))))
                  (dolist (stmt (dialect-schema-op-statements key op direction conn))
                    (%exec conn stmt)))
                ops)))
    (if transaction
        (with-transaction (conn) (funcall run))
        (funcall run))))

(defun upgrade-schema (connection ops-or-migration &key (transaction t))
  "Apply forward SCHEMA-OPs (or a SCHEMA-MIGRATION) to CONNECTION."
  (%run-schema-ops connection ops-or-migration :upgrade transaction))

(defun downgrade-schema (connection ops-or-migration &key (transaction t))
  "Roll back SCHEMA-OPs (or a SCHEMA-MIGRATION) — reverse order, each downgraded."
  (%run-schema-ops connection ops-or-migration :downgrade transaction))

(defun apply-schema-ops (connection ops)
  "Deprecated alias for UPGRADE-SCHEMA (no transaction wrap for old callers)."
  (upgrade-schema connection ops :transaction nil))
