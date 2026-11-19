(in-package #:sql-orm)

;;; sql-orm 0.2.0 schema algebra (S1): pure snapshot fold, equality, footprints.
;;; Contract: cl-stack docs/capabilities/sql.md, "sql-orm 0.2.0 — S1 locked".
;;; No dependency on revision-protocol or diff-protocol.

;;; ---------------------------------------------------------------------------
;;; Dialect designators
;;; ---------------------------------------------------------------------------

(defun schema-dialect-key (dialect)
  "Keyword naming DIALECT for sql-orm dispatch.

DIALECT is NIL (→ :ANSI), a keyword (returned as is), or a SQL-DIALECT
instance (→ the key it is registered under in SQL-QUERY:*SQL-DIALECT-REGISTRY*,
:ANSI for an unregistered ANSI-DIALECT, else NIL)."
  (etypecase dialect
    (null :ansi)
    (keyword dialect)
    (sql-query:sql-dialect
     (or (let ((hit nil))
           ;; Prefer a vendor key when one instance is registered twice.
           (maphash (lambda (k v)
                      (when (and (eq v dialect) (or (null hit) (eq hit :ansi)))
                        (setf hit k)))
                    sql-query:*sql-dialect-registry*)
           hit)
         ;; Exact class: vendor dialects subclass ANSI-DIALECT and must give NIL.
         (and (eq (class-of dialect) (find-class 'sql-query:ansi-dialect)) :ansi)))))

(defvar *%ansi-dialect* nil
  "Cached ANSI dialect for equality when :ANSI is not registered.")

(defun %ansi-dialect ()
  (or (gethash :ansi sql-query:*sql-dialect-registry*)
      *%ansi-dialect*
      (setf *%ansi-dialect* (sql-query:make-ansi-dialect))))

(defun %dialect-instance (dialect)
  "Rendering dialect for an equality designator. NIL means ANSI, never
SQL-QUERY:*SQL-DIALECT*."
  (etypecase dialect
    (sql-query:sql-dialect dialect)
    (null (%ansi-dialect))
    (keyword
     (or (gethash dialect sql-query:*sql-dialect-registry*)
         (and (eq dialect :ansi) (%ansi-dialect))
         (error 'orm-error
                :message (format nil "SQL dialect ~S is not registered" dialect))))))

;;; ---------------------------------------------------------------------------
;;; Equality
;;; ---------------------------------------------------------------------------

(defun %canonical-spacing (string)
  "Upcase, trim, collapse whitespace runs to one space, no space next to ( , )."
  (let ((whitespace '(#\Space #\Tab #\Newline #\Return #\Linefeed #\Page))
        (pending nil)
        (prev nil))
    (with-output-to-string (out)
      (loop for ch across (string-upcase (string-trim whitespace string))
            do (if (member ch whitespace)
                   (setf pending t)
                   (progn
                     (when (and pending prev
                                (not (find prev "(,"))
                                (not (find ch "(,)")))
                       (write-char #\Space out))
                     (write-char ch out)
                     (setf pending nil
                           prev ch)))))))

(defgeneric canonical-type-sql (dialect-key type-sql)
  (:documentation
   "Canonical spelling of the SQL type string TYPE-SQL under DIALECT-KEY.

Default: upcase, trim, collapse runs of whitespace to one space, no space next
to ( , ). :POSTGRES also folds catalog aliases to the sql-query spelling
(character varying(n) → VARCHAR(N), timestamp with time zone → TIMESTAMPTZ,
int4 → INTEGER, …). Two column types are equal when the canonical spellings of
their DIALECT-TYPE-SQL renderings are STRING=.")
  (:method (dialect-key type-sql)
    (declare (ignore dialect-key))
    (%canonical-spacing (or type-sql ""))))

(defparameter *%postgres-type-aliases*
  '(("CHARACTER VARYING" . "VARCHAR")
    ("CHARACTER" . "CHAR")
    ("TIMESTAMP WITH TIME ZONE" . "TIMESTAMPTZ")
    ("TIMESTAMP WITHOUT TIME ZONE" . "TIMESTAMP")
    ("TIME WITHOUT TIME ZONE" . "TIME")
    ("INT" . "INTEGER")
    ("INT4" . "INTEGER")
    ("INT8" . "BIGINT")
    ("INT2" . "SMALLINT")
    ("BOOL" . "BOOLEAN")
    ("FLOAT8" . "DOUBLE PRECISION")
    ("FLOAT4" . "REAL"))
  "PostgreSQL catalog type base names → sql-query spelling (canonical form).")

(defmethod canonical-type-sql ((dialect-key (eql :postgres)) type-sql)
  (let* ((s (call-next-method))
         (paren (position #\( s))
         (base (subseq s 0 paren))
         (alias (cdr (assoc base *%postgres-type-aliases* :test #'string=))))
    (if alias
        (concatenate 'string alias (if paren (subseq s paren) ""))
        s)))

(defun %type-sql (type dialect key)
  (canonical-type-sql key (sql-query:dialect-type-sql dialect type)))

(defun %default-sql (value dialect)
  "NIL for no default, else the SQL text DIALECT emits for it (the column-def
DEFAULT rule). DIALECT NIL means ANSI."
  (and value
       (values (compile-sql (%node-or-lit value)
                            :dialect (or dialect (%ansi-dialect))))))

(defun %column-equal* (a b dialect key)
  "COLUMN-INFO-EQUAL with a resolved DIALECT instance and dispatch KEY."
  (and (string-equal (column-info-name a) (column-info-name b))
       (%key-flags-equal a b)
       (eq (%effective-not-null a) (%effective-not-null b))
       (string= (%type-sql (column-info-type a) dialect key)
                (%type-sql (column-info-type b) dialect key))
       (equal (%default-sql (column-info-default a) dialect)
              (%default-sql (column-info-default b) dialect))))

(defun column-info-equal (a b &key dialect)
  "True when column-infos A and B describe the same column under DIALECT.

Name STRING-EQUAL; type by CANONICAL-TYPE-SQL of the dialect rendering;
PRIMARY-KEY, UNIQUE, AUTOINCREMENT as booleans; NOT-NULL as
(or not-null primary-key); defaults by their rendered SQL text (NIL = no
default)."
  (%column-equal* a b (%dialect-instance dialect) (schema-dialect-key dialect)))

(defun %one-to-one-p (xs ys name-of matchp)
  "Every X has a partner Y with the same name (STRING-EQUAL) satisfying MATCHP,
and the counts agree. Names are unique per level, so this is a bijection."
  (and (= (length xs) (length ys))
       (every (lambda (x)
                (let ((y (find (funcall name-of x) ys :key name-of :test #'string-equal)))
                  (and y (funcall matchp x y))))
              xs)))

(defun schema-snapshot-equal (a b &key dialect)
  "True when snapshots A and B hold the same tables (names STRING-EQUAL) with
the same columns (matched by name, COLUMN-INFO-EQUAL under DIALECT). Table and
column order are not significant."
  (let ((d (%dialect-instance dialect))
        (key (schema-dialect-key dialect)))
    (%one-to-one-p a b #'car
                   (lambda (ta tb)
                     (%one-to-one-p (cdr ta) (cdr tb) #'column-info-name
                                    (lambda (x y) (%column-equal* x y d key)))))))

;;; ---------------------------------------------------------------------------
;;; Pure fold
;;; ---------------------------------------------------------------------------

(defun %conflict (op fmt &rest args)
  (error 'snapshot-conflict :op op :message (apply #'format nil fmt args)))

(defgeneric %apply-op (op snapshot)
  (:documentation "New snapshot: OP applied to SNAPSHOT. Never modifies SNAPSHOT
(untouched table entries may be shared; APPLY-OPS-TO-SNAPSHOT copies its input
first). Signals SNAPSHOT-CONFLICT when OP does not fit.")
  (:method ((op schema-op) snapshot)
    (declare (ignore snapshot))
    (error 'invalid-schema-op :op op
                              :message "no snapshot semantics for this op class")))

(defun %table-or-conflict (op snapshot)
  (or (%find-table snapshot (schema-op-table-name op))
      (%conflict op "no table ~A" (schema-op-table-name op))))

(defun %column-or-conflict (op entry)
  (or (%col-by-name (cdr entry) (schema-op-column-name op))
      (%conflict op "no column ~A.~A" (car entry) (schema-op-column-name op))))

(defun %replace-entry (snapshot entry new-columns)
  (mapcar (lambda (e) (if (eq e entry) (cons (car e) new-columns) e)) snapshot))

(defmethod %apply-op ((op create-table-op) snapshot)
  (when (%find-table snapshot (schema-op-table-name op))
    (%conflict op "table ~A already exists" (schema-op-table-name op)))
  (append snapshot
          (list (cons (schema-op-table-name op)
                      (mapcar #'copy-column-info (schema-op-columns op))))))

(defmethod %apply-op ((op drop-table-op) snapshot)
  (remove (%table-or-conflict op snapshot) snapshot :test #'eq :count 1))

(defmethod %apply-op ((op add-column-op) snapshot)
  (let ((entry (%table-or-conflict op snapshot))
        (column (schema-op-column op)))
    (when (%col-by-name (cdr entry) (column-info-name column))
      (%conflict op "column ~A.~A already exists" (car entry) (column-info-name column)))
    (%replace-entry snapshot entry
                    (append (cdr entry) (list (copy-column-info column))))))

(defmethod %apply-op ((op drop-column-op) snapshot)
  (let* ((entry (%table-or-conflict op snapshot))
         (old (%column-or-conflict op entry)))
    (%replace-entry snapshot entry (remove old (cdr entry) :test #'eq :count 1))))

(defmethod %apply-op ((op alter-column-op) snapshot)
  (let* ((entry (%table-or-conflict op snapshot))
         (old (%column-or-conflict op entry)))
    (%replace-entry snapshot entry
                    (substitute (copy-column-info (schema-op-to-column op)) old (cdr entry)
                                :test #'eq :count 1))))

(defun apply-ops-to-snapshot (snapshot ops)
  "Fold OPS (a list of SCHEMA-OP, a single op, or a SCHEMA-MIGRATION) over
SNAPSHOT and return a new snapshot. Pure: SNAPSHOT and OPS are not modified
and the result shares no COLUMN-INFO with either.

create-table-op appends the table; drop-table-op removes it; add-column-op
appends the column; drop-column-op removes it; alter-column-op replaces the
column in place by a copy of its TO. An op that does not fit signals
SNAPSHOT-CONFLICT."
  (let ((s (mapcar (lambda (e) (cons (car e) (mapcar #'copy-column-info (cdr e))))
                   snapshot)))
    (dolist (op (%coerce-ops ops) s)
      (setf s (%apply-op op s)))))

;;; ---------------------------------------------------------------------------
;;; Footprints
;;; ---------------------------------------------------------------------------

(defun %footprint-name (name)
  (string-downcase (string name)))

(defun %table-footprint (op)
  (list (format nil "table:~A" (%footprint-name (schema-op-table-name op)))))

(defun %column-footprint (op)
  (list (format nil "column:~A.~A"
                (%footprint-name (schema-op-table-name op))
                (%footprint-name (schema-op-column-name op)))))

(defgeneric schema-op-footprint (op)
  (:documentation
   "Fresh list of resource strings OP touches.

Table ops (create-table-op, drop-table-op) → (\"table:<t>\").
Column ops (add-column-op, drop-column-op, alter-column-op) → (\"column:<t>.<c>\").
Names are downcased. A consumer treats \"table:<t>\" as covering every
\"column:<t>.<c>\".")
  (:method ((op schema-op))
    (error 'invalid-schema-op :op op :message "no footprint for this op class"))
  (:method ((op create-table-op)) (%table-footprint op))
  (:method ((op drop-table-op)) (%table-footprint op))
  (:method ((op add-column-op)) (%column-footprint op))
  (:method ((op drop-column-op)) (%column-footprint op))
  (:method ((op alter-column-op)) (%column-footprint op)))
