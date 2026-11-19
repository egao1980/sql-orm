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
  (declare (ignore dialect))
  (%s1-pending "schema-dialect-key"))

;;; ---------------------------------------------------------------------------
;;; Equality
;;; ---------------------------------------------------------------------------

(defgeneric canonical-type-sql (dialect-key type-sql)
  (:documentation
   "Canonical spelling of the SQL type string TYPE-SQL under DIALECT-KEY.

Default: upcase, trim, collapse runs of whitespace to one space, no space next
to ( , ). :POSTGRES also folds catalog aliases to the sql-query spelling
(character varying(n) → VARCHAR(N), timestamp with time zone → TIMESTAMPTZ,
int4 → INTEGER, …). Two column types are equal when the canonical spellings of
their DIALECT-TYPE-SQL renderings are STRING=.")
  (:method (dialect-key type-sql)
    (declare (ignore dialect-key type-sql))
    (%s1-pending "canonical-type-sql")))

(defun column-info-equal (a b &key dialect)
  "True when column-infos A and B describe the same column under DIALECT.

Name STRING-EQUAL; type by CANONICAL-TYPE-SQL of the dialect rendering;
PRIMARY-KEY, UNIQUE, AUTOINCREMENT as booleans; NOT-NULL as
(or not-null primary-key); defaults by their rendered SQL text (NIL = no
default)."
  (declare (ignore a b dialect))
  (%s1-pending "column-info-equal"))

(defun schema-snapshot-equal (a b &key dialect)
  "True when snapshots A and B hold the same tables (names STRING-EQUAL) with
the same columns (matched by name, COLUMN-INFO-EQUAL under DIALECT). Table and
column order are not significant."
  (declare (ignore a b dialect))
  (%s1-pending "schema-snapshot-equal"))

;;; ---------------------------------------------------------------------------
;;; Pure fold
;;; ---------------------------------------------------------------------------

(defun apply-ops-to-snapshot (snapshot ops)
  "Fold OPS (a list of SCHEMA-OP, a single op, or a SCHEMA-MIGRATION) over
SNAPSHOT and return a new snapshot. Pure: SNAPSHOT and OPS are not modified
and the result shares no COLUMN-INFO with either.

create-table-op appends the table; drop-table-op removes it; add-column-op
appends the column; drop-column-op removes it; alter-column-op replaces the
column in place by a copy of its TO. An op that does not fit signals
SNAPSHOT-CONFLICT."
  (declare (ignore snapshot ops))
  (%s1-pending "apply-ops-to-snapshot"))

;;; ---------------------------------------------------------------------------
;;; Footprints
;;; ---------------------------------------------------------------------------

(defgeneric schema-op-footprint (op)
  (:documentation
   "Fresh list of resource strings OP touches.

Table ops (create-table-op, drop-table-op) → (\"table:<t>\").
Column ops (add-column-op, drop-column-op, alter-column-op) → (\"column:<t>.<c>\").
Names are downcased. A consumer treats \"table:<t>\" as covering every
\"column:<t>.<c>\".")
  (:method ((op schema-op))
    (%s1-pending "schema-op-footprint")))
