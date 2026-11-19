(in-package #:sql-orm/tests)

;;; sql-orm 0.2.0 S1 fixtures: snapshot builders, the snapshot-pair corpus,
;;; and a canned sql-protocol connection for PostgreSQL reflection without a
;;; server. Snapshots are built fresh on every call, so a test may compare
;;; against a second copy to check that nothing was mutated.

;;; ---------------------------------------------------------------------------
;;; Builders
;;; ---------------------------------------------------------------------------

(defun col (name type &rest keys &key primary-key autoincrement not-null unique default)
  "COLUMN-INFO named NAME (a string, as sql-orm compares names STRING-EQUAL)."
  (declare (ignore primary-key autoincrement not-null unique default))
  (apply #'sql-orm::make-column-info :name name :type type keys))

(defun tbl (name &rest columns)
  (cons name columns))

(defun copy-snapshot (snapshot)
  "Deep copy: fresh conses and fresh COLUMN-INFOs."
  (mapcar (lambda (entry)
            (cons (car entry) (mapcar #'sql-orm::copy-column-info (cdr entry))))
          snapshot))

(defun snapshot-column-infos (snapshot)
  (loop for entry in snapshot append (cdr entry)))

(defun op-classes (ops)
  (mapcar (lambda (op) (class-name (class-of op))) ops))

(defun dialect (key)
  (or (gethash key sql-query:*sql-dialect-registry*)
      (error "dialect ~S not registered" key)))

(defun sql-text (node &optional (dialect-key :ansi))
  "Compiled SQL string of NODE under the registered dialect DIALECT-KEY."
  (values (sql-query:compile-sql node :dialect (if (eq dialect-key :ansi)
                                                    (sql-query:make-ansi-dialect)
                                                    (dialect dialect-key)))))

(defun ops-sql (statements &optional (dialect-key :ansi))
  (format nil "~{~A~^; ~}"
          (mapcar (lambda (s) (sql-text s dialect-key)) statements)))

(defun contains (needle haystack)
  (and (search needle haystack :test #'char-equal) t))

;;; ---------------------------------------------------------------------------
;;; Tables
;;; ---------------------------------------------------------------------------

(defun items-v1 ()
  (tbl :items
       (col "id" :integer :primary-key t :autoincrement t)
       (col "name" :text :not-null t)
       (col "qty" :integer)))

(defun items-with (&rest replacements)
  "ITEMS-V1 with columns replaced by name (REPLACEMENTS are column-infos);
a replacement whose name is new is appended."
  (let ((table (items-v1)))
    (dolist (r replacements table)
      (let ((pos (position (sql-orm:column-info-name r) (cdr table)
                           :key #'sql-orm:column-info-name :test #'string-equal)))
        (if pos
            (setf (nth pos (cdr table)) r)
            (setf (cdr table) (append (cdr table) (list r))))))))

(defun items-without (name)
  (let ((table (items-v1)))
    (cons (car table)
          (remove name (cdr table) :key #'sql-orm:column-info-name
                                   :test #'string-equal))))

(defun notes-v1 ()
  (tbl :notes
       (col "id" :integer :primary-key t :autoincrement t)
       (col "body" :text)))

(defun tags-v1 ()
  (tbl :tags
       (col "id" :integer :primary-key t)
       (col "label" (list :varchar 32))))

(defun users-v1 ()
  (tbl :users
       (col "id" :integer :primary-key t :autoincrement t)
       (col "email" :text :not-null t :unique t)
       (col "active" :boolean :default t)))

;;; ---------------------------------------------------------------------------
;;; Corpus
;;;
;;; Row: (id a b complete-kinds default-kinds live)
;;;   complete-kinds  op classes of (diff-schema a b :complete t), in order
;;;   default-kinds   op classes of (diff-schema a b)  — the 0.1 policy
;;;   live            the pair runs on SQLite in both directions
;;;                   (upgrade a, upgrade the complete diff, downgrade it)
;;; ---------------------------------------------------------------------------

(defun corpus ()
  (list
   (list "create-table" '() (list (items-v1))
         '(sql-orm:create-table-op) '(sql-orm:create-table-op) t)
   (list "drop-table" (list (items-v1)) '()
         '(sql-orm:drop-table-op) '() t)
   (list "add-column" (list (items-v1)) (list (items-with (col "sku" :text)))
         '(sql-orm:add-column-op) '(sql-orm:add-column-op) t)
   (list "add-not-null-with-default" (list (items-v1))
         (list (items-with (col "stock" :integer :not-null t :default 0)))
         '(sql-orm:add-column-op) '(sql-orm:add-column-op) t)
   (list "drop-column" (list (items-v1)) (list (items-without "qty"))
         '(sql-orm:drop-column-op) '(sql-orm:drop-column-op) t)
   (list "type-change" (list (items-v1)) (list (items-with (col "qty" :text)))
         '(sql-orm:alter-column-op) '() t)
   (list "set-not-null" (list (items-v1))
         (list (items-with (col "qty" :integer :not-null t)))
         '(sql-orm:alter-column-op) '() t)
   (list "drop-not-null" (list (items-v1))
         (list (items-with (col "name" :text)))
         '(sql-orm:alter-column-op) '() t)
   (list "set-default" (list (items-v1))
         (list (items-with (col "qty" :integer :default 0)))
         '(sql-orm:alter-column-op) '() t)
   (list "change-default" (list (items-with (col "qty" :integer :default 0)))
         (list (items-with (col "qty" :integer :default 1)))
         '(sql-orm:alter-column-op) '() t)
   (list "drop-default" (list (items-with (col "name" :text :not-null t :default "x")))
         (list (items-v1))
         '(sql-orm:alter-column-op) '() t)
   (list "alter-all" (list (items-v1))
         (list (items-with (col "qty" :real :not-null t :default 1)))
         '(sql-orm:alter-column-op) '() t)
   (list "varchar-length" (list (tags-v1))
         (list (tbl :tags (col "id" :integer :primary-key t)
                    (col "label" (list :varchar 64))))
         '(sql-orm:alter-column-op) '() t)
   (list "unique-toggle" (list (items-v1))
         (list (items-with (col "name" :text :not-null t :unique t)))
         '(sql-orm:drop-column-op sql-orm:add-column-op) '() nil)
   (list "pk-toggle" (list (tags-v1))
         (list (tbl :tags (col "id" :integer) (col "label" (list :varchar 32))))
         '(sql-orm:drop-column-op sql-orm:add-column-op) '() nil)
   (list "multi-table"
         (list (items-v1) (notes-v1) (tags-v1))
         (list (items-with (col "qty" :integer :default 0) (col "sku" :text))
               (tbl :tags (col "id" :integer :primary-key t) (col "color" :text))
               (users-v1))
         '(sql-orm:add-column-op sql-orm:alter-column-op
           sql-orm:drop-column-op sql-orm:add-column-op
           sql-orm:create-table-op
           sql-orm:drop-table-op)
         '(sql-orm:add-column-op
           sql-orm:add-column-op sql-orm:drop-column-op
           sql-orm:create-table-op)
         t)
   (list "identity" (list (items-v1) (users-v1)) (list (items-v1) (users-v1))
         '() '() t)
   (list "reorder-only" (list (items-v1) (notes-v1))
         (list (notes-v1)
               (tbl :items
                    (col "qty" :integer)
                    (col "id" :integer :primary-key t :autoincrement t)
                    (col "name" :text :not-null t)))
         '() '() t)
   (list "name-case" (list (items-v1))
         (list (cons "ITEMS" (mapcar (lambda (c)
                                       (let ((c (sql-orm::copy-column-info c)))
                                         (setf (sql-orm:column-info-name c)
                                               (string-upcase (sql-orm:column-info-name c)))
                                         c))
                                     (cdr (items-v1)))))
         '() '() t)))

;;; DEFTEST-PARAMETRIZE :ROWS splices row values into the test as forms, so
;;; parametrized tests take a corpus id (a self-evaluating string) and build
;;; the snapshots inside the body.

(defun corpus-ids (&key live)
  "Corpus ids, optionally only the rows that run on SQLite."
  (loop for row in (corpus)
        when (or (not live) (sixth row)) collect (first row)))

(defun %corpus-row (id)
  (or (find id (corpus) :key #'first :test #'string=)
      (error "no corpus row ~S" id)))

(defun corpus-pair (id)
  "Fresh (values a b) for corpus row ID."
  (let ((row (%corpus-row id)))
    (values (second row) (third row))))

(defun corpus-kinds (id)
  "Fresh (a b complete-kinds default-kinds) for corpus row ID."
  (let ((row (%corpus-row id)))
    (list (second row) (third row) (fourth row) (fifth row))))

;;; ---------------------------------------------------------------------------
;;; Canned connection (PostgreSQL reflection without a server)
;;; ---------------------------------------------------------------------------

(defclass canned-connection (sql-protocol:sql-connection)
  ((responses :initarg :responses :reader canned-responses
              :documentation "Alist SQL string → list of plist rows.")
   (log :initform '() :accessor canned-log
        :documentation "(sql params) per EXECUTE, newest first."))
  (:default-initargs :raw nil))

(defclass canned-result (sql-protocol:sql-result)
  ((rows :initarg :rows :accessor canned-rows))
  (:default-initargs :raw-query nil))

(defmethod sql-protocol:execute ((connection canned-connection) sql &optional params)
  (push (list sql params) (canned-log connection))
  (let ((hit (assoc sql (canned-responses connection) :test #'string=)))
    (unless hit
      (error "canned-connection: unexpected SQL ~S" sql))
    (make-instance 'canned-result :rows (copy-list (cdr hit)))))

(defmethod sql-protocol:fetch ((result canned-result) &key)
  (pop (canned-rows result)))

(defmethod sql-protocol:fetch-all ((result canned-result) &key)
  (prog1 (canned-rows result)
    (setf (canned-rows result) nil)))
