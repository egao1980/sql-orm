(in-package #:sql-orm)

;;; ---------------------------------------------------------------------------
;;; Conditions
;;; ---------------------------------------------------------------------------

(define-condition orm-error (error)
  ((message :initarg :message :reader orm-error-message))
  (:report (lambda (c s)
             (format s "sql-orm: ~A" (orm-error-message c)))))

(define-condition unknown-model (orm-error)
  ((name :initarg :name :reader unknown-model-name))
  (:report (lambda (c s)
             (format s "sql-orm: unknown model ~S" (unknown-model-name c)))))

(define-condition missing-primary-key (orm-error) ())

;;; ---------------------------------------------------------------------------
;;; Connection binding
;;; ---------------------------------------------------------------------------

(defvar *orm-connection* nil
  "Current sql-protocol connection for ORM ops.")

(defun use-connection (connection)
  "Bind CONNECTION as the ORM default (also mirrors sql-protocol:*sql-connection*)."
  (check-type connection sql-connection)
  (setf *orm-connection* connection
        *sql-connection* connection)
  connection)

(defun orm-connection (&optional (connection *orm-connection*))
  (or connection
      *sql-connection*
      (error 'orm-error :message "no ORM connection; call use-connection or with-orm-connection")))

(defmacro with-orm-connection ((var &rest connect-keys) &body body)
  "Connect (or reuse), bind *ORM-CONNECTION*, run BODY."
  `(sql-protocol:with-connection (,var ,@connect-keys)
     (let ((*orm-connection* ,var))
       ,@body)))

(defmacro with-orm-transaction ((&optional (connection '*orm-connection*)) &body body)
  `(sql-protocol:with-transaction ((orm-connection ,connection))
     ,@body))

;;; ---------------------------------------------------------------------------
;;; Column / model metadata
;;; ---------------------------------------------------------------------------

(defstruct column-info
  name
  type
  (primary-key nil)
  (autoincrement nil)
  (not-null nil)
  (unique nil)
  default)

(defclass model-class ()
  ((name :initarg :name :reader model-class-name)
   (class :initarg :class :reader model-class-clos)
   (table-name :initarg :table-name :accessor model-class-table-name)
   (columns :initarg :columns :accessor model-class-columns :initform nil)
   (relations :initarg :relations :accessor model-class-relations :initform nil)
   (computes :initarg :computes :accessor model-class-computes :initform nil))
  (:documentation "Registry entry for a defmodel class."))

(defvar *model-registry* (make-hash-table :test #'eq)
  "Map model name symbol → model-class.")

(defun register-model (meta)
  (setf (gethash (model-class-name meta) *model-registry*) meta)
  meta)

(defun find-model (name &key (errorp t))
  (let ((key (if (symbolp name) name (class-name (class-of name)))))
    (or (gethash key *model-registry*)
        (when errorp
          (error 'unknown-model :name key :message
                 (format nil "unknown model ~S" key))))))

(defun list-models ()
  (loop for v being the hash-values of *model-registry* collect v))

(defun model-class-of (object-or-name)
  (find-model object-or-name))

(defun model-table-name (object-or-name)
  (model-class-table-name (find-model object-or-name)))

(defun model-columns (object-or-name)
  (model-class-columns (find-model object-or-name)))

(defun model-primary-key (object-or-name)
  (or (find-if #'column-info-primary-key (model-columns object-or-name))
      (error 'missing-primary-key
             :message (format nil "~S has no primary key column"
                              (model-class-name (find-model object-or-name))))))

;;; ---------------------------------------------------------------------------
;;; Persistent instance base
;;; ---------------------------------------------------------------------------

(defclass model ()
  ((%new :initform t :accessor model-new-p
         :documentation "T until successfully persisted with a primary key."))
  (:documentation "Mixin for all defmodel classes."))

(defun %keywordize (name)
  (intern (string-upcase (string name)) :keyword))

(defun %slot-initarg (slot-name)
  (%keywordize slot-name))

(defun default-table-name (class-name)
  "USER → :user (sql-query downcases identifiers)."
  (%keywordize class-name))
