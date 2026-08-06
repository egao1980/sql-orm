(in-package #:sql-orm)

;;; defmodel — defclass-shaped, very lispy.
;;;
;;; (defmodel user ()
;;;   "Blog user."
;;;   (id :integer :primary-key t :autoincrement t)
;;;   (name :text :not-null t)
;;;   (email :text)
;;;   (:table users)
;;;   (:has-many posts post :key user-id)
;;;   (:belongs-to company company :key company-id)
;;;   (:compute label (self)
;;;     (format nil "~A <~A>" (name self) (email self))))

(defun %parse-defmodel-body (body)
  (let ((doc nil)
        (slots '())
        (table nil)
        (relations '())
        (computes '()))
    (dolist (form body)
      (cond
        ((stringp form)
         (setf doc form))
        ((and (consp form) (keywordp (first form)))
         (ecase (first form)
           (:table
            (setf table (second form)))
           (:has-many
            (destructuring-bind (rel-name model &key (key nil) foreign-key) (rest form)
              (push (list :has-many rel-name model :key (or key foreign-key))
                    relations)))
           (:belongs-to
            (destructuring-bind (rel-name model &key (key nil) foreign-key) (rest form)
              (push (list :belongs-to rel-name model :key (or key foreign-key))
                    relations)))
           (:compute
            (push (rest form) computes))
           (:sql-expr
            (push (list* :sql-expr (rest form)) computes))))
        ((consp form)
         (push form slots))
        (t
         (error 'orm-error
                :message (format nil "defmodel: bad form ~S" form)))))
    (values doc (nreverse slots) table (nreverse relations) (nreverse computes))))

(defun %parse-slot (slot-form)
  "Return (values column-info-plist clos-slot-definition)."
  (destructuring-bind (name type &rest keys) slot-form
    (unless (symbolp name)
      (error 'orm-error :message (format nil "defmodel: slot name must be symbol, got ~S" name)))
    (let* ((plist keys)
           (accessor (getf plist :accessor name))
           (initarg (getf plist :initarg (%slot-initarg name)))
           (primary-key (getf plist :primary-key))
           (autoincrement (getf plist :autoincrement))
           (not-null (getf plist :not-null))
           (unique (getf plist :unique))
           (has-default (not (eq (getf plist :default '%missing) '%missing)))
           (default (getf plist :default)))
      (values
       `(:name ,name
         :type ,type
         :primary-key ,primary-key
         :autoincrement ,autoincrement
         :not-null ,(or not-null primary-key)
         :unique ,unique
         :default ,(if has-default default nil))
       `(,name :initarg ,initarg
               :accessor ,accessor
               ,@(when has-default `(:initform ',default)))))))

(defun %column-info-form (plist)
  `(make-column-info
    :name ',(getf plist :name)
    :type ',(getf plist :type)
    :primary-key ,(getf plist :primary-key)
    :autoincrement ,(getf plist :autoincrement)
    :not-null ,(getf plist :not-null)
    :unique ,(getf plist :unique)
    :default ',(getf plist :default)))

(defun %specialize-lambda-list (lambda-list class-name)
  "Specialize the first required parameter to CLASS-NAME."
  (cond
    ((null lambda-list) `((,(gensym "SELF") ,class-name)))
    ((member (first lambda-list) '(&optional &key &rest &aux))
     (error 'orm-error :message "defmodel :compute needs a required parameter"))
    ((and (consp (first lambda-list)) (symbolp (first (first lambda-list))))
     lambda-list)
    ((symbolp (first lambda-list))
     (cons `(,(first lambda-list) ,class-name) (rest lambda-list)))
    (t
     (error 'orm-error :message (format nil "bad :compute lambda-list ~S" lambda-list)))))

(defmacro defmodel (name direct-superclasses &body body)
  "Define a persistent CLOS model.

Slot forms: (name type &key primary-key autoincrement not-null unique default
                           accessor initarg)
Options:
  (:table name)
  (:has-many accessor model :key fk-slot)
  (:belongs-to accessor model :key fk-slot)
  (:compute name lambda-list . body)  — method; first arg specialized to NAME"
  (multiple-value-bind (doc slots table relations computes)
      (%parse-defmodel-body body)
    (let ((clos-slots '())
          (col-plists '())
          (meta (gensym "META"))
          (compute-forms '())
          (table-name (or table (default-table-name name))))
      (dolist (s slots)
        (multiple-value-bind (plist def) (%parse-slot s)
          (push plist col-plists)
          (push def clos-slots)))
      (setf col-plists (nreverse col-plists)
            clos-slots (nreverse clos-slots))
      (dolist (c computes)
        (unless (eq (first c) :sql-expr)
          (destructuring-bind (cname lambda-list &body cbody) c
            (push `(defmethod ,cname ,(%specialize-lambda-list lambda-list name)
                     ,@cbody)
                  compute-forms))))
      `(progn
         (defclass ,name (,@direct-superclasses model)
           ,clos-slots
           ,@(when doc `((:documentation ,doc))))
         (let ((,meta (make-instance 'model-class
                                     :name ',name
                                     :class (find-class ',name)
                                     :table-name ',table-name
                                     :columns (list ,@(mapcar #'%column-info-form col-plists))
                                     :relations ',relations
                                     :computes ',computes)))
           (register-model ,meta)
           (%install-relations ,meta)
           ,@ (nreverse compute-forms)
           ,meta)))))
