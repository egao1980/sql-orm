(in-package #:sql-orm)

;;; Relationship methods installed from defmodel options.
;;;
;;; (:has-many posts post :key user-id)
;;;   → (posts user) => (select-instances 'post :where (:= :user-id (id user)))
;;;
;;; (:belongs-to company company :key company-id)
;;;   → (company user) => (find-instance 'company (company-id user))

(defun %install-relations (meta)
  (dolist (rel (model-class-relations meta))
    (destructuring-bind (kind accessor model &key key) rel
      (unless key
        (error 'orm-error
               :message (format nil "~A ~A on ~A needs :key"
                                kind accessor (model-class-name meta))))
      (ecase kind
        (:has-many
         (%install-has-many (model-class-name meta) accessor model key))
        (:belongs-to
         (%install-belongs-to (model-class-name meta) accessor model key))))))

(defun %install-has-many (owner-class accessor target-model fk-slot)
  (let ((fk (%keywordize fk-slot))
        (pk-reader nil))
    (declare (ignore pk-reader))
    (ensure-generic-function accessor
                             :lambda-list '(object &key connection)
                             :generic-function-class 'standard-generic-function)
    (eval
     `(defmethod ,accessor ((object ,owner-class)
                            &key (connection (orm-connection)))
        (let* ((pk (model-primary-key object))
               (pk-val (slot-value object (column-info-name pk))))
          (select-instances ',target-model
                            :where (:= ,fk pk-val)
                            :connection connection))))))

(defun %install-belongs-to (owner-class accessor target-model fk-slot)
  (ensure-generic-function accessor
                           :lambda-list '(object &key connection)
                           :generic-function-class 'standard-generic-function)
  (eval
   `(defmethod ,accessor ((object ,owner-class)
                          &key (connection (orm-connection)))
      (let ((fk-val (slot-value object ',fk-slot)))
        (when fk-val
          (find-instance ',target-model fk-val :connection connection))))))
