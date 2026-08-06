(in-package #:sql-orm)

(defun %col-keyword (column-info)
  (%keywordize (column-info-name column-info)))

(defun %slot-value-by-col (object column-info)
  (let ((name (column-info-name column-info)))
    (when (slot-boundp object name)
      (slot-value object name))))

(defun %set-slot-by-col (object column-info value)
  (setf (slot-value object (column-info-name column-info)) value))

(defun %row-get (row key)
  "Plist lookup; keys from sql-protocol are upcased keywords."
  (getf row (%keywordize key)))

(defun make-from-row (model-name row)
  "Hydrate a model instance from a plist row. Marks it as persisted."
  (let* ((meta (find-model model-name))
         (class (model-class-clos meta))
         (initargs '()))
    (dolist (col (model-class-columns meta))
      (let* ((key (%col-keyword col))
             (val (%row-get row key)))
        (when (or val (member key row))
          (setf initargs (list* (%slot-initarg (column-info-name col)) val initargs)))))
    (let ((obj (apply #'make-instance class initargs)))
      (setf (model-new-p obj) nil)
      obj)))

(defun %pk-value (object)
  (%slot-value-by-col object (model-primary-key object)))

(defun %dialect (connection)
  (ignore-errors (dialect-for-connection connection)))

(defun %exec (connection statement)
  (apply #'execute-query connection statement
         (let ((d (%dialect connection)))
           (when d (list :dialect d)))))

(defun %fetch-all (connection statement)
  (apply #'fetch-all-query connection statement
         (let ((d (%dialect connection)))
           (when d (list :dialect d)))))

(defun %fetch-one (connection statement)
  (apply #'fetch-query connection statement
         (let ((d (%dialect connection)))
           (when d (list :dialect d)))))

(defun %last-insert-id (connection)
  "SQLite-oriented; Postgres should use RETURNING in a later wave."
  (let ((row (fetch (execute connection "SELECT last_insert_rowid() AS id"))))
    (%row-get row :id)))

(defgeneric persist (object &key connection)
  (:documentation "INSERT or UPDATE OBJECT. Returns OBJECT."))

(defmethod persist ((object model) &key (connection (orm-connection)))
  (let* ((meta (find-model object))
         (table (model-class-table-name meta))
         (cols (model-class-columns meta))
         (pk (model-primary-key object))
         (pk-val (%pk-value object))
         (conn (orm-connection connection)))
    (cond
      ((or (model-new-p object) (null pk-val))
       (let* ((insert-cols (remove-if
                            (lambda (c)
                              (and (column-info-autoincrement c)
                                   (null (%slot-value-by-col object c))))
                            cols))
              (names (mapcar #'%col-keyword insert-cols))
              (vals (mapcar (lambda (c) (%slot-value-by-col object c)) insert-cols))
              (stmt (apply #'insert-into table
                           (list (apply #'columns names)
                                 (apply #'sql-values vals)))))
         (%exec conn stmt)
         (when (and (column-info-autoincrement pk) (null pk-val))
           (%set-slot-by-col object pk (%last-insert-id conn)))
         (setf (model-new-p object) nil)
         object))
      (t
       (let* ((set-cols (remove pk cols :test #'eq))
              (assigns (mapcar (lambda (c)
                                 (:= (%col-keyword c) (%slot-value-by-col object c)))
                               set-cols))
              (stmt (update table
                            (apply #'sql-set assigns)
                            (where (:= (%col-keyword pk) pk-val)))))
         (%exec conn stmt)
         (setf (model-new-p object) nil)
         object)))))

(defgeneric destroy (object &key connection)
  (:documentation "DELETE OBJECT by primary key."))

(defmethod destroy ((object model) &key (connection (orm-connection)))
  (let* ((meta (find-model object))
         (pk (model-primary-key object))
         (pk-val (%pk-value object))
         (conn (orm-connection connection)))
    (unless pk-val
      (error 'orm-error :message "cannot destroy instance without primary key"))
    (%exec conn
           (delete-from (model-class-table-name meta)
                        (where (:= (%col-keyword pk) pk-val))))
    (setf (model-new-p object) t)
    (%set-slot-by-col object pk nil)
    object))

(defgeneric refresh (object &key connection)
  (:documentation "Reload OBJECT slots from the database."))

(defmethod refresh ((object model) &key (connection (orm-connection)))
  (let* ((pk (model-primary-key object))
         (pk-val (%pk-value object)))
    (unless pk-val
      (error 'orm-error :message "cannot refresh instance without primary key"))
    (let ((fresh (find-instance (model-class-name (find-model object))
                                pk-val
                                :connection connection)))
      (unless fresh
        (error 'orm-error :message "row vanished during refresh"))
      (dolist (col (model-columns object))
        (%set-slot-by-col object col (%slot-value-by-col fresh col)))
      (setf (model-new-p object) nil)
      object)))

(defun find-instance (model-name pk-value &key (connection (orm-connection)))
  "Find one instance by primary key, or NIL."
  (let* ((meta (find-model model-name))
         (pk (model-primary-key model-name))
         (conn (orm-connection connection))
         (col-names (mapcar #'%col-keyword (model-class-columns meta)))
         (stmt (select (apply #'columns col-names)
                       (from (model-class-table-name meta))
                       (where (:= (%col-keyword pk) pk-value))))
         (row (%fetch-one conn stmt)))
    (when row
      (make-from-row model-name row))))

(defun select-instances (model-name &key where order-by limit offset
                                      (connection (orm-connection)))
  "Return a list of model instances. WHERE is a sql-query expression object."
  (let* ((meta (find-model model-name))
         (conn (orm-connection connection))
         (col-names (mapcar #'%col-keyword (model-class-columns meta)))
         (clauses (list (apply #'columns col-names)
                        (from (model-class-table-name meta))))
         (stmt (apply #'select
                      (append clauses
                              (when where (list (where where)))
                              (when order-by
                                (list (apply #'order-by
                                             (if (listp order-by) order-by (list order-by)))))
                              (when limit (list (limit limit)))
                              (when offset (list (offset offset))))))
         (rows (%fetch-all conn stmt)))
    (mapcar (lambda (row) (make-from-row model-name row)) rows)))

(defun count-instances (model-name &key where (connection (orm-connection)))
  (let* ((meta (find-model model-name))
         (conn (orm-connection connection))
         (stmt (apply #'select
                      (append (list (columns (label (count :*) :n))
                                    (from (model-class-table-name meta)))
                              (when where (list (where where))))))
         (row (%fetch-one conn stmt)))
    (or (%row-get row :n) 0)))
