(defpackage #:sql-orm
  (:use #:cl)
  (:nicknames #:stack-sql-orm)
  (:shadowing-import-from #:sql-query #:count)
  (:import-from #:sql-protocol
                #:sql-connection
                #:*sql-connection*
                #:with-connection
                #:with-transaction
                #:connect
                #:disconnect
                #:execute
                #:fetch
                #:fetch-all)
  (:import-from #:sql-query
                #:select #:insert-into #:update #:delete-from
                #:create-table #:drop-table #:alter-table
                #:columns #:from #:where #:order-by #:limit #:offset
                #:sql-values #:sql-set #:column #:add-column #:drop-column
                #:make-sql-table #:table-column #:create-table-from
                #:sql-table #:sql-table-name #:sql-table-columns
                #:column-def-name #:column-def-type #:column-def-primary-key
                #:column-def-autoincrement #:column-def-not-null
                #:column-def-unique #:column-def-default
                #:compile-sql #:execute-query #:fetch-query #:fetch-all-query
                #:dialect-for-connection #:label)
  (:export
   ;; connection
   #:*orm-connection*
   #:use-connection
   #:orm-connection
   #:with-orm-connection
   #:with-orm-transaction

   ;; model base + meta
   #:model
   #:model-new-p
   #:model-class
   #:model-class-of
   #:find-model
   #:list-models
   #:model-table-name
   #:model-columns
   #:model-primary-key
   #:column-info
   #:column-info-name
   #:column-info-type
   #:column-info-primary-key
   #:column-info-autoincrement
   #:column-info-not-null
   #:column-info-unique
   #:column-info-default

   ;; definition
   #:defmodel

   ;; persistence
   #:persist
   #:destroy
   #:refresh
   #:find-instance
   #:select-instances
   #:count-instances
   #:make-from-row

   ;; schema / migrations surface
   #:model-sql-table
   #:schema-snapshot
   #:diff-schema
   #:ensure-schema
   #:apply-schema-ops

   ;; conditions
   #:orm-error
   #:orm-error-message
   #:unknown-model
   #:missing-primary-key))

(in-package #:sql-orm)
