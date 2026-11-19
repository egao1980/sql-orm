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

   ;; schema snapshots
   #:model-sql-table
   #:schema-snapshot
   #:diff-schema
   #:ensure-schema

   ;; reversible schema ops (Alembic-style foundation)
   #:schema-op
   #:create-table-op
   #:drop-table-op
   #:add-column-op
   #:drop-column-op
   #:schema-op-table-name
   #:schema-op-columns
   #:schema-op-column
   #:schema-op-upgrade
   #:schema-op-downgrade
   #:invert-schema-op
   #:schema-ops-upgrade
   #:schema-ops-downgrade
   #:upgrade-schema
   #:downgrade-schema
   #:apply-schema-ops

   ;; 0.2.0 algebra (S1)
   #:alter-column-op
   #:schema-op-column-name
   #:schema-op-from-column
   #:schema-op-to-column
   #:apply-ops-to-snapshot
   #:schema-snapshot-equal
   #:column-info-equal
   #:canonical-type-sql
   #:schema-dialect-key
   #:schema-op-footprint
   #:reflect-schema
   #:reflect-schema-using
   #:reflection-query
   #:dialect-schema-op-statements

   ;; migration handle
   #:schema-migration
   #:make-migration
   #:schema-migration-name
   #:schema-migration-ops
   #:schema-migration-revision
   #:schema-migration-down-revision
   #:migration-upgrade-ops
   #:migration-downgrade-ops

   ;; conditions
   #:orm-error
   #:orm-error-message
   #:unknown-model
   #:missing-primary-key
   #:invalid-schema-op
   #:invalid-schema-op-op
   #:snapshot-conflict
   #:snapshot-conflict-op
   #:reflection-unsupported
   #:reflection-unsupported-dialect))

(in-package #:sql-orm)
