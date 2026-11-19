# sql-orm

Lispy **CLOS** ORM for [cl-stack](https://github.com/egao1980/cl-stack) — models, relationships, calculated fields, schema diff — on [`sql-query`](https://github.com/egao1980/sql-query) + [`sql-protocol`](https://github.com/egao1980/sql-protocol).

OCI: `ghcr.io/egao1980/cl-systems/sql-orm:0.1.0` · nick **`stack-sql-orm`**

Not a Mito wrapper. Feature *ideas* overlap common ORMs; the API is plain Lisp (`defclass`-shaped `defmodel`, generics, sql-query sexps for filters).

## Shape

```lisp
(asdf:load-system "sql-backend-sqlite3")
(asdf:load-system "sql-query-sqlite3")
(asdf:load-system "sql-orm")

(defmodel user ()
  (id :integer :primary-key t :autoincrement t)
  (name :text :not-null t)
  (email :text)
  (:table users)
  (:has-many posts post :key user-id)
  (:compute label (self)
    (format nil "~A <~A>" (name self) (email self))))

(with-orm-connection (c :driver :sqlite3 :database-name ":memory:")
  (ensure-schema c 'user)
  (let ((u (persist (make-instance 'user :name "ada" :email "a@x"))))
    (label u)                          ; calculated
    (find-instance 'user (id u))
    (select-instances 'user :where (:= :name "ada"))))

;; schema as data — structural diff → reversible ops → sql-query AST
(let ((mig (make-migration old-snap new-snap :name "add-email" :revision "0002")))
  (upgrade-schema c mig)
  (downgrade-schema c mig))          ; rollback
```

| Surface | Notes |
|---------|--------|
| `defmodel` | columns + `:table` / `:has-many` / `:belongs-to` / `:compute` |
| `persist` / `destroy` / `refresh` | generics |
| `find-instance` / `select-instances` | filters are **sql-query** exprs |
| `schema-snapshot` / `diff-schema` | model-level → `schema-op` list |
| `schema-op-upgrade` / `schema-op-downgrade` | op → sql-query DDL AST |
| `upgrade-schema` / `downgrade-schema` | apply / roll back |
| `diff-schema :complete t [:dialect d]` | faithful diff (drops + `alter-column-op`s, locked order); default `:complete nil` is the 0.1 policy |
| `alter-column-op` | type / nullability / default change in place; portable `ALTER COLUMN` DDL, table rebuild on SQLite |
| `apply-ops-to-snapshot` | pure fold of ops (list, op or migration) over a snapshot; signals `snapshot-conflict` |
| `schema-snapshot-equal` / `column-info-equal` | order-insensitive, dialect-aware equality |
| `schema-op-footprint` | `"table:<t>"` / `"column:<t>.<c>"` resources an op touches |
| `reflect-schema` | live snapshot from the catalog (SQLite, PostgreSQL); `:exclude`, `:dialect`, `:schema` |
| `make-migration` | thin handle (`revision` / `down-revision` reserved for a runner) |

No versioned migration product here — just the reversible op algebra an Alembic-style package can version.

## SQLite alter-column rebuild

SQLite has no `ALTER COLUMN`, so `upgrade-schema` / `downgrade-schema` rebuild the table inside the caller's transaction: `CREATE TABLE <t>__sql_orm_rebuild`, `INSERT … SELECT`, `DROP TABLE <t>`, `ALTER TABLE … RENAME TO <t>`. Since 0.3.0 the rebuild reflects and keeps what `column-info` does not model:

| Kept | Read from | Re-created as |
|------|-----------|---------------|
| foreign keys (column or table, composite, `ON DELETE` / `ON UPDATE`) | `pragma_foreign_key_list` | table `FOREIGN KEY` constraints, same order |
| composite `PRIMARY KEY` | `pragma_table_info` (`pk` order) | table `PRIMARY KEY (…)` |
| multi-column `UNIQUE` | `pragma_index_list` origin `u` + `pragma_index_info` | table `UNIQUE (…)` |
| `CHECK` (column or table, named or not) | the `CREATE TABLE` text in `sqlite_master` | table `CHECK (…)`, name kept |
| indexes (any, partial, expression) and triggers | their `sqlite_master.sql` | the same statement, after the rename |

Views are not dropped. Constraint names other than `CHECK` names are not kept.

What the rebuild cannot keep makes it signal **`sqlite-rebuild-unsupported`** (an `orm-error`; readers `-table`, `-objects`, `-op`) before anything runs, with a **`continue`** restart that rebuilds anyway and loses the listed objects:

- `COLLATE`, generated columns, `ON CONFLICT`, `DEFERRABLE` foreign keys, `WITHOUT ROWID`, `STRICT`;
- any foreign key **into** the table while `PRAGMA foreign_keys` is `ON` — `DROP TABLE` would run the children's `ON DELETE` actions (a cascade deletes their rows). Turn `foreign_keys` off outside the transaction first, as SQLite's own procedure does.

```lisp
(handler-bind ((sql-orm:sqlite-rebuild-unsupported #'continue)) ; accept the loss
  (sql-orm:upgrade-schema c ops))
```

Portable `ALTER COLUMN` DDL compares types and defaults by the SQL the dialect renders (`:integer` = `"integer"`; PostgreSQL `int4` = `integer`), so no redundant `SET DATA TYPE`.

## Test / demo

```bash
(asdf:test-system "sql-orm")
ros -l examples/demo.lisp -q
```
