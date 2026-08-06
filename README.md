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
| `make-migration` | thin handle (`revision` / `down-revision` reserved for a runner) |

No versioned migration product here — just the reversible op algebra an Alembic-style package can version.

## Test / demo

```bash
ros -l scripts/ci-test.lisp   # after deps on ASDF registry
ros -l examples/demo.lisp -q
```
