# sql-orm

Lispy **CLOS** ORM for [cl-stack](https://github.com/egao1980/cl-stack) — models, relationships, calculated fields, schema diff — on [`sql-query`](https://github.com/egao1980/sql-query) + [`sql-protocol`](https://github.com/egao1980/sql-protocol).

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

;; schema as data
(diff-schema (schema-snapshot '(user)) new-snapshot)  ; → DDL stmts
```

| Surface | Notes |
|---------|--------|
| `defmodel` | columns + `:table` / `:has-many` / `:belongs-to` / `:compute` |
| `persist` / `destroy` / `refresh` | generics |
| `find-instance` / `select-instances` | filters are **sql-query** exprs |
| `schema-snapshot` / `diff-schema` / `ensure-schema` | model diffs → DDL |

## Test / demo

```bash
ros -l scripts/ci-test.lisp   # after deps on ASDF registry
ros -l examples/demo.lisp -q
```
