# lmbd

simple lambda-calculus parser

```
\f.(\x.(f (x x)) \x.(f (x x))) // fix
Lambda "f" (App (Lambda "x" (App (Const "f") (App (Const "x") (Const "x")))) (Lambda "x" (App (Const "f") (App (Const "x") (Const "x")))))
```

