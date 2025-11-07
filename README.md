# Gris

These are some statically typed concatenative experiments.

**Iron** is the current (public) interpreter draft.
Executing the interpreter can be done like so:

```
$ dune exec -- ./iron/main.exe < ./iron/examples/addmul.iron
User definitions:
  ! : [Int] -> []
  println : [Str] -> []
  addmul : [Int Int Int] -> [Int]
  scream : [Str] -> [Str]
9!
```

There's no row polymorphism in the type system yet, so inferring the types of
block literals (quotations) doesn't work yet (as of now it simply throws an
exception.)
