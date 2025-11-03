# gris

catlang experiments (warning: awful code)

`iron` is the current interpreter draft (`mini.ml` was the first one I made)

    $ dune exec -- ./iron/main.exe 'def over (swap dup bury); def nip (swap drop); 3 4 over + *'
    Inferred effect: [] -> [Int]
    User definitions:
        nip : [a b] -> [b]
        over : [a b] -> [a b a]
    Resulting stack: [21]

## to do (`iron`)

- [x] type inference (needed for word definitions)
  - still WIP, doesn't do the latent stack stuff the interpreter does, so the
    inferred effect for non-postfix code is wrong
- [x] word definitions
- [ ] latent stack transformation (lift from runtime to type checking)
