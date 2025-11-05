# gris

catlang experiments (warning: awful code)

## iron

`iron` is the current interpreter draft

    $ dune exec -- ./iron/main.exe 'def over (swap dup bury); def nip (swap drop); 3 4 over + *'
    Inferred effect: [] -> [Int]
    User definitions:
        nip : [a b] -> [b]
        over : [a b] -> [a b a]
    Resulting stack: [21]

the type checker is still a work in progress, as it needs to be reworked to use
the concept of the latent stack as the interpreter does, so programs that would
execute correctly will have their effect incorrectly inferred.

i am working on getting this done, i just need to figure out how :-)

### to-do

- [ ] finish the type checker
- [x] rework lexer and parser... perhaps?
  - [ ] done, but still needs some work, especially since compiling the parser
    throws some warnings (baby's first parser yay \^_\^)

    ```
    Warning: one state has reduce/reduce conflicts.
    Warning: one reduce/reduce conflict was arbitrarily resolved.
    ```
