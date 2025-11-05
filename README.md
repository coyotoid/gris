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
- [ ] rework lexer and parser... perhaps?
  - would like to have a proper expression tree as opposed to working directly
    on a s-exp
