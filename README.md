# gris

catlang experiments (warning: awful code)

## steel

`steel` is the current interpreter draft

## iron

`iron` is the second interpreter draft i've written

    $ dune exec -- ./iron/main.exe 'def over (swap dup bury); def nip (swap drop); 3 4 over + *'
    Inferred effect: [] -> [Int]
    User definitions:
        nip : [a b] -> [b]
        over : [a b] -> [a b a]
    Resulting stack: [21]

the type checker is broken though, and doesn't work with the latent stack like
the interpreter does
