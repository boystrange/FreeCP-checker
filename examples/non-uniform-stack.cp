// non-uniform stack

type A = &{ push : ?a; B; A, free : skip }
type B = &{ push : ?a; B; B, pop  : !a^  }

None(x : A; c, y : !c^; 1) =
    x▹{
        push :
            x(u).
            new (z : !(A; c)^; 1)
                Some(u, x, z)
            in
                z(x).
                z().
                None(x, y),
        free :
            y⟨x⟩.
            y[]
    }

Some(v : a, x : B; c, y : !c^; 1) =
    x▹{
        push :
            x(u).
            new (z : !(B; c)^; 1)
                Some(u, x, z)
            in
                z(x).
                z().
                Some(v, x, y),
        pop :
            x⟨v⟩.
            y⟨x⟩.
            y[]
    }