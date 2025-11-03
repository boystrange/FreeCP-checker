// non-uniform stack

type A = &{ push : ?α; B; A, free : skip }
type B = &{ push : ?α; B; B, pop  : !α^  }

None(x : A; γ, y : !γ^; 1) =
    x▹{
        push :
            x(u).
            new (z : !(A; γ)^; 1)
                Some(u, x, z)
            in
                z(x).
                z().
                None(x, y)
    ,   free :
            y⟨x⟩.
            y[]
    }

Some(v : α, x : B; γ, y : !γ^; 1) =
    x▹{
        push :
            x(u).
            new (z : !(B; γ)^; 1)
                Some(u, x, z)
            in
                z(x).
                z().
                Some(v, x, y)
    ,   pop :
            x⟨v⟩.
            y⟨x⟩.
            y[]
    }