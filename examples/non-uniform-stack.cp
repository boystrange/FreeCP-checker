// non-uniform stack

type B = rec X.&{ push : α ⅋ (X; X), pop : α^ ⊗ skip }
type A = rec X.&{ push : α ⅋ (B; X), free : skip }

None(x : A; γ, y : γ^ ⊗ 1) =
    x▹{ push :
            x(u).
            new (z : (A; γ) ⅋ ⊥)
                z(x).z().None(x, y)
            in Some(u, x, z)
       , free : y⟨x⟩.y[] }

Some(v : α, x : B; γ, y : γ^ ⊗ 1) =
    x▹{ push :
            x(u).
            new (z : (B; γ) ⅋ ⊥)
                z(x).z().Some(v, x, y)
            in Some(u, x, z)
       , pop : x⟨v⟩.y⟨x⟩.y[] }