// non-uniform stack

type A = &{ push : α ⅋ (B; A), free : skip }
type B = &{ push : α ⅋ (B; B), pop  : α^ ⊗ skip }

None(x : A; γ, y : γ^ ⊗ 1) =
    x▹{ push : x(u).(z : ((A; γ) ⅋ ⊥)^)(Some⟨u,x,z⟩ | z(x).z().None⟨x,y⟩)
       , free : y⟨x⟩.y[] }

Some(v : α, x : B; γ, y : γ^ ⊗ 1) =
    x▹{ push : x(u).(z : ((B; γ) ⅋ ⊥)^)(Some⟨u,x,z⟩ | z(x).z().Some⟨v,x,y⟩)
       , pop  : x⟨v⟩.y⟨x⟩.y[] }