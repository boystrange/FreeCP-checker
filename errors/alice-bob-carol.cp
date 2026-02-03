// light type system incomplete

type A = &{ a : A, b : skip }
type B = &{ c : B, d : 1 }
type C = &{ a : C, b : B }

Alice(x : A; α, u : α^ ⊗ 1) = x▹{ a : Alice⟨x,u⟩ , b : u⟨x⟩.u[] }
Bob(x : B)                  = x▹{ c : Bob⟨x⟩     , d : x[]      }
Carol(x : C)                = x▹{ a : Carol⟨x⟩   , b : Bob⟨x⟩   }

Main(y : (A; B) ⅋ &{ tt : ⊥, ff : ⊥ }) =
  y(x).y▹{ tt : y().(u : (B ⅋ ⊥)^)(Alice⟨x,u⟩ | u(x).u().Bob⟨x⟩)
          , ff : y().Carol⟨x⟩ }
