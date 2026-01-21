// tree deserialization

type A = &{ node : A ⅋ A ⅋ ⊥, leaf : ⊥ }
type B = ⊕{ node : B; B, leaf : skip }

Serialize(x : A, y : B; β, z : β^ ⊗ 1) =
    x▹{ node : x(u).x(v).x().y◃node.
		(w : ((B; β) ⅋ ⊥)^)(Serialize⟨u,y,w⟩ | w(y).w().Serialize⟨v,y,z⟩)
       , leaf : x().y◃leaf.z⟨y⟩.z[] }