// tree deserialization

type A = rec X.&{ node : X ⅋ X ⅋ ⊥, leaf : ⊥ }
type B = rec X.⊕{ node : X; X, leaf : skip }

Serialize(x : A, y : B; β, z : β^ ⊗ 1) =
    x▹{ node :
            x(u).x(v).x().y◃node.
            new (w : (B; β) ⅋ ⊥)
              w(y).w().Serialize(v, y, z)
            in Serialize(u, y, w)
       , leaf : x().y◃leaf.z⟨y⟩.z[] }