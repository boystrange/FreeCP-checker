// tree serialization

type A = &{ node : ?A; ?A; ⊥, leaf : ⊥     }
type B = +{ node : B; B,      leaf : skip }

Ser(x : A, y : B; b, z : !b^; 1) =
    x▹{
        node :
            x(u).
            x(v).
            x().
            y◃node.
            new (w : !(B; b)^; 1)
                Ser(u, y, w)
            in
                w(y).
                w().
                Ser(v, y, z),
        leaf :
            x().
            y◃leaf.
            z⟨y⟩.
            z[]
    }