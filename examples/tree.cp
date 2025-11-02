// tree serialization

type A = &{ node : ?A; ?A; 1^, leaf : 1^   }
type B = +{ node : B; B,       leaf : skip }

Ser(x : A, y : B; b, z : !b^; 1) =
    case x {
        node :
            x(u).
            x(v).
            wait x.
            y[node].
            new (w : !(B; b)^; 1)
                Ser(u, y, w)
            in
                w(y).
                wait w.
                Ser(v, y, z),
        leaf :
            wait x.
            y[leaf].
            z<y>.
            close z
    }