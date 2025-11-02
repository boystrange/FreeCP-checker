
type A = &{ push : ?a; B; A, free : skip }
type B = &{ push : ?a; B; B, pop  : !a   }

None(x : (A; c), y : (!^c; 1)) =
    case x {
        push :
            x(u).
            new (z : !^(A; c); 1)
                Some(u, x, z)
            in
                z(x).
                wait z.
                None(x, y),
        free :
            y<x>.
            close y
    }

Some(v : a, x : (B; c), y : (!^c; 1)) =
    case x {
        push :
            x(u).
            new (z : !(a; B; c); 1)
                Some(u, x, z)
            in
                z(x).
                wait z.
                Some(v, x, y),
        pop :
            x<v>.
            y<x>.
            close y
    }