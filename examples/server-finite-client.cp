// server and finite client

type A = &{ req : A; ⊕{ resp : skip }, stop : skip }

Server(x : A; a, y : a^ ⊗ 1) =
    x▹{
        req :
            new (z : &{ resp : a^ } ⊗ 1)
                Server(x, z)
            in
                z(x).
                z().
                x◃resp.
                y⟨x⟩.
                y[]
    ,   stop :
            y⟨x⟩.
            y[]
    }

Client(x : A^; 1) =
    x◃req.
    x◃req.
    x◃req.
    x◃stop.
    x▹resp.
    x▹resp.
    x▹resp.
    x[]
