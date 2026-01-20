// server and finite client

type A = rec X.&{ req : X; ⊕{ resp : skip }, stop : skip }
type B = rec X.⊕{ req : X; &{ resp : skip }, stop : skip }

Server(x : A; α, y : α^ ⊗ 1) =
    x▹{
        req :
            new (z : &{ resp : α^ } ⊗ 1)
                Server(x, z)
            in
                z(x).z().x◃resp.y⟨x⟩.y[]
    ,   stop : y⟨x⟩.y[]
    }

Client(x : B; 1) =
    x◃req.x◃req.x◃req.x◃stop.x▹resp.x▹resp.x▹resp.x[]
