// server and infinite client

type A = &{ req : A; ⊕{ resp : skip }, stop : skip }

Server(x : A; α, y : α^ ⊗ 1) =
    x▹{ req  : (z : &{ resp : α^ } ⊗ 1)(Server⟨x,z⟩ | z(x).z().x◃resp.y⟨x⟩.y[])
       , stop : y⟨x⟩.y[] }

Client(x : (A; α)^) = x◃req.Client⟨x⟩
