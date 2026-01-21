type Bool  = ⊕{ tt : 1, ff : 1 }

True(x : Bool) = x◃tt.x[]

False(x : Bool) = x◃ff.x[]

Not(x : Bool^, y : Bool) =
    x▹{ tt : x().y◃ff.y[]
       , ff : x().y◃tt.y[] }

Copy(x : Bool^, y : Bool) = (z : Bool)(Not⟨x,z⟩ | Not⟨z,y⟩)

Drop(x : Bool^, y : 1) =
    x▹{ tt : x().y[]
       , ff : x().y[] }

And(x : Bool^, y : Bool^, z : Bool) =
    x▹{ tt : x().y ↔ z
       , ff : x().(u : 1)(Drop⟨y,u⟩ | u().z◃tt.z[]) }
