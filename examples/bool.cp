type Bool  = ⊕{ tt : 1, ff : 1 }
type BoolC = &{ tt : ⊥, ff : ⊥ }

Not(x : BoolC, y : Bool) =
    x▹{
        tt : x().y◃ff.y[],
        ff : x().y◃tt.y[]
    }

Copy(x : BoolC, y : Bool) =
    new (z : Bool) Not(x, z) in Not(z, y)

Drop(x : BoolC, y : 1) =
    x▹{
        tt : x().y[],
        ff : x().y[]
    }

And(x : BoolC, y : BoolC, z : Bool) =
    x▹{
        tt : x(). y ↔ z,
        ff : x().
             new (u : 1) Drop(y, u) in u().z◃tt.z[]
    }
