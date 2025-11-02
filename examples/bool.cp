type Bool = ⊕{ tt : 1, ff : 1 }

Not(x : Bool^, y : Bool) =
    x▹{
        tt : x().y◃ff.y[],
        ff : x().y◃tt.y[]
    }

Copy(x : Bool^, y : Bool) =
    new (z : Bool) Not(x, z) in Not(z, y)

Drop(x : Bool^, y : 1) =
    x▹{
        tt : x().y[],
        ff : x().y[]
    }

And(x : Bool^, y : Bool^, z : Bool) =
    x▹{
        tt : x(). y ↔ z,
        ff : x().
             new (u : 1) Drop(y, u) in u().z◃tt.z[]
    }
