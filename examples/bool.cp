type Bool = +{ tt : 1, ff : 1 }

Not(x : ^Bool, y : Bool) =
    case x {
        tt : wait x.y[ff].close y,
        ff : wait x.y[tt].close y
    }

Copy(x : ^Bool, y : Bool) =
    new (z : Bool) Not(x, z) in Not(z, y)

Drop(x : ^Bool, y : 1) =
    case x {
        tt : wait x.close y,
        ff : wait x.close y
    }

And(x : ^Bool, y : ^Bool, z : Bool) =
    case x {
        tt : wait x. y = z,
        ff : wait x.
             new (u : 1) Drop(y, u) in wait u.z[tt].close z
    }
