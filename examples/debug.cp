
type B = &{ pop  : !a^   }

Some(v : a, x : B; c, y : !c^; 1) =
    case x {
        pop :
            x<v>.
            y<x>.
            close y
    }