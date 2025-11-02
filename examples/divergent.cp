type A = &{ req : A; +{ resp : skip }, stop : skip }

Server(x : A; a, y : (!^a); 1) =
  case x {
    req :
        new (z : (!(&{ resp : ^a })); 1)
            Server(x, z)
        in
            z(x).
            wait z.
            x[resp].
            y<x>.
            close y,
    stop :
        y<x>.
        close y
  }

  Client(x : ^A; a) = x[req].Client(x)
