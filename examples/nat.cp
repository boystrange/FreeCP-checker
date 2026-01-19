type Nat  = rec X.⊕{ zero : 1, succ : X }
type NatC = rec X.&{ zero : ⊥, succ : X }

Succ(x : NatC, y : Nat) = y◃succ.x ↔ y

Drop(x : NatC, y : 1) =
  x▹{ zero : x ↔ y
     , succ : Drop(x, y) }

Add(x : NatC, y : NatC, z : Nat) =
  x▹{ zero : x(). y ↔ z
     , succ : z◃succ.Add(x, y, z) }
