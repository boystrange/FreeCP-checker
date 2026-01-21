type Nat = ⊕{ zero : 1, succ : Nat }

Succ(x : Nat^, y : Nat) = y◃succ.x ↔ y

Drop(x : Nat^, y : 1) =
  x▹{ zero : x ↔ y
     , succ : Drop⟨x,y⟩ }

Add(x : Nat^, y : Nat^, z : Nat) =
  x▹{ zero : x(). y ↔ z
     , succ : z◃succ.Add⟨x,y,z⟩ }
