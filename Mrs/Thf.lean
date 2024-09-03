import Mrs.Basic

namespace THF

open MRS (Var EP Constraint MRS)

def joinSep (l : List String) (sep : String) : String := l.foldr (fun s r => (if r == "" then s else r ++ sep ++ s)) ""

def Var.format.typeOnly (var : Var) : String :=
  match var with
  | {id := n, sort := s, props := #[]} =>
       if s == 'h' then
         s!"{s}{n}"
       else
         s!"{s}"
  | {id := n, sort := s, props := ps} =>
       if s == 'h' then
         s!"{s}{n}"
       else
         s!"{s}"

def Var.format.labelOnly (var : Var) : String :=
  match var with
  | {id := n, sort := s, props := #[]} =>
       s!"{s.toUpper}{n}"
  | {id := n, sort := s, props := ps} =>
       s!"{s.toUpper}{n}"

def Var.format.labelOnlyGround (var : Var) : String :=
  match var with
  | {id := n, sort := s, props := #[]} =>
       s!"{s}{n}"
  | {id := n, sort := s, props := ps} =>
       s!"{s}{n}"

def Var.format.pair (var : Var) : String :=
  match var with
  | {id := n, sort := s, props := #[]} =>
       if s == 'h' then
         s!"{s.toUpper}{n} : {s}{n}"
       else
         s!"{s.toUpper}{n} : {s}"
  | {id := n, sort := s, props := ps} =>
       if s == 'h' then
         s!"{s.toUpper}{n} : {s}{n}"
       else
         s!"{s.toUpper}{n} : {s}"

def EP.format.type (ep : EP) : String :=
  match ep with
  | {predicate := p, link := some (n,m), label := l, rargs := rs, carg := some c} =>
    let pairs := joinSep (rs.map fun a => Var.format.typeOnly a.2)  " > "
    "thf(" ++ Var.format.labelOnlyGround l ++ "_decl,type," ++ Var.format.labelOnlyGround l ++ ": " ++ pairs ++ " > string)."
  | {predicate := p, link := some (n,m), label := l, rargs := rs, carg := none} =>
    let pairs := joinSep (rs.map fun a => Var.format.typeOnly a.2)  " > "
    "thf(" ++ Var.format.labelOnlyGround l ++ "_decl,type," ++ Var.format.labelOnlyGround l ++ ": " ++ pairs ++ ")."
  | {predicate := p, link := none, label := l, rargs := rs, carg := some c} =>
    let pairs := joinSep (rs.map fun a => Var.format.typeOnly a.2)  " > "
    "thf(" ++ Var.format.labelOnlyGround l ++ "_decl,type," ++ Var.format.labelOnlyGround l ++ ": " ++ pairs ++ " > string)."
  | {predicate := p, link := none, label := l, rargs := rs, carg := none} =>
    let pairs := joinSep (rs.map fun a => Var.format.typeOnly a.2)  " > "
    "thf(" ++ Var.format.labelOnlyGround l ++ "_decl,type," ++ Var.format.labelOnlyGround l ++ ": " ++ pairs ++ ")."

def EP.format.axiom (ep : EP) : String :=
  match ep with
  | {predicate := p, link := some (n,m), label := l, rargs := rs, carg := some c} =>
    let pairs := joinSep (rs.map fun a => Var.format.pair a.2)  ","
    let args := joinSep (rs.map fun a => Var.format.labelOnly a.2)  " @ "
    "thf(" ++ Var.format.labelOnlyGround l ++ ",axiom," ++ "\n   " ++ Var.format.labelOnlyGround l ++ " = ( ^ [" ++ pairs ++ ",'" ++ s!"{c}" ++ "'] : (" ++ p ++ " @ " ++ args ++ ")))."
  | {predicate := p, link := some (n,m), label := l, rargs := rs, carg := none} =>
    let pairs := joinSep (rs.map fun a => Var.format.pair a.2)  ","
    let args := joinSep (rs.map fun a => Var.format.labelOnly a.2)  " @ "
    "thf(" ++ Var.format.labelOnlyGround l ++ ",axiom," ++ "\n   " ++ Var.format.labelOnlyGround l ++ " = ( ^ [" ++ pairs ++ "] : (" ++ p ++ " @ " ++ args ++ ")))."
  | {predicate := p, link := none, label := l, rargs := rs, carg := some c} =>
    let pairs := joinSep (rs.map fun a => Var.format.pair a.2)  ","
    let args := joinSep (rs.map fun a => Var.format.labelOnly a.2)  " @ "
    "thf(" ++ Var.format.labelOnlyGround l ++ ",axiom," ++ "\n   " ++ Var.format.labelOnlyGround l ++ " = ( ^ [" ++ pairs ++ ",'" ++ s!"{c}" ++ "'] : (" ++ p ++ " @ " ++ args ++ ")))."
  | {predicate := p, link := none, label := l, rargs := rs, carg := none} =>
    let pairs := joinSep (rs.map fun a => Var.format.pair a.2)  ","
    let args := joinSep (rs.map fun a => Var.format.labelOnly a.2)  " @ "
    "thf(" ++ Var.format.labelOnlyGround l ++ ",axiom," ++ "\n   " ++ Var.format.labelOnlyGround l ++ " = (^ [" ++ pairs ++ "] : (" ++ p ++ " @ " ++ args ++ ")))."

def MRS.format (mrs : MRS.MRS) : String :=
 let header0 := "thf(x_decl,type,x : $tType)."
 let header1 := "thf(e_decl,type,e : $tType)."
 let headers := header0 ++ "\n" ++ header1 ++ "\n"
 match mrs with
 | {top := t, index := i, preds := ps, icons := [], hcons := hs} => 
   let rlt := List.map EP.format.type ps
   let rla := List.map EP.format.axiom ps
   headers ++ (joinSep rlt "\n") ++ "\n" ++ (joinSep rla "\n")
 | {top := t, index := i, preds := ps, icons := is, hcons := hs} => 
   let rlt := List.map EP.format.type ps
   let rla := List.map EP.format.axiom ps
   headers ++ (joinSep rlt "\n") ++ "\n" ++ (joinSep rla "\n")

end THF


