import Lean

/- the data structure
   https://github.com/delph-in/docs/wiki/MrsRFC
-/

open Lean

structure Var where
 id    : Nat
 sort  : Char
 props : Array (String × String)

instance : BEq Var where
 beq a b := a.id == b.id

instance : ToFormat Var where
 format
  | {id := n, sort := s, props := #[]} =>
    f!"{s}{n}"
  | {id := n, sort := s, props := ps} =>
    let a := Format.joinSep (ps.toList.map fun p => f!"{p.1}: {p.2}") " "
    f!"{s}{n} [{s} {a}]"

instance : Repr Var where
 reprPrec v _ := f!"{v}"

-- set_option trace.Meta.synthInstance true in
-- #synth Repr Var

structure Constraint where
  rel : String
  lhs : Var
  rhs : Var

instance : BEq Constraint where
  beq a b := a.lhs == b.lhs && a.rhs == b.rhs

instance : ToFormat Constraint where
 format
  | {rel := r, lhs := l, rhs := h} => f!"{l} {r} {h}"

instance : Repr Constraint where
 reprPrec c _ := f!"{c}"


structure EP where
  predicate : String
  link  : Option (Int × Int)
  label : Var
  rargs : List (String × Var)
  carg  : Option String
 deriving BEq

instance : ToFormat EP where
 format
  | {predicate := p, link := some (n,m), label := l, rargs := rs, carg := some c} =>
    let as := Format.joinSep (rs.map fun a => f!"{a.1}: {a.2}") " "
    f!"[ {p}<{n}:{m}> LBL: {l} {as} CARG: {c} ]"
  | {predicate := p, link := some (n,m), label := l, rargs := rs, carg := none} =>
    let as := Format.joinSep (rs.map fun a => f!"{a.1}: {a.2}") " "
    f!"[ {p}<{n}:{m}> LBL: {l} {as} ]"
  | {predicate := p, link := none, label := l, rargs := rs, carg := some c} =>
    let as := Format.joinSep (rs.map fun a => f!"{a.1}: {a.2}") " "
    f!"[ {p} LBL: {l} {as} CARG: {c} ]"
  | {predicate := p, link := none, label := l, rargs := rs, carg := none} =>
    let as := Format.joinSep (rs.map fun a => f!"{a.1}: {a.2}") " "
    f!"[ {p} LBL: {l} {as} ]"

instance : Repr EP where
 reprPrec e _ := f!"{e}"


structure MRS where
  top : Var
  index : Var
  preds : List EP
  hcons : List Constraint
  icons : List Constraint

instance : ToFormat MRS where
 format
 | {top := t, index := i, preds := ps, icons := [], hcons := hs} =>
   f!"[ LTOP: {t}
        INDEX: {i}
        RELS: < {Format.joinSep (ps.map fun a => format a) " "} >
        HCONS: < {Format.joinSep (hs.map fun a => format a) " "} > ]"
 | {top := t, index := i, preds := ps, icons := is, hcons := hs} =>
   f!"[ LTOP: {t}
        INDEX: {i}
        RELS: < {Format.joinSep (ps.map fun a => format a) " "} >
        HCONS: < {Format.joinSep (hs.map fun a => format a) " "} >
        ICONS: < {Format.joinSep (is.map fun a => format a) " "} > ]"

instance : Repr MRS where
 reprPrec m _ := f!"{m}"
