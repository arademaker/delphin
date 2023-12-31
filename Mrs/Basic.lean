
/- the data structure
   https://github.com/delph-in/docs/wiki/MrsRFC
-/

open Std

structure Var where
 name  : String
 sort  : Option String
 props : Array (String × String)

instance : ToFormat Var where
 format
  | {name := n, sort := some _, props := #[]} =>
    f!"{n}"
  | {name := n, sort := some s, props := ps} =>
    let a := Format.joinSep (ps.toList.map fun p => f!"{p.1} {p.2}") " "
    f!"{n} [{s} {a}]"
  | {name := n, sort := none, props := #[]} =>
    f!"{n}"
  | {name := n, sort := none, props := ps} =>
    let a := Format.joinSep (ps.toList.map fun p => f!"{p.1} {p.2}") " "
    f!"{n} [? {a}]"

instance : Repr Var where
 reprPrec v _ := f!"{v}"

structure Constraint where
  rel : String
  lhs : Var
  rhs : Var

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
deriving Repr

instance : ToFormat EP where
 format
  | {predicate := p, link := some (n,m), label := l, rargs := rs, carg := some c} =>
    let as := Format.joinSep (rs.map fun a => f!"{a.1}: {a.2}") " "
    f!"[ {p}<{n},{m}> LBL: {l} {as} CARG: {c} ]"
  | {predicate := p, link := some (n,m), label := l, rargs := rs, carg := none} =>
    let as := Format.joinSep (rs.map fun a => f!"{a.1}: {a.2}") " "
    f!"[ {p}<{n},{m}> LBL: {l} {as} ]"
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
deriving Repr

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
