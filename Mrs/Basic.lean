import Lean

/- the data structure
   https://github.com/delph-in/docs/wiki/MrsRFC

set_option trace.Meta.synthInstance true in
#synth Repr Var
-/

open Lean

namespace MRS
open Std.Format

/- variables -/

structure Var where
 id    : Nat
 sort  : Char
 props : Array (String × String)

instance : BEq Var where
 beq a b := a.id == b.id

instance : Hashable Var where
 hash v := hash s!"{v.sort}{v.id}"

def Var.toSimple (v : Var) : Std.Format :=
  let a := v.props.toList.map fun p => f!"{p.1}: {p.2}"
  let p := f!"{v.sort}{v.id}"
  let c := [f!"{v.sort}"]
  if a.isEmpty then p
  else
   group (p ++ " " ++ (bracket "[ " (joinSep (c ++ a) .line) " ]"))

instance : Repr Var where
 reprPrec v _ := v.toSimple

instance : ToFormat Var where
 format v := v.toSimple

/- constraints: hcons and icons -/

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


/- Elementary Predications -/

structure EP where
  predicate : String
  link  : Option (Int × Int)
  label : Var
  rargs : List (String × Var)
  carg  : Option String
 deriving BEq


def priority (s : String) : Nat :=
 (priorities.find? s).getD 50
 where
  priorities := [
   ("RSTR", 1),("BODY", 2),
   ("ARG0", 3),("ARG1", 4),("ARG2", 5),("ARG3", 6),("ARG4", 7),("ARG5", 8),
   ("CARG", 100)].toAssocList'

def EP.toSimple' (e : EP) : Format :=
  nest 2 (bracket "[ " (text e.predicate ++ lnk ++ f!"LBL: {e.label}") " ]")
 where
  lnk : Format :=
   match e.link with
   | some (n,m) => f!"<{n}:{m}>"
   | none => Format.nil

#eval Var.mk 0 'h' #[("PERS","3"),("NUM","sg"),("IND","+")]
#eval Var.mk 0 'h' #[]
#eval (EP.mk "teste" (some (1,2)) (Var.mk 0 'h' #[("PERS", "3")]) [] none).toSimple'

def EP.toSimple : EP → Format
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
 reprPrec e _ := e.toSimple

instance : ToFormat EP where
 format e := e.toSimple


structure MRS where
  top : Var
  index : Var
  preds : List EP
  hcons : List Constraint
  icons : List Constraint

def MRS.toSimple : MRS → Format
 | {top := t, index := i, preds := ps, icons := [], hcons := hs} =>
   f!"[ LTOP: {t}
        INDEX: {i}
        RELS: < {Format.joinSep ps " "} >
        HCONS: < {Format.joinSep hs " "} > ]"
 | {top := t, index := i, preds := ps, icons := is, hcons := hs} =>
   f!"[ LTOP: {t}
        INDEX: {i}
        RELS: < {Format.joinSep ps " "} >
        HCONS: < {Format.joinSep hs " "} >
        ICONS: < {Format.joinSep is " "} > ]"

instance : Repr MRS where
 reprPrec m _ := m.toSimple

instance : ToFormat MRS where
 format m := m.toSimple


section MRS_Prolog
open Std.Format

def Var.toProlog (v : Var) : Std.Format :=
  f!"{v.sort}{v.id}"

def EP.toProlog (e : EP) : Std.Format :=
  nest 1 (text "rel" ++ text "("
  ++ f!"'{text e.predicate}'"
  ++ text "," ++ e.label.toProlog
  ++ text "," ++ text "["
  ++ Format.joinSep (e.rargs.map farg) ", "
  ++ text "]"
  ++ text ")"
  ++ line)
 where
  farg (a : String × Var) : Std.Format :=
    nest 1 (text "attrval"
    ++ text "("
    ++ f!"'{text a.1}'" ++ text "," ++ a.2.toProlog
    ++ text ")")

def Constraint.toProlog (c : Constraint) : Std.Format :=
  text c.rel ++ text "("
  ++ c.lhs.toProlog ++ text "," ++ c.rhs.toProlog
  ++ text ")"

def MRS.toProlog (m : MRS) : Std.Format :=
  text "psoa" ++ text "("
  ++ m.top.toProlog ++ text "," ++ m.index.toProlog
  ++ text "," ++ nest 1 (
     text "["
     ++ Format.joinSep (m.preds.map EP.toProlog) ", "
     ++ text "]")
  ++ text ","
  ++ text "hcons" ++ text "("
  ++ text "["
  ++ Format.joinSep (m.hcons.map Constraint.toProlog) ", "
  ++ text "]"
  ++ text ")"
  ++ text ")"

end MRS_Prolog

end MRS
