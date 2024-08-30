import Mrs.Basic
import Ace
import Lean.Data.HashMap

namespace Utool

/- In this namespace we have the code to parse the Utool output
   and produce the Utool Input from a MRS -/


open Lean (Parsec)
open Lean.Parsec (pchar pstring satisfy many1 asciiLetter digit many)
open MRS (Var EP Constraint MRS parseSpace)
open Lean (HashMap)

structure Plug where
  hol : Var
  lbl : Var
 deriving Repr

instance : ToString Plug where
  toString p := s!"Plug(hol : {p.hol.id}, lbl: {p.lbl.id})"

def joinSep (l : List String) (sep : String) : String := l.foldr (fun s r => (if r == "" then s else r ++ sep ++ s)) ""

def Var.format (var : Var) : String :=
  match var with
  | {id := n, sort := s, props := #[]} =>
       s!"{s}{n}"
  | {id := n, sort := s, props := ps} =>
     -- let a := Format.join $ ps.toList.map fun p => s!"{p.1}: {p.2}"
     s!"{s}{n}"
     -- s!"{s}{n} [{s} {a}]" -- drop the properties

def EP.format (ep : MRS.EP) : String :=
  match ep with
  | {predicate := p, link := some (n,m), label := l, rargs := rs, carg := some c} =>
    let pairs := joinSep (rs.map fun a => "attrval('" ++ a.1 ++ "'," ++ (Var.format a.2) ++ ")")  ","
    "rel(" ++ "'" ++ p ++ "'" ++ "," ++ Var.format l ++ ",[" ++ pairs ++ "," ++ "attrval('CARG','" ++ s!"{c}" ++ "')])"
  | {predicate := p, link := some (n,m), label := l, rargs := rs, carg := none} =>
    let pairs := joinSep (rs.map fun a => "attrval('" ++ a.1 ++ "'," ++ (Var.format a.2) ++ ")") ","
    "rel(" ++ "'" ++ p ++ "'" ++ "," ++ Var.format l ++ ",[" ++ pairs ++ "])"
  | {predicate := p, link := none, label := l, rargs := rs, carg := some c} =>
    let pairs := joinSep (rs.map fun a => "attrval('" ++ a.1 ++ "'," ++ (Var.format a.2) ++ ")") ","
    "rel(" ++ "'" ++ p ++ "'" ++ "," ++ Var.format l ++ ",[" ++ pairs ++ "," ++ "attrval('CARG','" ++ s!"{c}" ++ "')])"
  | {predicate := p, link := none, label := l, rargs := rs, carg := none} =>
    let pairs := joinSep (rs.map fun a => "attrval('" ++ a.1 ++ "\"," ++ (Var.format a.2) ++ ")") ","
    "rel(" ++ "'" ++ p ++ "'" ++ "," ++ Var.format l ++ ",[" ++ pairs ++ "])"

def Constraint.format (cons : MRS.Constraint) : String :=
  match cons with
  | {rel := r, lhs := l, rhs := h} => r ++ "(" ++ Var.format l ++ "," ++ Var.format h ++ ")"

def MRS.format (mrs : MRS.MRS) : String :=
 match mrs with
 | {top := t, index := i, preds := ps, icons := [], hcons := hs} =>
   let rl : List String := List.map (fun p => EP.format p) ps
   let relStr : String := joinSep rl ","
   let hl : List String := List.map (fun h => Constraint.format h) hs
   let hStr : String := joinSep hl ","
   "psoa(" ++ (Var.format t) ++ "," ++ (Var.format i) ++ "," ++ "[" ++ relStr ++ "],hcons([" ++ hStr ++ "]))"
 | {top := t, index := i, preds := ps, icons := is, hcons := hs} =>
   let rl := List.map (fun p => EP.format p) ps
   let relStr : String := joinSep rl ","
   let hl : List String := List.map (fun p => Constraint.format p) hs
   let hStr : String := joinSep hl ","
   let il : List String := List.map (fun i => Constraint.format i) is
   let iStr : String := joinSep il ","
   "psoa(" ++ (Var.format t) ++ "," ++ (Var.format i) ++ "," ++ "[" ++ relStr ++ "],hcons([" ++ hStr ++ "],icons([" ++ iStr ++ "]))"

def parseVarName : Parsec (Char × Nat) := do
  let p ← asciiLetter
  let s ← many1 digit
  return (p, s.asString.toNat!)

def parsePlug : Parsec Plug := do
  let _ ← pstring "plug" <* pchar '('
  let lo ← parseVarName <* parseSpace
  let hi ← parseVarName
  let _ ← pchar ')'
  let vlo := {id := lo.2, sort := lo.1, props := #[] : Var}
  let vhi := {id := hi.2, sort := hi.1, props := #[] : Var}
  return {hol := vlo, lbl := vhi}

def parseSolution : Parsec (Array Plug) := do
  let _ ← pchar '['
  let ps ← many $ (parsePlug <* parseSpace)
  let _ ← pchar ']'
  return ps

def parseOutput : Parsec (Array (Array Plug)) := do
  let _ ← pchar '%' *> many (satisfy $ fun c => c ≠ '\n')
  let _ ← parseSpace *> pchar '['
  let ps ← many $ (parseSolution <* parseSpace)
  let _ ← pchar ']'
  return ps

def run_utool (txt : String) : IO (Except String $ Array $ Array Plug) := do
  let ret ← cmd_with_stdin {cmd := "java", args := #["-jar","utool-3.4.jar", "solve", "-I", "mrs-prolog", "-O", "plugging-oz", "-"], cwd := "."} txt
  let  p := Parsec.run parseOutput ret.stdout
  return p
def prolog_example : String := "psoa(h0,e2,
  [rel('unknown',h1,
       [attrval('ARG',x4),
        attrval('ARG0',e2)]),
   rel('_a_q',h5,
       [attrval('ARG0',x4),
        attrval('RSTR',h6),
        attrval('BODY',h7)]),
   rel('_dog_n_1',h8,
       [attrval('ARG0',x4)]),
   rel('generic_entity',h9,
       [attrval('ARG0',x10)]),
   rel('_that_q_dem',h11,
       [attrval('ARG0',x10),
        attrval('RSTR',h12),
        attrval('BODY',h13)]),
   rel('_happy_a_with',h8,
       [attrval('ARG0',e14),
        attrval('ARG1',x10),
        attrval('ARG2',h15)]),
   rel('_bark_v_1',h16,
       [attrval('ARG0',e17),
        attrval('ARG1',x4)])],
  hcons([qeq(h0,h1),qeq(h6,h8),qeq(h12,h9),qeq(h15,h16)]))"


-- #eval run_utool prolog_example

def subst (var : Var) (plugs : Array Plug) : Var := 
  let result := plugs.find? (fun plug => var.id == plug.hol.id)
  match result with
  | some value => value.lbl
  | none => var

def rewrite (plugs : Array Plug) (a : String × Var) : (String × Var) :=
  (a.1,subst a.2 plugs)

def updateEP (plugs : Array Plug) (ep : MRS.EP) : MRS.EP :=
  match ep with 
  | {predicate := p, link := lnk, label := l, rargs := rs, carg := c} => MRS.EP.mk p lnk l (rs.map (rewrite plugs)) c

def updateEPs (mrs : MRS.MRS) (plugs : Array Plug) : List MRS.EP :=
  mrs.preds.map (updateEP plugs)

def expandPlugs (mrs : MRS.MRS) (plugsSet : Array (Array Plug)) : Array (List MRS.EP) :=
  plugsSet.map (fun plugs => (updateEPs mrs plugs))

def insertDepsForEP (hm : HashMap Var Var) (ep : EP) : HashMap Var Var :=
  ep.rargs.foldl (fun hmacc pair => hmacc.insert pair.2 ep.label) hm

def collectDepsForEPs (preds : List EP) : HashMap Var Var :=
  preds.foldl (fun hmacc ep => insertDepsForEP hmacc ep) HashMap.empty

def findRoot (scopes : (HashMap Var Var)) (start : Var) : Var :=
  let rec findRoot_aux (fuel : Nat) (current : Var) : Var :=
    if fuel = 0 then
      current  -- Base case: return current if we've exhausted our fuel
    else
      match scopes.find? current with
      | some value => findRoot_aux (fuel - 1) value
      | none => current
  findRoot_aux (scopes.size + 1) start

def createNewMRS (mrsIn : MRS) (newPreds : List EP) : MRS :=
  let scopes := collectDepsForEPs newPreds
  let newScopes := 
    match mrsIn.hcons.find? (fun item => item.lhs == mrsIn.top) with
    | some x => scopes.insert x.lhs x.rhs
    | none => panic! "top node not found in hcons"
  MRS.mk (findRoot newScopes mrsIn.top) mrsIn.index newPreds [] []

def createNewMRS_many (mrsIn : MRS) (alep : (Array (List EP))) : (Array MRS) :=
  alep.map (fun preds => createNewMRS mrsIn preds)

def solveIt (mrs : MRS) : IO (Except String (Array MRS)) := do
  let ret ← run_utool $ MRS.format mrs
  match ret with
  | Except.ok plugsSet => return (Except.ok (createNewMRS_many mrs (expandPlugs mrs plugsSet)))
  | Except.error e => return (Except.error e)

end Utool


/- TODO

set_option profiler true
set_option trace.profiler.output.pp true
set_option profiler.threshold 2

1. MRS ~> Prolog String
2. Parse plugins ~> maps of Var to Var
3. MRS plugs ~> MRS resolved

def is_quantifier (p : EP) : Bool :=
 match p.rargs.find? (λ (k, _) => k == "BODY") with
 | none => false
 | some _ => true

def get_arg_value (nm : String) (rel : EP) : Option Var :=
 match rel.rargs.find? (λ (k,_) => k == nm) with
 | none => none
 | some (_, v) => some v

def collect_vars (p : EP) : List Var :=
  p.rargs.map (λ fvp => fvp.2)

def find_unbound_vars (m : MRS) : List Var :=
  let q_rels := m.preds.filter is_quantifier
  let q_vars := q_rels.map (get_arg_value "ARG0")
  let as := m.preds.foldl (λ vs e => vs.append $ collect_vars e) []
  let is_q (v₁ : Var) : Option Var → Bool
  | none => false
  | some v₂ => v₁ == v₂
  as.filter (λ v => not $ q_vars.any (is_q v))

def update_predicate (tb : AssocList Var Var) (p : EP) : EP :=
  { p with rargs := p.rargs.map (λ fvp =>
    match tb.find? fvp.2 with
    | none => fvp
    | some n => (fvp.1, n)) }

def equate_qeqs (m : MRS) : MRS :=
  let f1 : Constraint → Prop        := (λ r => r.rel == "qeq")
  let f2 : Constraint → (Var × Var) := (λ r => (r.lhs, r.rhs))
  let tb := ((m.hcons.filter f1).map f2).toAssocList'
  let f3 := update_predicate tb
  { m with preds := m.preds.map f3 }

inductive Tree where
 | node : List EP → List Tree → Tree

-/
