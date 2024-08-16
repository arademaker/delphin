import Mrs.Basic
import Ace

namespace Utool

/- In this namespace we have the code to parse the Utool output
   and produce the Utool Input from a MRS -/

open Lean Parsec
open MRS

structure Plug where
  lo : MRS.Var
  hi : MRS.Var
 deriving Repr

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
  return { lo := vlo, hi := vhi}

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

def run_utool (txt : String) := do
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


#eval run_utool prolog_example


def mrs_to_prolog (m : MRS) : String := sorry


def solve_mrs (m : MRS) : MRS := sorry


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
