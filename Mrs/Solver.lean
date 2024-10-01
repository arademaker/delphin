import Mrs.Basic
import Ace

namespace Utool

/- In this namespace we have the code to parse the Utool output
   and produce the Utool Input from a MRS -/

open Lean (Parsec)
open Lean.Parsec (pchar pstring satisfy many1 asciiLetter digit many)
open MRS (Var parseSpace MRS EP)

structure Plug where
  hol : Var
  lbl : Var
 deriving Repr

instance : ToString Plug where
  toString
  | s => s!"({s.hol.toProlog},{s.lbl.toProlog})"

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
  let p := Parsec.run parseOutput ret.stdout
  return p

/- in pydelphin, the top need to be one among the labels of the EPs, otherwise,
   none is returned. We don't have provision for a none in the top field of an
   MRS. -/
def solve_mrs (m : MRS) (ps : Array Plug) : MRS :=
  let map := Lean.HashMap.ofList $ ps.map (λ p => (p.hol, p.lbl)) |>.toList
  let top' := match m.hcons.find? (λ h => h.lhs == m.top) with
              | some h => h.rhs
              | none => m.top
  { m with preds := m.preds.map (λ p : EP =>
      { p with rargs :=
         p.rargs.map (λ v =>
          match map.find? v.2 with
          | some v' => (v.1, v')
          | none => (v.1, v.2)) }), hcons := [], top := top' }

end Utool
