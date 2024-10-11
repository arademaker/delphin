import Mrs
import Ace

open MRS Utool

-- set_option pp.oneline true
-- set_option pp.proofs true
-- set_option trace.profiler true in

def main₁ (s : String) := do
  let as ← run_ace s
  return as.head?

def main₂ (s : String) := do
  let as ← run_ace s
  return MRS.toProlog <$> as.head?

def main₃ (s : String) := do
  let as ← run_ace s
  match as.head? with
  | none => return none
  | some a =>
     let r ← Utool.run_utool (Std.Format.pretty a.toProlog)
     match r with
     | Except.ok ps => return some (a, (ps.get! 0))
     | Except.error _ => return none

#eval main₁ "The aunt Agatha"


/-
 LTOP: h0
 INDEX: e2 [e SF: prop]
 RELS: < [ unknown<0:15> LBL: h1 ARG: x4 [x PERS: 3 NUM: sg IND: +] ARG0: e2 ] [ _the_q<0:3> LBL: h5 BODY: h7 RSTR: h6 ARG0: x4 ] [ compound<4:15> LBL: h8 ARG2: x10 [x IND: +] ARG1: x4 ARG0: e9 [e SF: prop TENSE: untensed MOOD: indicative PROG: - PERF: -] ] [ udef_q<4:8> LBL: h11 BODY: h13 RSTR: h12 ARG0: x10 ] [ _aunt_n_of<4:8> LBL: h14 ARG1: i15 ARG0: x10 ] [ named<9:15> LBL: h8 ARG0: x4 CARG: "Agatha" ] >
 HCONS: < h0 qeq h1 h6 qeq h8 h12 qeq h14 > ]

 Does it make sense?

 compound e x y ∧ aunt_of x i ∧ named y S ~>
  aunt_of y i ∧ named y S
-/
