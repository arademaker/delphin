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
     | Except.ok ps => return some (solve_mrs a (ps.get! 0))
     | Except.error _ => return none

#eval main₃ "A man is walking."
