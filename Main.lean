import Mrs
import Ace

open MRS Utool

-- set_option pp.oneline true
-- set_option pp.proofs true
-- set_option trace.profiler true in

def report0 (r : String × Except String MRS) :=
 match r.2 with
 | Except.ok b => IO.println $ List.length b.preds
 | Except.error e => IO.println (r.1, e)

def report1 (r : String × Except String MRS) :=
 match r.2 with
 | Except.ok b => IO.println $
   (b.preds.map (λ p : EP => p.predicate)).filter (λ s : String => s.endsWith "q")
 | Except.error e => IO.println e


def main₁ := do
  let as ← run_ace "Every boy does not love a book."
  return as.head?

def main₂ := do
  let as ← run_ace "Every boy loves a book."
  return MRS.toProlog <$> as.head?  -- why .dot notation doesn't work here?

def main₃ := do
  let as ← run_ace "Every boy does not love a book."
  match as.head? with
  | none => return none
  | some a =>
     let r ← Utool.run_utool (Std.Format.pretty a.toProlog)
     match r with
     | Except.ok ps => return some (solve_mrs a (ps.get! 0))
     | Except.error _ => return none

#eval main₃
