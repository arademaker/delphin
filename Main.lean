import Mrs
import Ace

open MRS

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

def main := do
  let as ← run_ace "Every boy loves a book."
  match as.head? with
  | none => println! "No parse"
  | some a => println! a.toProlog

#eval main
