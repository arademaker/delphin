import Init.System.IO
import Std
import Mrs
import Ace

def test1 := do
  let as ← run_ace "Every happy dog barks."
  return as.head?

-- set_option pp.oneline true
-- set_option pp.proofs true
-- set_option trace.profiler true in

#eval test1


def report (r : String × Except String MRS) :=
 match r.2 with
 | Except.ok b => IO.println $ List.length b.preds
 | Except.error e => IO.println (r.1, e)

def main : IO Unit := do
  let ls ← IO.FS.lines "ws201.txt"
  let rs := ls.toList.map (λ l => (l, Lean.Parsec.run parseMRS l))
  let _ ← rs.mapM report
  return ()

-- #eval main
