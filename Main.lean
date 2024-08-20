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

def report2 (r : String × Except String MRS) : IO (Except String (Array Unit)) := do
 match r.2 with
 | Except.ok b =>
    let ret <- Utool.solveIt b
    match ret with
    | Except.ok expansions => 
      let retArray <- expansions.mapM (fun epList => IO.println $ f!"{epList}")
      return (Except.ok retArray)
    | Except.error e2 => return (Except.error e2)
 | Except.error e => return (Except.error e)

def main : IO Unit := do
  let ls ← IO.FS.lines "ws201.txt"
  let rs := ls.toList.map (λ l => (l, Lean.Parsec.run parseMRS l))
  let _ ← rs.mapM report2
  return ()


-- #eval main
