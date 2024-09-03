import Mrs
import Ace

open MRS
open THF

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

def reportMRS (mrs : MRS) : IO (Except String (Array Unit)) := do
  let ret <- Utool.solveIt mrs
  match ret with
  | Except.ok expansions => 
    let retArray <- expansions.mapM (fun mrs => IO.println $ f!"{mrs}")
    return (Except.ok retArray)
  | Except.error e2 => return (Except.error e2)

def reportTHF (mrs : MRS) : IO (Except String (Array Unit)) := do
  let ret <- Utool.solveIt mrs
  match ret with
  | Except.ok expansions => 
    let retArray <- expansions.mapM (fun mrs => IO.println $ ((THF.MRS.format mrs) ++ "\n"))
    return (Except.ok retArray)
  | Except.error e2 => return (Except.error e2)

def report2 (r : String × Except String MRS) : IO (Except String (Array Unit)) := do
  match r.2 with
  | Except.ok b => reportTHF b
  | Except.error e => return (Except.error e)

def main : IO Unit := do
 let as <- run_ace "Every boy loves a book."
 for mrs in as do
   let val <- reportTHF mrs
  
-- #eval main
