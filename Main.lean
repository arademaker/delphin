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
 -- let mrsList : List MRS <- run_ace "Someone who lives in Dreadbury Mansion killed Aunt Agatha."
 -- let mrsList : List MRS <- run_ace "Agatha, the butler, and Charles live in Dreadbury Mansion, and are the only people who live therein."
 -- let mrsList : List MRS <- run_ace "A killer always hates his victim, and is never richer than his victim." 
 -- let mrsList : List MRS <- run_ace "Charles hates no one that Aunt Agatha hates."
 -- let mrsList : List MRS <- run_ace "Agatha hates everyone except the butler." 
 -- let mrsList : List MRS <- run_ace "The butler hates everyone not richer than Aunt Agatha."
 -- let mrsList : List MRS <- run_ace "The butler hates everyone Aunt Agatha hates." 
 -- let mrsList : List MRS <- run_ace "No one hates everyone."
 -- let mrsList : List MRS <- run_ace "Agatha is not the butler."
 let mrsList : List MRS <- run_ace "Therefore : Agatha killed herself."
 _ <- match mrsList.head? with
      | some firstMrs => 
          let solveRet <- Utool.solveIt firstMrs
          match solveRet with
          | Except.ok sols => 
            match sols.get? 0 with
            | some sol => IO.println $ THF.MRS.format $ sol
            | none => unreachable!
          | Except.error _ => unreachable!
      | none => unreachable!

  
-- #eval main
