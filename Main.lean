import Mrs
import Ace

open MRS
open THF


-- set_option pp.oneline true
-- set_option pp.proofs true
-- set_option trace.profiler true in

def solveAndFormat (mrs : MRS) : IO String := do
  let solveRet <- Utool.solveIt mrs
  match solveRet with
  | Except.ok sols => 
    match sols.get? 0 with
    | some sol => return THF.MRS.format $ sol
    | none => unreachable!
  | Except.error e2 => unreachable!

def xform (i : Nat) (str : String) : IO Unit := do
  let (mrsList : List MRS) <- run_ace str
  let ret <- match mrsList.head? with
      | some firstMrs => solveAndFormat firstMrs
      | none => unreachable!
  let moduleName := "Agatha_Sentence_" ++ toString i
  IO.FS.writeFile ("thf-outputs/agatha_sentence_" ++ toString i ++ ".p") ("module " ++ moduleName ++ "\n" ++ ret ++ "\n" ++ "end " ++ moduleName ++ "\n")

def mapWithIndexM [Monad m] (xs : List α) (f : Nat → α → m β) : m (List β) := do
  let rec loop : List α → Nat → m (List β)
    | [],    _ => pure []
    | x::xs, i => do
      let y ← f i x
      let ys ← loop xs (i+1)
      pure (y::ys)
  loop xs 0

def main : IO Unit := do
 let sentences := ["Someone who lives in Dreadbury Mansion killed Aunt Agatha.",
                   "Agatha, the butler, and Charles live in Dreadbury Mansion, and are the only people who live therein.",
                   "A killer always hates his victim, and is never richer than his victim.",
                   "Charles hates no one that Aunt Agatha hates.",
                   "Agatha hates everyone except the butler.",
                   "The butler hates everyone not richer than Aunt Agatha.",
                   "The butler hates everyone Aunt Agatha hates.",
                   "No one hates everyone.",
                   "Agatha is not the butler.",
                   "Therefore : Agatha killed herself."]

 _ <- mapWithIndexM sentences xform
 return ()
  
-- #eval main
