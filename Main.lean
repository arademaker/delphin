import Mrs
import Ace

open MRS
open THF


-- set_option pp.oneline true
-- set_option pp.proofs true
-- set_option trace.profiler true in

def solveAndFormat (sentenceNumber : Nat) (mrs : MRS) : IO (String × List String × List Var) := do
  let solveRet <- Utool.solveIt mrs
  match solveRet with
  | Except.ok sols => 
    match sols.get? 0 with
    | some sol => return THF.MRS.format sentenceNumber sol
    | none => unreachable!
  | Except.error _ => unreachable!

def xform (sentenceNumber : Nat) (str : String) : IO (Nat × (String × List String × List Var)) := do
  let (mrsList : List MRS) <- run_ace str
  let ret <- match mrsList.head? with
      | some firstMrs => (solveAndFormat sentenceNumber firstMrs)
      | none => unreachable!
  return (sentenceNumber,ret)

def mapWithIndexM [Monad m] (xs : List α) (f : Nat → α → m β) : m (List β) := do
  let rec loop : List α → Nat → m (List β)
    | [],    _ => pure []
    | x::xs, i => do
      let y ← f i x
      let ys ← loop xs (i+1)
      pure (y::ys)
  loop xs 0

instance : Ord (Nat × Var) where
  compare := fun a b =>
    match compare a.1 b.1 with
    | .eq => compare a.2 b.2
    | ord => ord

def addSentenceNumber (sentenceNumber : Nat) (l : List Var) : List (Nat × Var) :=
  l.map (fun var => (sentenceNumber,var))

def enumerateUniquePairs [Ord α] (list : List α) : List (α × α) :=
  let rec aux : List α → List (α × α)
    | [] => []
    | x :: xs =>
      (xs.filter (λ y => compare x y == Ordering.lt)).map (λ y => (x, y)) ++ (aux xs)
  aux list

def main : IO Unit := do
 let sentencesText := ["Someone who lives in Dreadbury Mansion killed Aunt Agatha.", -- 0
                      "Agatha, the butler, and Charles live in Dreadbury Mansion, and are the only people who live therein.", -- 1
                      "A killer always hates his victim, and is never richer than his victim.", -- 2
                      "Charles hates no one that Aunt Agatha hates.", -- 3
                      "Agatha hates everyone except the butler.", -- 4
                      "The butler hates everyone not richer than Aunt Agatha.", -- 5
                      "The butler hates everyone Aunt Agatha hates.", -- 6
                      "No one hates everyone.", -- 7
                      "Agatha is not the butler.", -- 8
                      "Therefore : Agatha killed herself." -- 9
                   ] 

 let (sentences : List (Nat × (String × (List String) × (List Var)))) <- mapWithIndexM sentencesText xform
 let header0 := "thf(x_decl,type,x : $tType)."
 let header1 := "thf(e_decl,type,e : $i)."
 let header2 := "thf(name_decl,type,name : $i)."
 let (eSet : List (Nat × Var)) := sentences.foldl (fun acc tup => 
                                                    let (trip : (String × (List String) × (List Var))) := tup.snd
                                                    acc ++ (addSentenceNumber tup.fst trip.snd.snd)) []
 let (iSet : List String) := sentences.foldl (fun acc tup => 
                                               let (trip : (String × (List String) × (List Var))) := tup.snd
                                               let (strList : (List String)) := trip.snd.fst
                                               acc ++ strList) []
 let itypes := (insertionSort iSet).eraseDups.reverse.map (fun str =>
                                                            let lab := formatId str
                                                            "thf(" ++ lab ++ "_decl,type," ++ lab ++ ": " ++ "name).")
 let (epairs : List ((Nat × Var) × (Nat × Var)))  := (enumerateUniquePairs $ THF.insertionSort eSet).reverse
 let uids := (insertionSort iSet).eraseDups
 let ipairs := enumerateUniquePairs $ uids
 let ecompares := epairs.map (fun pair => ("(" ++ (Var.format.labelOnlyGround pair.1.1 pair.1.2) ++ " != " ++ (Var.format.labelOnlyGround pair.2.1 pair.2.2) ++ ")" ))
 let icompares := ipairs.map (fun pair => ("(" ++ (formatId pair.1) ++ " != " ++ (formatId pair.2) ++ ")"))
 let eaxioms := "thf(distinct_events, axiom,\n   (" ++ (joinSep ecompares " & ") ++ ")).\n"
 let iaxioms := "thf(distinct_ids, axiom,\n   (" ++ (joinSep icompares " & ") ++ ")).\n"
 
 let headers := header0 ++ "\n" ++ header1 ++ "\n" ++ header2 ++ "\n\n" ++ (joinSep itypes "\n") ++ "\n\n" ++ THF.libraryRoutines ++ "\n" ++ iaxioms ++ "\n"
 IO.FS.writeFile "thf-outputs/sentences.p" (((sentences.foldl (fun acc pair => acc ++ pair.snd.fst ++ "\n\n") headers)) ++ eaxioms)
 return ()
  
-- #eval main
