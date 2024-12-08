import Mrs.Basic
import Mrs.Hof
import Lean.Data.RBMap

namespace PWL.Predicates

open Lean (RBMap)
open MRS (EP Var)
open MM

structure WorkItem where
  pred: EP
  depth: Nat
  deriving Inhabited

structure CollectState where
  visited: RBMap Var Unit compare
  result: List EP
  queue: List WorkItem
  depth: Nat
  deriving Inhabited

partial def collectPredsLoop (hm: Multimap Var EP) (maxDepth: Nat) (state: CollectState) : List EP :=
  match state.queue with 
  | [] => state.result
  | item::rest =>
    let newDepth := state.depth + 1
    if newDepth > maxDepth then
      dbg_trace s!"Max depth {maxDepth} reached, remaining queue size: {rest.length}"
      state.result ++ (rest.map fun item => item.pred) 
    else
      let newResult := if state.visited.contains item.pred.label then
        state.result
      else
        item.pred :: state.result
      
      let newVisited := state.visited.insert item.pred.label ()
      
      let newPreds := item.pred.rargs.foldl (fun acc (_, var) =>
        if var.sort == 'h' then
          match hm.find? var with
          | some morePreds => 
            dbg_trace s!"Found {morePreds.length} predicates at handle {var.sort}{var.id}"
            acc ++ (morePreds.map fun p => WorkItem.mk p newDepth)
          | none => 
            dbg_trace s!"No predicates found at handle {var.sort}{var.id}"
            acc
        else acc) []
        
      collectPredsLoop hm maxDepth {
        visited := newVisited,
        result := newResult,
        queue := rest ++ newPreds,
        depth := newDepth
      }

def collectAllPreds (preds: List EP) (hm: Multimap Var EP) : List EP :=
  let maxDepth := 10
  let initialState := {
    visited := RBMap.empty,
    result := [],
    queue := preds.map fun p => WorkItem.mk p 0,
    depth := 0
  }
  let result := collectPredsLoop hm maxDepth initialState
  result.eraseDups

end PWL.Predicates

export PWL.Predicates (collectAllPreds)
