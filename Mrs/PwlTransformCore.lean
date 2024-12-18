import Mrs.Basic
import Mrs.PwlTypes 
import Mrs.PwlTransformScoping
import Mrs.PwlTransformShared 
import Mrs.PwlTransformMinScoping
import Mrs.PwlTransformSerialize
import Mrs.Hof
import Util.InsertionSort

namespace PWL.Transform

open MRS (Var EP Constraint MRS)
open MM (Multimap)
open Lean (Format)
open InsertionSort
open PWL.Transform.Scoping (processPredicates processEP EliminatedVars isVarEliminated collectEliminatedVars)
open PWL.Transform.MinScoping (minimizeScoping)
open PWL.Transform.Serialize (formatAsPWL)

structure CompoundMatch where
  compound : EP
  properQ1 : EP
  properQ2 : EP
  named1   : EP
  named2   : EP
  deriving Repr, Inhabited, BEq

instance : ToString CompoundMatch where
  toString m := s!"CompoundMatch(compound.label: {m.compound.label}, properQ1.label: {m.properQ1.label}, compound: {m.compound})"

instance : ToString (List CompoundMatch) where
  toString xs := String.intercalate ", " (xs.map toString)

def shouldRemove (p : EP) (pat : CompoundMatch) : Bool :=
  p == pat.compound || p == pat.properQ1 || p == pat.properQ2 || 
  p == pat.named1 || p == pat.named2

def getCompoundPattern (preds : List EP) (c : EP) (handleMap : Multimap Var EP) : Option CompoundMatch :=
  if c.predicate != "compound" && c.predicate != "_compound" then none else
  (match c.rargs with
  | (_, x1) :: (_, x2) :: _ =>
    dbg_trace ("Found compound args: " ++ toString x1 ++ ", " ++ toString x2)
    if x1.sort == 'x' && x2.sort == 'x' then
      let handlePreds := handleMap.keys.foldl (fun acc k =>
        match handleMap.find? k with
        | some preds => acc ++ preds
        | none => acc) []

      let preds1 := handlePreds.filter fun p => p.rargs.any fun (_, v2) => v2 == x1
      let preds2 := handlePreds.filter fun p => p.rargs.any fun (_, v2) => v2 == x2

      let properQ1 := preds1.find? fun p => p.predicate.endsWith "_q"
      let properQ2 := preds2.find? fun p => p.predicate.endsWith "_q"
      let named1 := preds1.find? fun p => p.predicate == "named"
      let named2 := preds2.find? fun p => p.predicate == "named"

      match properQ1, properQ2, named1, named2 with
      | some q1, some q2, some n1, some n2 =>
        match n1.carg, n2.carg with
        | some s1, some s2 => some ⟨c, q1, q2, n1, n2⟩
        | _, _ => none
      | _, _, _, _ => none
    else none
  | _ => none)

def makeTemp (parent : Var) (ev : EliminatedVars) (pat : CompoundMatch) : Option EP :=
  dbg_trace ("Making temp_compound_name with: " ++
             "pat.properQ1=" ++ toString pat.properQ1 ++
             " pat.properQ2=" ++ toString pat.properQ2 ++
             " pat.named1=" ++ toString pat.named1 ++
             " pat.named2=" ++ toString pat.named2)

  (match pat.named1.carg, pat.named2.carg with
  | some s1, some s2 =>
    let x1 := pat.properQ1.rargs.find? (fun arg => arg.1 == "ARG0")
    let x2 := pat.properQ2.rargs.find? (fun arg => arg.1 == "ARG0")
    let b1 := pat.properQ1.rargs.find? (fun arg => arg.1 == "BODY")
    let b2 := pat.properQ2.rargs.find? (fun arg => arg.1 == "BODY") 
    
    dbg_trace ("properQ1 args: " ++ toString pat.properQ1.rargs)
    dbg_trace ("properQ2 args: " ++ toString pat.properQ2.rargs)
    dbg_trace ("Found ARG0s: " ++ toString x1 ++ ", " ++ toString x2)
    dbg_trace ("Found BODYs: " ++ toString b1 ++ ", " ++ toString b2)
    
    match x1, x2, b1, b2 with
    | some (_, var1), some (_, var2), some (_, body1), some (_, body2) =>
      some (EP.mk "temp_compound_name" none parent
        [("X1", var1), ("X2", var2), ("A", body1), ("B", body2)]
        (some ("\"" ++ removeExtraQuotes s1 ++ " " ++ removeExtraQuotes s2 ++ "\"")))
    | _, _, _, _ => none
  | _, _ => none)

def phase1 (parent : Var) (preds : List EP) (hm : Multimap Var EP) : (List EP × EliminatedVars) :=
  let compounds := preds.filter fun p => 
    p.predicate == "compound" || p.predicate == "_compound"
  dbg_trace ("Found compounds: " ++ toString compounds)
  
  let patterns := compounds.filterMap (fun c => getCompoundPattern preds c hm)
  dbg_trace ("Found patterns: " ++ toString patterns)
  dbg_trace ("Found pattern handles: " ++ toString (patterns.map (fun p => p.compound.label)))

  -- First create temps with no variables eliminated  
  let temps := patterns.filterMap (makeTemp parent EliminatedVars.empty)
  dbg_trace ("Created temp compounds: " ++ toString temps)

  -- Then track variables eliminated by successful transformations
  let eliminatedVars := collectEliminatedVars $
    patterns.filter (fun p => temps.any (fun t => t.predicate == "temp_compound_name"))
    |>.map (fun p => p.compound)
  
  let remaining := preds.filter fun p =>
    not (patterns.any (shouldRemove p))
  
  dbg_trace ("Remaining predicates: " ++ toString remaining)
  
  (remaining ++ temps, eliminatedVars)

def phase2 (parent : Var) (handle : Var) (preds : List EP) (ev : EliminatedVars) (hm : Multimap Var EP) : Option Formula := 
  match hm.find? handle with 
  | none => unreachable!
  | some rootPreds => 
    dbg_trace ("phase2 starting at handle " ++ toString handle)
    dbg_trace ("  root predicates: " ++ toString rootPreds)
    -- First collect all substitutions from temp_compound_name predicates
    let substitutions := preds.foldl (fun acc ep =>
      if ep.predicate == "temp_compound_name" then
        match (getArg ep "X1", getArg ep "X2") with
        | (some x1, some x2) => (x2, x1) :: acc
        | _ => acc
      else acc) []
    dbg_trace s!"Collected substitutions: {substitutions}"
    -- Then process predicates normally
    let emptyStats : Stats := default
    let (result, _) := processPredicates parent rootPreds [] hm emptyStats ev
    -- Finally apply all substitutions to the result
    match result with
    | none => none
    | some formula =>
      some (substitutions.foldl (fun f (old, new) => f.substitute old new) formula)
      |>.map Formula.removeEmptyConj

def phase3 (f : Formula) : Formula := 
  minimizeScoping f

def phase4 (f : Formula) : String :=
  formatAsPWL f

def updateHandleMap (preds : List EP) : Multimap Var EP :=
  preds.foldl (fun hm ep => hm.insert ep.label ep) Multimap.empty

def transform (handle : Var) (preds : List EP) (hm : Multimap Var EP) : String :=
  let msg := "Transform - Starting with handle " ++ toString handle ++
             "\nPreds count: " ++ toString preds.length ++
             "\nHandle map size: " ++ toString hm.keys.length
  dbg_trace msg
  
  let (p1preds, ev) := phase1 handle preds hm
  dbg_trace ("After phase1, updating handle map with temp compounds")
  let newHm := updateHandleMap p1preds 
  dbg_trace ("  new handle map size: " ++ toString newHm.keys.length)
  
  match phase2 handle handle p1preds ev newHm with
  | none => "!!! NO FORMULA GENERATED !!!"
  | some formula => 
      let minScoped := phase3 formula
      phase4 minScoped

end PWL.Transform 

export PWL.Transform (transform)
