import Mrs.Basic
import Mrs.PwlTypes
import Mrs.PwlVarFormat
import Mrs.PwlTransformShared
import Util.InsertionSort

namespace PWL.Transform.MinScoping

open MRS
open InsertionSort
open PWL.Transform
open Lean (HashMap)

structure QuantifierInfo where
  quant : String
  boundVars : List Var
  deriving Inhabited

structure FormulaState where
  declared : List Var
  neededVars : List Var
  formula : Formula
  deriving Inhabited

instance : ToString QuantifierInfo where
  toString qi := s!"{qi.quant}({qi.boundVars})"

instance : ToString (HashMap Var String) where
  toString m := s!"{m.toList}"

/-- Detect if formula contains universal quantification -/
partial def hasUniversalQuantificationInFormula : Formula → Bool
| Formula.atom _ => 
  dbg_trace "Checking atom: no universal quantification"
  false 
| Formula.conj fs => 
  (match fs with
  | [] => false
  | _ =>
    dbg_trace "Checking conjunction for universal quantification"
    fs.any hasUniversalQuantificationInFormula)
| Formula.scope _ (some "every_q") _ => 
  dbg_trace "Found universal quantification at scope level"
  true
| Formula.scope _ _ inner => 
  dbg_trace "Checking inner scope for universal quantification"
  hasUniversalQuantificationInFormula inner

/-- Extract x-variables used in predicate -/
def getXVars (ep : EP) : List Var :=
  dbg_trace s!"Extracting x-variables from EP: {ep}"
  (ep.rargs.filter (·.2.sort == 'x')
   |>.map (·.2)
   |>.eraseDups)

/-- Check if variable is universally quantified -/
def isUniversalVar (quantifiers : List QuantifierInfo) (var : Var) : Bool :=
  (match quantifiers.find? (fun qi => qi.quant == "every_q" && qi.boundVars.contains var) with
  | some _ => 
    dbg_trace s!"Variable {var} is universal"
    true
  | none => 
    dbg_trace s!"Variable {var} is not universal"
    false)

/-- Create scoped formula while respecting universal/existential ordering -/
def createScopedFormula (formula : Formula) (univVars existVars : List Var) (quantMap : HashMap Var String) : Formula :=
  (match univVars with
  | [] =>
    dbg_trace "No universal variables"
    (match existVars with
    | [] =>
      dbg_trace "No existential variables - returning base formula"
      formula
    | _ =>
      dbg_trace s!"Processing existential vars: {existVars}"
      (match existVars.head? with
      | some v =>
        dbg_trace s!"Looking up quantifier for head var: {v}"
        let quant := quantMap.find? v
        dbg_trace s!"Found quantifier: {quant}"
        Formula.scope existVars quant formula
      | none =>
        dbg_trace "No head variable found"
        formula))
  | _ =>
    dbg_trace s!"Processing universal vars: {univVars}"
    let withUniv := Formula.scope univVars (some "every_q") formula
    (match existVars with
    | [] => withUniv
    | _ =>
      dbg_trace s!"Adding existential scope after universal"
      (match existVars.head? with
      | some v =>
        let quant := quantMap.find? v
        dbg_trace s!"Using quantifier {quant} for existentials"
        Formula.scope existVars quant withUniv
      | none => withUniv)))

/-- Extract predicates from a formula -/
partial def getPredicatesFromFormula : Formula → List EP
| Formula.atom ep => [ep]
| Formula.conj fs => 
  (match fs with
  | [] => []
  | _ =>
    dbg_trace "Extracting predicates from conjunction"
    fs.foldl (fun acc f => acc ++ getPredicatesFromFormula f) [])
| Formula.scope _ _ inner => 
  dbg_trace "Extracting predicates from scope"
  getPredicatesFromFormula inner

/-- Extract quantifier info from formula scope -/
partial def getFormulaQuantifiers : Formula → List QuantifierInfo  
| Formula.atom _ => []
| Formula.conj fs => 
  (match fs with
  | [] => []
  | _ => 
    dbg_trace "Getting quantifiers from conjunction"
    fs.foldl (fun acc f => acc ++ getFormulaQuantifiers f) [])
| Formula.scope vars (some quant) inner => 
  (match quant with
  | q => 
    dbg_trace s!"Found scope with quantifier: {q}"
    if q.endsWith "_q" then
      dbg_trace s!"Quantifier {q} matches _q pattern"
      let quantInfo := { quant := q, boundVars := vars.eraseDups }
      -- Add this line to collect nested quantifiers
      let innerQuants := getFormulaQuantifiers inner
      dbg_trace s!"Collected inner quantifiers: {innerQuants}"
      quantInfo :: innerQuants
    else
      dbg_trace "Quantifier does not end with _q"
      getFormulaQuantifiers inner)
| Formula.scope _ none inner =>
  dbg_trace "Found scope with no quantifier, checking inner formula"
  getFormulaQuantifiers inner

/-- Build formula from predicates and quantifiers -/
def buildFormula (preds : List EP) (quantifiers : List QuantifierInfo) : Formula :=
  dbg_trace s!"Building formula with {preds.length} predicates and {quantifiers.length} quantifiers"
  
  let initialState : FormulaState := {
    declared := [],
    neededVars := [],
    formula := Formula.conj []
  }
  
  let baseState := preds.foldl (fun state ep => 
    dbg_trace s!"Processing predicate: {ep}"
    let varsUsed := getXVars ep
    let newNeeded := varsUsed.filter (fun v => !state.declared.contains v)
    let newFormula := match state.formula with
    | Formula.conj fs => Formula.conj (Formula.atom ep :: fs)
    | f => Formula.conj [Formula.atom ep, f]
    { declared := state.declared,
      neededVars := (state.neededVars ++ newNeeded).eraseDups,
      formula := newFormula }) initialState
  
  dbg_trace s!"Base state built with {baseState.neededVars.length} needed variables"
  
  let univVars := baseState.neededVars.filter (isUniversalVar quantifiers)
  let existVars := baseState.neededVars.filter (fun v => !univVars.contains v)
  
  let quantMap := (match quantifiers with
  | [] => 
    dbg_trace "No quantifiers to process"
    HashMap.empty
  | _ =>
    quantifiers.foldl (fun map qi => 
      (match qi.boundVars.head? with
      | some v => 
        dbg_trace s!"Adding to quantMap: {v} -> {qi.quant}"
        map.insert v qi.quant
      | none => map)) HashMap.empty)
  
  dbg_trace s!"Created quantMap with {quantMap.size} entries"
  createScopedFormula baseState.formula univVars existVars quantMap

/-- Public interface for minimum scoping -/
def minimizeScoping (f : Formula) : Formula :=
  dbg_trace "Starting minimizeScoping"
  let preds := getPredicatesFromFormula f
  let quants := getFormulaQuantifiers f 
  dbg_trace s!"Found {preds.length} predicates and {quants.length} quantifiers"
  buildFormula preds quants

end PWL.Transform.MinScoping

export PWL.Transform.MinScoping (minimizeScoping)
