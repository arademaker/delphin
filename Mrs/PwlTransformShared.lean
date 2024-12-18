import Mrs.Basic
import Mrs.PwlTypes
import Util.InsertionSort

namespace PWL.Transform

open MRS (EP Var)
open InsertionSort

inductive Formula : Type where
  | atom : EP → Formula
  | conj : List Formula → Formula 
  | scope : List Var → Option String → Formula → Formula
  deriving Inhabited

mutual
  partial def formulaToString : Formula → String
    | Formula.atom ep => toString ep
    | Formula.conj fs => "(" ++ listFormulaToString fs ++ ")"
    | Formula.scope vars none inner => s!"?[{vars}]: {formulaToString inner}"
    | Formula.scope vars (some q) inner => s!"?[{vars}]: /* {q} */ {formulaToString inner}"

  partial def listFormulaToString : List Formula → String
    | [] => ""
    | [f] => formulaToString f 
    | f :: fs => formulaToString f ++ " & " ++ listFormulaToString fs
end

instance : ToString Formula where
  toString := formulaToString

instance : ToString (List Formula) where
  toString fs := "[" ++ String.intercalate ", " (fs.map formulaToString) ++ "]"

def Formula.isAtom : Formula → Bool
  | atom _ => true 
  | _ => false

def Formula.isConj : Formula → Bool 
  | conj _ => true
  | _ => false

def Formula.hasScope : Formula → Bool
  | scope _ _ _ => true
  | _ => false

def Formula.getScopedFormula : Formula → Option Formula
  | scope _ _ f => some f
  | _ => none

def Formula.getScopeVars : Formula → Option (List Var)
  | scope vs _ _ => some vs
  | _ => none

def Formula.isEmptyConj : Formula → Bool
  | conj [] => true
  | conj [f] => f.isEmptyConj
  | _ => false

partial def Formula.removeEmptyConj : Formula → Formula
  | atom ep => atom ep
  | conj [] => conj []  
  | conj [f] => f.removeEmptyConj
  | conj fs =>
    let nonEmpty := fs.filter (fun f => !f.isEmptyConj)
    match nonEmpty with
    | [] => conj []
    | [f] => f.removeEmptyConj
    | fs => conj (fs.map Formula.removeEmptyConj)
  | scope vars quant inner => 
    scope vars quant (inner.removeEmptyConj)

private def substituteVar (old new : Var) (args : List (String × Var)) : List (String × Var) :=
  dbg_trace s!"substituteVar old:{old} new:{new} args:{args}"
  args.map fun (name, var) => 
    let res := if var == old then (name, new) else (name, var)
    dbg_trace s!"  {name}: {var} => {res.2}"
    res

partial def Formula.substitute (old new : Var) : Formula → Formula
  | atom ep => 
    dbg_trace s!"substitute in atom EP {ep.predicate}, args: {ep.rargs}"
    let newArgs := substituteVar old new ep.rargs
    dbg_trace s!"  after substitution: {newArgs}"
    atom { ep with rargs := newArgs }
  | conj fs => 
    dbg_trace "substitute in conj"
    conj (fs.map (Formula.substitute old new))
  | scope vars quant inner => 
    dbg_trace s!"substitute in scope: vars {vars}"
    let newVars := vars.map fun v => if v == old then new else v
    dbg_trace s!"  after var substitution: {newVars}"
    scope newVars quant (inner.substitute old new)

def removeExtraQuotes (s : String) : String :=
  if s.startsWith "\"" && s.endsWith "\"" then s.extract ⟨1⟩ ⟨s.length - 1⟩ else s

def normalizedPredName (predicate : String) : String :=
  if predicate.startsWith "_" then predicate.drop 1 else predicate

def joinSep (l : List String) (sep : String) : String := 
  l.foldr (fun s r => (if r == "" then s else r ++ sep ++ s)) ""

def joinComma (l : List String) : String := 
  joinSep l ","

def reformQuotedPair (s : String) : String :=
  let parts := String.split s (· == ' ')
  let unquoted := parts.map removeExtraQuotes
  "\"" ++ " ".intercalate unquoted ++ "\""

def getArg (ep : EP) (name : String) : Option Var :=
  ep.rargs.find? (fun r => r.1 == name)
  |>.map (fun r => r.2)

def orderArgs (args : List (String × Var)) : List (String × Var) :=
  args.filter (fun a => a.1.startsWith "ARG") |> insertionSort

def getOrderedQuantArgs (args : List (String × Var)) : Option (Var × Var × Var) :=
  let arg0 := args.find? (fun p => p.1 == "ARG0")
  let rstr := args.find? (fun p => p.1 == "RSTR")
  let body := args.find? (fun p => p.1 == "BODY") 
  match arg0, rstr, body with 
  | some (_, a), some (_, r), some (_, b) => some (a, r, b)
  | _, _, _ => none

end PWL.Transform

export PWL.Transform (normalizedPredName joinSep joinComma reformQuotedPair getArg orderArgs getOrderedQuantArgs)
