import Mrs.Basic
import Mrs.PwlVarFormat
import Mrs.PwlArguments
import Mrs.PwlTypes
import Mrs.Hof
import Mrs.PwlTransformCore
import Mrs.PwlExpression
import Lean.Data.HashMap

namespace PWL.Transform

open Lean (HashMap)
open MRS (EP Var)
open MM (Multimap)
open PWL (joinComma joinSep reformQuotedPair)
open PWL.Arguments (getArg)
open PWL.Expression (LogicalForm transformTopLevel)

def collectScopeVars (ep : EP) (hm : Multimap Var EP) : List Var :=
  let rec findVars (size : Nat) (seen : List EP) (form : EP) : List Var :=
    if size = 0 then []
    else if seen.contains form then []
    else
      let argVars := form.rargs.foldl (fun acc (_, var) => 
        if var.sort == 'x' then var :: acc else acc) []
      let bodyVars := match getArg form "BODY" with
      | some body => match hm.find? body with
        | some preds => preds.foldl (fun acc p => acc ++ findVars (size-1) (form :: seen) p) []
        | none => []
      | none => []
      argVars ++ bodyVars
  
  let allVars := findVars ep.rargs.length [] ep
  allVars.eraseDups

def EP.format (ep : EP) (hm : Multimap Var EP) : String :=
  let vars := collectScopeVars ep hm
  let form := transformTopLevel ep hm vars
  toString form

def handleProperQ (ep: EP) (hm: Multimap Var EP) (_sentenceNumber: Nat) 
  (_qm: HashMap Var Var) (_em: Multimap Var Var) (_visited: Array Var) 
  (_formatDefinition: Nat → HashMap Var Var → Multimap Var Var → Multimap Var EP → Array Var → Var → Option (String × String) → String) : Option String :=
  let vars := collectScopeVars ep hm
  let form := transformTopLevel ep hm vars
  some (toString form)

end PWL.Transform

export PWL.Transform (EP.format handleProperQ)
