import Mrs.Basic
import Mrs.PwlTypes
import Mrs.PwlTransformShared
import Util.InsertionSort

namespace PWL.Transform.Serialize

open MRS (EP Var)
open PWL.Transform (Formula)
open InsertionSort

def orderPredicateArgs (args : List (String × Var)) : List (String × Var) :=
  args.filter (fun a => a.1.startsWith "ARG") |> insertionSort

def varList_toString (vars : List Var) : String :=
  String.intercalate "," (vars.map toString)

def removeExtraQuotes (s : String) : String :=
  if s.startsWith "\"" && s.endsWith "\"" then s.extract ⟨1⟩ ⟨s.length - 1⟩ else s

partial def formatAsPWL (f : Formula) : String := 
  match f with
  | Formula.atom ep => 
    let args := (orderPredicateArgs ep.rargs).map fun r => toString r.2
    let argStr := match ep.carg with
      | some value => s!"{String.intercalate ", " args}, \"{removeExtraQuotes value}\""
      | none => String.intercalate ", " args
    s!"{ep.predicate}({argStr})"

  | Formula.conj [] => ""
  | Formula.conj [f] => formatAsPWL f
  | Formula.conj fs =>
    let nonEmpty := fs.filter (fun f => !f.isEmptyConj)
    match nonEmpty with
    | [] => ""
    | fs => s!"({String.intercalate " & " (fs.map formatAsPWL)})"

  | Formula.scope vars quant inner => 
    dbg_trace s!"formatAsPWL processing scope with vars {vars} and quant {quant}"
    match quant with
    | none => s!"?[{varList_toString vars}]:({formatAsPWL inner})"
    | some "every_q" => s!"![{varList_toString vars}]:(/* every_q */ {formatAsPWL inner})"
    | some "no_q" => s!"?[{varList_toString vars}]:(/* no_q */ {formatAsPWL inner})"
    | some q => s!"?[{varList_toString vars}]:(/* {q} */ {formatAsPWL inner})"

end PWL.Transform.Serialize

export PWL.Transform.Serialize (formatAsPWL)
