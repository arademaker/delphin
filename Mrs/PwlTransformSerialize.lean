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

def formatArgAssignment (first : Var) (arg : Var) (num : Nat) : String :=
  s!"arg{num}({first}) = {arg}"

/-- Find first x-variable in argument list -/
def findFirstXVar (args : List (String × Var)) : Option Var :=
  args.find? (fun p => p.2.sort == 'x') |>.map Prod.snd

def formatPredicateArgs (allArgs : List (String × Var)) : String :=
  let args := allArgs.filter (fun a => a.1.startsWith "ARG")
  match args with
  | [] => ""
  | [(_, v)] => toString v
  | args => 
    -- Find first x-variable to use as anchor
    match findFirstXVar args with
    | some xvar =>
      -- Keep all non-x variables in their original order
      let nonXVars := args.filter fun (_, v) => v.sort ≠ 'x'
      let assignmentStrs := nonXVars.foldl (fun (acc : List String × Nat) (_, v) =>
        (formatArgAssignment xvar v acc.2 :: acc.1, acc.2 + 1)) ([], 1)
      -- Put x-variable first followed by ordered arg assignments
      toString xvar ++ (if nonXVars.isEmpty then "" else ", " ++ String.intercalate ", " assignmentStrs.1.reverse)
    | none =>
      -- No x-variable - use first arg as anchor
      let first := args.head!.2
      let rest := args.tail!
      let assignmentStrs := rest.foldl (fun (acc : List String × Nat) (_, v) =>
        (formatArgAssignment first v acc.2 :: acc.1, acc.2 + 1)) ([], 1)
      toString first ++ (if rest.isEmpty then "" else ", " ++ String.intercalate ", " assignmentStrs.1.reverse)

partial def formatAsPWL (f : Formula) : String := 
  let rec substituteVar (oldVar newVar : Var) : Formula → String
    | Formula.atom ep =>
      let args := ep.rargs.map fun (s, v) => (s, if v == oldVar then newVar else v)
      let args' := if args.any (fun (_, v) => v == newVar)
        then args else args
      s!"{ep.predicate}({formatPredicateArgs args})"
    | Formula.conj [] => ""
    | Formula.conj [f] => substituteVar oldVar newVar f
    | Formula.conj fs => s!"({String.intercalate " & " (fs.map (substituteVar oldVar newVar))})"
    | Formula.scope vars quant inner =>
      if vars.any (· == oldVar)
      then formatAsPWL (Formula.scope vars quant inner)
      else 
        match quant with
        | none => s!"?[{varList_toString vars}]:({substituteVar oldVar newVar inner})"
        | some "every_q" => s!"![{varList_toString vars}]:(/* every_q */ {substituteVar oldVar newVar inner})"
        | some q => s!"?[{varList_toString vars}]:(/* {q} */ {substituteVar oldVar newVar inner})"

  let formatTheQScope (vars : List Var) (rstr body : Formula) : String :=
    match vars with
    | [x] => 
      -- Use s as our bound variable
      let newVar := {id := x.id, sort := x.sort, props := x.props}  -- Create 's' var
      s!"?[S]:(S=^[s]:{substituteVar x newVar rstr} & size(S)=1 & S({x}) & {formatAsPWL body})"
    | _ => 
      s!"?[{varList_toString vars}]:({formatAsPWL rstr} & {formatAsPWL body})"

  match f with
  | Formula.atom ep => 
    let args := ep.rargs
    let base := match ep.carg with
      | some value => s!", \"{removeExtraQuotes value}\""
      | none => ""
    if ep.predicate.endsWith "_q" then
      -- For quantifiers, order args normally
      let orderedArgs := orderPredicateArgs args
      let argStr := String.intercalate ", " (orderedArgs.map fun r => toString r.2)
      s!"{ep.predicate}({argStr}{base})"
    else
      -- Regular predicate with new argument format 
      s!"{ep.predicate}({formatPredicateArgs args}{base})"

  | Formula.conj [] => ""
  | Formula.conj [f] => formatAsPWL f
  | Formula.conj fs =>
    let nonEmpty := fs.filter (fun f => !f.isEmptyConj)
    match nonEmpty with
    | [] => ""
    | [Formula.atom ep, rest] => 
      if ep.predicate.endsWith "_q" then
        s!"({formatAsPWL (Formula.atom ep)} {formatAsPWL rest})"
      else
        s!"({formatAsPWL (Formula.atom ep)} & {formatAsPWL rest})"
    | fs => s!"({String.intercalate " & " (fs.map formatAsPWL)})"

  | Formula.scope vars quant inner => 
    dbg_trace s!"formatAsPWL processing scope with vars {vars} and quant {quant}"
    match quant, inner with
    | some "the_q", Formula.conj [rstr, body] =>
      formatTheQScope vars rstr body
    | none, _ => s!"?[{varList_toString vars}]:({formatAsPWL inner})"
    | some "every_q", _ => s!"![{varList_toString vars}]:(/* every_q */ {formatAsPWL inner})"
    | some "no_q", _ => s!"?[{varList_toString vars}]:(/* no_q */ {formatAsPWL inner})"
    | some q, _ => s!"?[{varList_toString vars}]:(/* {q} */ {formatAsPWL inner})"

end PWL.Transform.Serialize

export PWL.Transform.Serialize (formatAsPWL)
