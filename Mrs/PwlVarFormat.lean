import Mrs.Basic
import Mrs.Hof
import Util.InsertionSort

namespace PWL

open InsertionSort
open MRS (EP Var)
open MM (Multimap)

def pwlVar (var : Var) : String :=
  match var.sort with
  | 'x' => s!"X{var.id}"
  | 'e' => s!"e{var.id}"
  | 'h' => s!"H{var.id}"
  | _ => s!"{var.sort}{var.id}"

def findArg (args : List (String × Var)) (name : String) : Option Var :=
  args.find? (fun r => r.1 == name)
  |>.map (fun r => r.2)

def formatBeVId (args : List (String × Var)) : String :=
  match (findArg args "ARG1", findArg args "ARG2") with
  | (some x1, some x2) =>
    s!"?[b]:(same(b) & arg1(b)={pwlVar x1} & arg2(b)={pwlVar x2})"
  | _ => "be_v_id(error: wrong number of arguments)"

def getVarsForScope (preds : List EP) (handle : Var) (em : Multimap Var Var) : List Var :=
  let directVars := preds.foldl (fun acc ep =>
    acc ++ (ep.rargs.filter (fun arg => arg.2.sort == 'x')).map (fun arg => arg.2)
  ) []
  match em.find? handle with
  | some extraVars => (insertionSort (directVars ++ extraVars)).eraseDups
  | none => (insertionSort directVars).eraseDups

end PWL

export PWL (pwlVar formatBeVId getVarsForScope)
