import Mrs.Basic
import Mrs.PwlVarFormat
import Mrs.PwlArguments
import Mrs.PwlTypes
import Mrs.Hof
import Lean.Data.HashMap

namespace PWL.Expression

open Lean (HashMap)
open MRS (EP Var)
open MM (Multimap)
open PWL (joinComma joinSep reformQuotedPair)
open PWL.Arguments (getArg)

inductive LogicalForm where 
  | pred : EP → LogicalForm                            -- kill_v_1(e2,x3,x23)
  | equals : Var × Var → LogicalForm                   -- x = y
  | properName : Var × String → LogicalForm            -- ?[n]:(name(n) & arg1(n)=x & arg2(n)=s)
  | someQ : Var × LogicalForm × LogicalForm → LogicalForm  -- some_q(x, rstr, body)
  | conj : List LogicalForm → LogicalForm              -- a & b & c
  | scope : List Var × LogicalForm → LogicalForm       -- ?[x1,x2]:(...)
  deriving Inhabited

structure ExprState where
  seenPreds : List EP
  handledHandles : List Var
  deriving Inhabited

def ExprState.empty : ExprState := ⟨[], []⟩

mutual

partial def formatLogicalForm : LogicalForm → String
  | LogicalForm.pred ep => 
    let args := ep.rargs.map fun r => toString r.2
    let name := if ep.predicate.startsWith "_" 
                then ep.predicate.drop 1 
                else ep.predicate
    s!"{name}({joinComma args})"
  | LogicalForm.equals (v1, v2) => s!"{toString v1}={toString v2}"
  | LogicalForm.properName (var, str) => 
    s!"?[n]:(name(n) & arg1(n)={toString var} & arg2(n)={str})"
  | LogicalForm.someQ (var, rstr, body) => 
    s!"some_q({toString var}, {formatLogicalForm rstr}, {formatLogicalForm body})"
  | LogicalForm.conj forms => 
    String.intercalate " & " (forms.map formatLogicalForm)
  | LogicalForm.scope (vars, form) => 
    let varStr := String.intercalate "," (vars.map toString)
    s!"?[{varStr}]:({formatLogicalForm form})"

partial def expandHandle (handle : Var) (hm : Multimap Var EP) (state : ExprState) : (LogicalForm × ExprState) :=
  if state.handledHandles.contains handle then
    (LogicalForm.pred (EP.mk "error_recursive" none handle [] none), state)
  else
    match hm.find? handle with
    | some preds => 
      let newState := {state with handledHandles := handle :: state.handledHandles}
      let (forms, finalState) := preds.foldl (fun (acc, st) ep =>
        if st.seenPreds.contains ep then (acc, st)
        else
          let (form, newSt) := transformEP ep hm {st with seenPreds := ep :: st.seenPreds}
          (form :: acc, newSt)
      ) ([], newState)
      (LogicalForm.conj forms.reverse, finalState)
    | none => (LogicalForm.pred (EP.mk "error_no_handle" none handle [] none), state)

partial def transformEP (ep : EP) (hm : Multimap Var EP) (state : ExprState) : (LogicalForm × ExprState) :=
  if ep.predicate == "proper_q" then
    match (getArg ep "ARG0", getArg ep "RSTR") with
    | (some var, some rstrHandle) =>
      -- Check for direct CARG first
      match ep.carg with
      | some name => (LogicalForm.properName (var, name), state)
      | none =>
        -- Look for named predicate in RSTR
        match hm.find? rstrHandle with
        | some [namedEP] =>
          if namedEP.predicate == "named" then
            match namedEP.carg with
            | some name => (LogicalForm.properName (var, name), state)
            | none => (LogicalForm.pred ep, state)
          else (LogicalForm.pred ep, state)
        | _ => (LogicalForm.pred ep, state)
    | _ => (LogicalForm.pred ep, state)
  else if ep.predicate == "=" then 
    match ep.rargs with
    | [(_, v1), (_, v2)] => (LogicalForm.equals (v1, v2), state)
    | _ => (LogicalForm.pred ep, state)
  else if ep.predicate == "some_q" then
    match (getArg ep "ARG0", getArg ep "RSTR", getArg ep "BODY") with
    | (some var, some rstr, some body) =>
      let (rstrForm, state1) := expandHandle rstr hm state
      let (bodyForm, state2) := expandHandle body hm state1
      (LogicalForm.someQ (var, rstrForm, bodyForm), state2)
    | _ => (LogicalForm.pred ep, state)
  else (LogicalForm.pred ep, state)

end

instance : ToString LogicalForm where
  toString := formatLogicalForm

def transformTopLevel (ep : EP) (hm : Multimap Var EP) (scopeVars : List Var) : LogicalForm :=
  let (form, _) := transformEP ep hm ExprState.empty
  LogicalForm.scope (scopeVars, form)

end PWL.Expression

export PWL.Expression (LogicalForm transformTopLevel)
