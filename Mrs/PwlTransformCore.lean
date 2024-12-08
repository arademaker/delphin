import Mrs.Basic
import Mrs.PwlVarFormat
import Mrs.PwlTypes
import Mrs.Hof
import Lean.Data.HashMap
import Util.InsertionSort

namespace PWL.Transform

open Lean (HashMap)
open MRS (EP Var)
open MM (Multimap)
open PWL (joinComma joinSep reformQuotedPair getArg)
open InsertionSort

instance : Ord (String × Var) where
  compare := fun a b =>
    if a.1.startsWith "ARG" && b.1.startsWith "ARG" then
      match (a.1.drop 3).toNat?, (b.1.drop 3).toNat? with
      | some n1, some n2 => compare n1 n2
      | some _, none => .lt
      | none, some _ => .gt
      | none, none => .eq
    else .eq

inductive PWLQuantifier where
  | proper_q : Var → String → PWLQuantifier  -- var, name
  | some_q : Var → String → String → PWLQuantifier  -- var, expanded_rstr, expanded_body
  | other_q : String → Var → String → String → PWLQuantifier  -- predname, var, expanded_rstr, expanded_body
  deriving BEq, Inhabited

structure CompoundMatch where
  var1 : Var
  var2 : Var
  name1 : String
  name2 : String
  rstr : Var
  body : Var
  deriving Repr, BEq, Inhabited

structure TransformResult where
  quants : List PWLQuantifier := []  
  eqs : List (Var × Var) := []       
  vars : List Var := []              
  deriving Inhabited

private def matchArg (args : List (String × Var)) (argName : String) (targetVar : Var) : Bool :=
  match args.find? (fun x => x.1 == argName && x.2 == targetVar) with
  | some _ => true 
  | none => false

private def handlePredsToString (preds : List EP) : String := 
  String.intercalate ", " (preds.map toString)

private def formatPredicate (var : Var) (ep : EP) : String :=
  dbg_trace s!"formatPredicate: Processing {ep.predicate} for var {var}"
  dbg_trace s!"Arguments: {ep.rargs}"
  
  let orderedArgs := insertionSort ep.rargs
  dbg_trace s!"Ordered arguments: {orderedArgs}"
    
  let argsStr := String.intercalate ", " (orderedArgs.map fun (_, argVar) => toString argVar)
  dbg_trace s!"Combined arguments: {argsStr}"
  
  let predName := if ep.predicate.startsWith "_" then ep.predicate.drop 1 else ep.predicate
  let result := s!"{predName}({argsStr})"
  dbg_trace s!"Final result: {result}"
  result

private def expandQuantifier (hm : Multimap Var EP) (mainVar : Var) (rstr : Var) (body : Var) : String × String :=
  dbg_trace s!"Expanding quantifier for var={mainVar}, rstr={rstr}, body={body}"
  match (hm.find? rstr, hm.find? body) with
  | (some rstrPreds, some bodyPreds) =>
    dbg_trace s!"Found handle content:"
    dbg_trace s!"  RSTR preds: {rstrPreds}"
    dbg_trace s!"  BODY preds: {bodyPreds}"
    let rstrStr := String.intercalate " & " (rstrPreds.map (formatPredicate mainVar))
    let bodyStr := String.intercalate " & " (bodyPreds.map (formatPredicate mainVar))
    dbg_trace s!"Expanded to: ({rstrStr}), {bodyStr}"
    (rstrStr, bodyStr)
  | _ => 
    dbg_trace "Failed to find handle content"
    ("", "")

private def findCompoundMatches (preds : List EP) : List CompoundMatch :=
  let rec processOne (ep : EP) : Option CompoundMatch := 
    if ep.predicate == "compound" then
      dbg_trace "Processing compound predicate"
      let arg1 := ep.rargs.find? (fun p => p.1 == "ARG1")
      let arg2 := ep.rargs.find? (fun p => p.1 == "ARG2")
      match (arg1, arg2) with
      | (some (_, var1), some (_, var2)) =>
        dbg_trace s!"Found compound args: var1={var1}, var2={var2}"
        let n1 := preds.find? (fun p => 
          p.predicate == "named" && matchArg p.rargs "ARG0" var1 && Option.isSome p.carg)
        let n2 := preds.find? (fun p => 
          p.predicate == "named" && matchArg p.rargs "ARG0" var2 && Option.isSome p.carg)
        let pq1 := preds.find? (fun p => 
          p.predicate == "proper_q" && matchArg p.rargs "ARG0" var1)
        let getRstrArg p := p.rargs.find? (fun pair => pair.1 == "RSTR") |>.bind (fun r => some r.2)
        let getBodyArg p := p.rargs.find? (fun pair => pair.1 == "BODY") |>.bind (fun r => some r.2)
        match (n1.bind (fun x => x.carg), n2.bind (fun x => x.carg),
               pq1.bind getRstrArg,
               pq1.bind getBodyArg) with
        | (some s1, some s2, some rstr, some body) => 
          dbg_trace "Found complete compound match"
          some { var1 := var1, var2 := var2, name1 := s1, name2 := s2, rstr := rstr, body := body }
        | _ => 
          dbg_trace "Failed to find all compound components"
          none
      | _ => none
    else none

  let rec findAll (remaining : List EP) (acc : List CompoundMatch) : List CompoundMatch :=
    match remaining with
    | [] => acc
    | ep :: rest =>
      match processOne ep with
      | some m => findAll rest (m :: acc)
      | none => findAll rest acc

  findAll preds []

def phase1 (preds : List EP) (hm : Multimap Var EP) : List EP :=
  dbg_trace "Starting phase1"
  let foundMatches := findCompoundMatches preds
  dbg_trace s!"Found {foundMatches.length} compound matches"
  let processMatch (m : CompoundMatch) : EP :=
    let s1_clean := if m.name1.startsWith "\"" then String.dropRight (String.drop m.name1 1) 1 else m.name1
    let s2_clean := if m.name2.startsWith "\"" then String.dropRight (String.drop m.name2 1) 1 else m.name2
    EP.mk "temp_compound_name" none m.var1
      [("ARG1", m.var1), ("ARG2", m.var2), ("RSTR", m.rstr), ("BODY", m.body)]
      (some s!"\"{s2_clean} {s1_clean}\"")

  let temp_compounds := foundMatches.map processMatch
  let remaining := preds.filter (fun p => 
    p.predicate != "compound" &&
    not (foundMatches.any fun m =>
      (p.predicate == "named" && matchArg p.rargs "ARG0" m.var1) ||
      (p.predicate == "proper_q" && matchArg p.rargs "ARG0" m.var1) ||
      (p.predicate == "named" && matchArg p.rargs "ARG0" m.var2) ||
      (p.predicate == "proper_q" && matchArg p.rargs "ARG0" m.var2)))

  dbg_trace "Completed phase1"
  remaining ++ temp_compounds

def phase2 (preds : List EP) (hm : Multimap Var EP) : TransformResult :=
  dbg_trace "Starting phase2"
  let init : TransformResult := {quants := [], eqs := [], vars := []}
  preds.foldl (fun res ep =>
    dbg_trace s!"Processing EP: {ep.predicate} with label={ep.label}"
    dbg_trace s!"  Args: {ep.rargs}"
    if ep.predicate == "temp_compound_name" then
      dbg_trace "Found temp_compound_name"
      match (ep.rargs.find? (fun x => x.1 == "ARG1"),
             ep.rargs.find? (fun x => x.1 == "ARG2"),
             ep.rargs.find? (fun x => x.1 == "RSTR"),
             ep.rargs.find? (fun x => x.1 == "BODY"),
             ep.carg) with
      | (some (_, var1), some (_, var2), some (_, rstr), some (_, body), some name) =>
        { res with 
          quants := PWLQuantifier.proper_q var1 name :: res.quants,
          eqs := (var1, var2) :: res.eqs,
          vars := var1 :: var2 :: res.vars }
      | _ => res
    else if ep.predicate == "proper_q" then
      match (ep.rargs.find? (fun x => x.1 == "ARG0"), ep.carg) with
      | (some (_, var), some name) =>
        { res with
          quants := PWLQuantifier.proper_q var name :: res.quants,
          vars := var :: res.vars }
      | _ => res
    else if ep.predicate.endsWith "_q" then
      match (ep.rargs.find? (fun x => x.1 == "ARG0"),
             ep.rargs.find? (fun x => x.1 == "RSTR"),
             ep.rargs.find? (fun x => x.1 == "BODY")) with
      | (some (_, var), some (_, rstr), some (_, body)) =>
        let (rstrExpanded, bodyExpanded) := expandQuantifier hm var rstr body
        let predName := if ep.predicate.startsWith "_" then ep.predicate.drop 1 else ep.predicate
        match predName with
        | "some_q" =>
          { res with
            quants := PWLQuantifier.some_q var rstrExpanded bodyExpanded :: res.quants,
            vars := var :: res.vars }
        | _ =>
          { res with
            quants := PWLQuantifier.other_q predName var rstrExpanded bodyExpanded :: res.quants,
            vars := var :: res.vars }
      | _ => res
    else res) init

def phase3 (result : TransformResult) : String :=
  let vars := result.vars.reverse.eraseDups
  let varList := String.intercalate "," (vars.map toString)

  let quantStrs := result.quants.map fun quant =>
    match quant with
    | PWLQuantifier.proper_q var name => 
      s!"?[n]:(name(n) & arg1(n)={var} & arg2(n)={name})"
    | PWLQuantifier.some_q var rstr body =>
      s!"?[{var}]:({rstr} & {body})"
    | PWLQuantifier.other_q predname var rstr body =>
      s!"{predname}({var}, ({rstr}), {body})"

  let eqStrs := result.eqs.map fun (v1, v2) => s!"{v1}={v2}"
  let allParts := quantStrs ++ eqStrs
  let body := String.intercalate " &\n  " allParts
  s!"?[{varList}]:(\n  {body}\n)"

def transform (preds : List EP) (hm : Multimap Var EP) : String := 
  dbg_trace "Starting full transform"
  dbg_trace s!"Handle map contains: {hm.keys}"
  let phase1Result := phase1 preds hm
  dbg_trace "Phase 1 complete"
  let phase2Result := phase2 phase1Result hm
  dbg_trace "Phase 2 complete"
  phase3 phase2Result

end PWL.Transform
