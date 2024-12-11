import Mrs.Basic
import Mrs.PwlVarFormat
import Mrs.PwlTypes
import Mrs.PwlTransformShared
import Mrs.PwlTransformSurface
import Mrs.Hof
import Lean.Data.HashMap
import Util.InsertionSort

namespace PWL.Transform

open Lean (HashMap)
open MRS (EP Var)
open MM (Multimap)
open PWL (joinComma joinSep reformQuotedPair getArg compareArgs)
open InsertionSort

/-- A compound match consists of variables with names and handles -/
structure CompoundMatch where
  var1 : Var
  var2 : Var
  name1 : String
  name2 : String
  rstr : Var
  body : Var
  deriving Repr, BEq, Inhabited

/-- A non-quantifier argument pair of string and variable -/
structure NonQuantArg where
  pair : String × Var
  deriving Inhabited

private instance : Ord NonQuantArg where
  compare a b := let getNum (s : String) : Option Nat :=
                   if s.startsWith "ARG" then
                     let numStr := s.drop 3
                     numStr.toNat?
                   else none
                 match (getNum a.pair.1, getNum b.pair.1) with
                 | (some n1, some n2) => compare n1 n2  
                 | (some _, none) => .lt
                 | (none, some _) => .gt
                 | (none, none) => compareArgs a.pair b.pair

private def matchArg (args : List (String × Var)) (argName : String) (targetVar : Var) : Bool :=
  match args.find? (fun x => x.1 == argName && x.2 == targetVar) with
  | some _ => true 
  | none => false

private def handlePredsToString (preds : List EP) : String := 
  String.intercalate ", " (preds.map toString)

private def normalizedPredName (predicate : String) : String :=
  if predicate.startsWith "_" then predicate.drop 1 else predicate

/-- Find all compound matches in the predicates -/
private def findCompoundMatches (preds : List EP) : List CompoundMatch :=
  let rec processOne (ep : EP) : Option CompoundMatch := 
    if ep.predicate == "compound" then
      let arg1 := ep.rargs.find? (fun p => p.1 == "ARG1")
      let arg2 := ep.rargs.find? (fun p => p.1 == "ARG2")
      match (arg1, arg2) with
      | (some (_, var1), some (_, var2)) =>
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
          some { var1 := var1, var2 := var2, name1 := s1, name2 := s2, rstr := rstr, body := body }
        | _ => none
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

/-- Sort non-quantifier arguments for consistent ordering -/
private def nonQuantSort (args : List (String × Var)) : List (String × Var) :=
  let asNonQuant : List NonQuantArg := args.map (λ p => ⟨p⟩)
  let sorted := insertionSort asNonQuant
  sorted.map NonQuantArg.pair

partial def expandQuantifier (hm : Multimap Var EP) (_mainVar : Var) (rstr : Var) (body : Var) : String × String :=
  let rec processPredicates (preds : List EP) : String := 
    let normalized := fun (p : EP) => normalizedPredName p.predicate
    let nonQuantPreds := preds.filter (fun p => !p.predicate.endsWith "_q" && normalized p != "neg" && normalized p != "never_a_1")
    let quantPreds := preds.filter (fun p => p.predicate.endsWith "_q" || normalized p == "neg" || normalized p == "never_a_1")
    
    let nonQuantStr := String.intercalate " & " (nonQuantPreds.map (fun ep => 
      let sortedArgs := nonQuantSort ep.rargs
      let argsStr := String.intercalate ", " (sortedArgs.map (fun a => toString a.2))
      let predName := normalizedPredName ep.predicate
      let nameArg := if ep.predicate == "named" && Option.isSome ep.carg
                     then s!", {Option.get! ep.carg}"
                     else ""
      s!"{predName}({argsStr}{nameArg})"))
    
    let quantStr := quantPreds.foldl (fun acc ep =>
      if ep.predicate.endsWith "_q" then
        match insertionSort ep.rargs with 
        | (_, var) :: (_, rstr) :: (_, body) :: _ =>
          let (nestedRstr, nestedBody) := expandQuantifier hm var rstr body
          let predName := normalizedPredName ep.predicate
          let quantStr := s!"?[{var}]:(({nestedRstr}) & {nestedBody})"
          if acc == "" then quantStr else acc ++ " & " ++ quantStr
        | _ => acc
      else if normalized ep == "neg" || normalized ep == "never_a_1" then
        match ep.rargs.find? (fun arg => arg.2.sort == 'h') with
        | some (_, handleArg) =>
          let (rstrExpanded, _) := expandQuantifier hm ep.label handleArg handleArg
          let negStr := s!"~({rstrExpanded})"
          if acc == "" then negStr else acc ++ " & " ++ negStr
        | _ => acc
      else acc) ""
    
    if quantStr == "" then nonQuantStr 
    else if nonQuantStr == "" then quantStr
    else nonQuantStr ++ " & " ++ quantStr

  match (hm.find? rstr, hm.find? body) with
  | (some rstrPreds, some bodyPreds) =>
    (processPredicates rstrPreds, processPredicates bodyPreds)
  | _ => ("", "")

/-- Phase 1: Convert compound names to temporary predicates -/
def phase1 (preds : List EP) (hm : Multimap Var EP) : List EP :=
  let foundMatches := findCompoundMatches preds

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

  remaining ++ temp_compounds

/-- Phase 2: Build the PWL formula from predicates -/
def phase2 (preds : List EP) (hm : Multimap Var EP) : TransformResult :=
  let init : TransformResult := {quants := [], eqs := [], vars := []}
  preds.foldl (fun res ep =>
    if ep.predicate == "temp_compound_name" then
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
      match (getArg ep "ARG0", getArg ep "RSTR", getArg ep "BODY", ep.carg) with
      | (some var, some rstr, some body, some name) =>
        { res with
          quants := PWLQuantifier.proper_q var name :: res.quants,
          vars := var :: res.vars }
      | (some var, some rstr, some body, none) =>
        match hm.find? rstr with
        | some [namedEP] =>
          if namedEP.predicate == "named" && Option.isSome namedEP.carg then
            { res with
              quants := PWLQuantifier.proper_q var (Option.get! namedEP.carg) :: res.quants,
              vars := var :: res.vars }
          else res
        | _ => res
      | _ => res
    else if ep.predicate.endsWith "_q" then
      match insertionSort ep.rargs with
      | (_, var) :: (_, rstr) :: (_, body) :: _ =>
        let (rstrExpanded, bodyExpanded) := expandQuantifier hm var rstr body
        let predName := normalizedPredName ep.predicate
        if ["some_q", "a_q", "udef_q", "pronoun_q"].contains predName then
          { res with
            quants := PWLQuantifier.indefinite_q predName var rstrExpanded bodyExpanded :: res.quants,
            vars := var :: res.vars }
        else if ["the_q", "def_explicit_q"].contains predName then
          { res with
            quants := PWLQuantifier.definite_q predName var rstrExpanded bodyExpanded :: res.quants,
            vars := var :: res.vars }
        else
          { res with
            quants := PWLQuantifier.other_q predName var rstrExpanded bodyExpanded :: res.quants,
            vars := var :: res.vars }
      | _ => res
    else res) init

/-- Main transform function that applies phase1 and phase2 and formats the result -/
def transform (preds : List EP) (hm : Multimap Var EP) : String := 
  let phase1Result := phase1 preds hm
  let transformResult := phase2 phase1Result hm
  formatToSurface transformResult

end PWL.Transform
