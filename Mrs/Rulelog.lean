import Lean.Data.HashMap
import Mrs.Basic
import Mrs.Hof
import Util.InsertionSort

namespace Rulelog

open Lean (HashMap)
open MRS (Var EP Constraint MRS)
open HOF
open MM
open InsertionSort

def libraryRoutines : String := "% Library predicates\n" ++
  "a_q(?Restr, ?Body) :- true.\n" ++
  "every_q(?Restr, ?Body) :- true.\n" ++
  "some_q(?Restr, ?Body) :- true.\n" ++
  "the_q(?Restr, ?Body) :- true.\n" ++
  "proper_q(?Restr, ?Body) :- true.\n" ++
  "pronoun_q(?Restr, ?Body) :- true.\n" ++
  "udef_q(?Restr, ?Body) :- true.\n" ++
  "def_explicit_q(?Restr, ?Body) :- true.\n" ++
  "no_q(?Restr, ?Body) :- true.\n" ++
  "never_a_1(?X) :- true.\n" ++
  "neg(?E, ?X) :- true.\n" ++
  "colon_p_namely(?E, ?X, ?Y) :- true.\n" ++
  "therein_p_dir(?E1, ?E2) :- true.\n" ++
  "live_v_1(?E, ?X) :- true.\n" ++
  "people_n_of(?X) :- true.\n" ++
  "victim_n_of(?X) :- true.\n" ++
  "only_a_1(?E, ?X) :- true.\n" ++
  "named(?X, ?Name) :- true.\n" ++
  "and_c(?X, ?Y, ?Z) :- true.\n" ++
  "butler_n_1(?X) :- true.\n" ++
  "killer_n_1(?X) :- true.\n" ++
  "implicit_conj(?X, ?Y, ?Z) :- true.\n" ++
  "be_v_id(?E, ?X, ?Y) :- true.\n" ++
  "in_p_loc(?E, ?E2, ?X) :- true.\n" ++
  "compound(?E, ?X, ?Y) :- true.\n" ++
  "person(?X) :- true.\n" ++
  "kill_v_1(?E, ?X, ?Y) :- true.\n" ++
  "hate_v_1(?E, ?X, ?Y) :- true.\n" ++
  "pron(?X) :- true.\n" ++
  "poss(?E, ?X, ?Y) :- true.\n"

def formatId (s : String) : String :=
  s!"'{s}'"  -- Use single quotes for Rulelog strings

def removeQuotes (s : String) : String :=
  if s.startsWith "\"" && s.endsWith "\"" then s.extract ⟨1⟩ ⟨s.length - 1⟩ else s

def fixName (ep : EP) : String :=
  let PredName := if ep.predicate == "_and_c" then "and_c" else ep.predicate
  match (PredName.get? 0) with
    | '_' => PredName.drop 1
    | _ => PredName

def joinSep (l : List String) (sep : String) : String := 
  l.foldr (fun s r => (if r == "" then s else r ++ sep ++ s)) ""

namespace Var
  def format (sentenceNumber : Nat) (var : Var) : String :=
    s!"?S{sentenceNumber}_{var.sort.toUpper}{var.id}"

  def formatGround (sentenceNumber : Nat) (var : Var) : String :=
    if var.sort == 'e' then
      s!"event_{sentenceNumber}_{var.id}"  -- Use numeric IDs for events
    else
      s!"s{sentenceNumber}_{var.sort}{var.id}"

  def format.labelOnly (sentenceNumber : Nat) (var : Var) : String :=
    if var.sort == 'x' then
      s!"S{sentenceNumber}_{var.sort.toUpper}{var.id}"
    else if var.sort == 'e' then
      s!"event_{sentenceNumber}_{var.id}"
    else
      s!"s{sentenceNumber}_{var.sort}{var.id}"

  def format.labelOnlyGround (sentenceNumber : Nat) (var : Var) : String :=
    if var.sort == 'e' then
      s!"event_{sentenceNumber}_{var.id}"
    else
      s!"s{sentenceNumber}_{var.sort}{var.id}"

  def formatWithDeps (sentenceNumber : Nat) (ep : EP) (var : Var) (qm : HashMap Var Var) (em : Multimap Var Var) : String :=
    let defaultExpr := format sentenceNumber var
    match qm.find? var with
    | some iterVar => 
      match (em.find? var) with
      | some extraList => 
        let l := extraList.filter (fun evar => iterVar != evar)
        let l1 := (insertionSort l).eraseDups
        let l2 := l1.map (fun item => format sentenceNumber item)
        let l2str := joinSep l2 ", "
        defaultExpr
      | none => defaultExpr
    | none =>
      match (em.find? var) with
      | some extraList => 
        let l := (insertionSort extraList).eraseDups
        defaultExpr
      | none => defaultExpr
end Var

def EP.format (sentenceNumber : Nat) (qm : HashMap Var Var) (em : Multimap Var Var) (hm : Multimap Var EP) (rootHandle : Var) (handle : Var) : String :=
  let preds := match (hm.find? handle) with
  | some value => value
  | none => []

  let firstEp := match preds.head? with
  | some value => value
  | none => 
    dbg_trace "firstEP"; sorry

  let printNormal (_l : Var) (preds : List EP) : String :=
    let joinArgs (ep : EP) := 
      let args := ep.rargs.map fun rarg => Var.formatWithDeps sentenceNumber ep rarg.snd qm em
      match ep.carg with
      | some str => joinSep args ", " ++ s!", '{removeQuotes str}'"  -- Use single quotes for strings
      | none => joinSep args ", "
    
    preds.foldl (fun acc ep =>
      acc ++ s!"{fixName ep}({joinArgs ep}).\n"
    ) ""

  printNormal firstEp.label preds

def generateEventDistinctness (mrs : MRS.MRS) (sentenceNumber : Nat) : String :=
  let events := collectEvents mrs.preds
  let declarations := events.map fun e => 
    s!"event_value(event_{sentenceNumber}_{e.id}, {e.id})."
  joinSep declarations "\n"

def MRS.format (sentenceNumber : Nat) (mrs : MRS.MRS) : (String × List String × List Var) :=
  let strings := mrs.preds.foldl (fun stab pred =>
    match pred with
    | {predicate := _, link := some _, label := l, rargs := _, carg := some c} =>
      stab.insert c l
    | {predicate := _, link := some _, label := _, rargs := _, carg := none} =>
      stab
    | {predicate := _, link := none, label := l, rargs := _, carg := some c} =>
      stab.insert c l
    | {predicate := _, link := none, label := _, rargs := _, carg := none} =>
      stab) Multimap.empty
      
  let eSet := collectEvents mrs.preds
  let qm := collectQuantifierVars mrs.preds
  let em := collectHOExtraVarsForEPs mrs.preds $
            collectHOExtraVarsForEPs mrs.preds $
            collectHOExtraVarsForEPs mrs.preds $
            collectHOExtraVarsForEPs mrs.preds $ collectExtraVarsForEPs mrs.preds qm
  let hm := collectEPsByHandle mrs.preds

  -- Generate rules
  let rules := hm.keys.map (EP.format sentenceNumber qm em hm mrs.top)

  -- Generate event declarations and distinctness constraints
  let eventDistinctness := generateEventDistinctness mrs sentenceNumber

  -- Generate type declarations  
  let typeDecls := eSet.map (fun var => 
    s!"event(event_{sentenceNumber}_{var.id}).")

  let str := libraryRoutines ++ "\n" ++ 
             "% Event declarations and distinctness\n" ++
             (joinSep typeDecls "\n") ++ "\n" ++
             eventDistinctness ++ "\n\n" ++
             "% Facts\n" ++
             (joinSep rules "\n")
  
  (str, strings.keys, eSet)

end Rulelog
