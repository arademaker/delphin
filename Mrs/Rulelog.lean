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
  "% Base type definitions\n" ++
  "event(?E) :- event_value(?E, _).\n" ++
  "individual(?X) :- person(?X).\n" ++
  "individual(?X) :- place(?X).\n" ++
  "individual(?X) :- other_object(?X).\n\n" ++
  "% Quantifier rules\n" ++
  "a_q(?Restr, ?Body) :- apply(?Restr, ?X), apply(?Body, ?X).\n" ++
  "every_q(?Restr, ?Body) :- \\+ (apply(?Restr, ?X), \\+ apply(?Body, ?X)).\n" ++
  "some_q(?Restr, ?Body) :- apply(?Restr, ?X), apply(?Body, ?X).\n" ++
  "the_q(?Restr, ?Body) :- apply(?Restr, ?X), apply(?Body, ?X), \\+ (apply(?Restr, ?Y), ?Y \\= ?X).\n" ++
  "proper_q(?Restr, ?Body) :- apply(?Restr, ?X), \\+ (apply(?Restr, ?Y), \\+ apply(?Body, ?Y)).\n" ++
  "no_q(?Restr, ?Body) :- \\+ (apply(?Restr, ?X), apply(?Body, ?X)).\n\n" ++
  "% Domain predicates\n" ++
  "person(?X) :- (butler_n_1(?X); named(?X,'Agatha'); named(?X,'Charles')), \\+ place(?X), \\+ other_object(?X).\n" ++
  "same_named(?X, ?Y) :- named(?X, ?N), named(?Y, ?N).\n" ++
  "different_named(?X, ?Y) :- named(?X, ?N1), named(?Y, ?N2), ?N1 \\= ?N2.\n"

def removeQuotes (s : String) : String :=
  if s.startsWith "\"" && s.endsWith "\"" then s.extract ⟨1⟩ ⟨s.length - 1⟩ else s

def formatString (s : String) : String :=
  let s' := if s.startsWith "\"" && s.endsWith "\"" then s.extract ⟨1⟩ ⟨s.length - 1⟩ else s
  s!"'{s'}'"

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
      s!"event_{sentenceNumber}_{var.id}"
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
    | some iterVar => defaultExpr
    | none =>
      match (em.find? var) with
      | some extraList => defaultExpr
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
      | some str => joinSep args ", " ++ s!", {formatString str}"
      | none => joinSep args ", "
    
    preds.foldl (fun acc ep =>
      acc ++ s!"{fixName ep}({joinArgs ep}).\n"
    ) ""

  printNormal firstEp.label preds

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

  let rules := hm.keys.map (EP.format sentenceNumber qm em hm mrs.top)

  let eventValues := eSet.map (fun var => 
    s!"event_value(event_{sentenceNumber}_{var.id}, {var.id}).")

  let str := libraryRoutines ++ "\n" ++
             "% Event declarations\n" ++
             (joinSep eventValues "\n") ++ "\n\n" ++
             "% Knowledge base\n" ++
             (joinSep rules "\n")
  
  (str, strings.keys, eSet)

end Rulelog
