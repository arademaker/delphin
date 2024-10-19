import Lean.Data.HashMap
import Mrs.Basic
import Mrs.Hof
import Util.InsertionSort

namespace THF

open Lean (HashMap)
open MRS (Var EP Constraint MRS)
open HOF
open MM
open InsertionSort

def libraryRoutines : String := 
  "thf(a_q_decl,type,a_q:                       x > (x > $o) > (x > $o) > $o).\n" ++
  "thf(every_q_decl,type,every_q:               x > (x > $o) > (x > $o) > $o).\n" ++
  "thf(some_q_decl,type,some_q:                 x > (x > $o) > (x > $o) > $o).\n" ++
  "thf(the_q_decl,type,the_q:                   x > (x > $o) > (x > $o) > $o).\n" ++
  "thf(proper_q_decl,type,proper_q:             x > (x > $o) > (x > $o) > $o).\n" ++
  "thf(pronoun_q_decl,type,pronoun_q:           x > (x > $o) > (x > $o) > $o).\n" ++
  "thf(udef_q_decl,type,udef_q:                 x > (x > $o) > (x > $o) > $o).\n" ++
  "thf(def_explicit_q_decl,type,def_explicit_q: x > (x > $o) > (x > $o) > $o).\n" ++
  "thf(no_q_decl,type,no_q:                     x > (x > $o) > (x > $o) > $o).\n" ++
  "thf(never_a_1_decl,type,never_a_1:           ($o) > $o).\n" ++
  "thf(neg_decl,type,neg:                       e > ($o) > $o).\n" ++
  "thf(colon_p_namely,type,colon_p_namely:      e > ($o) > ($o) > $o).\n" ++
  "\n" ++ 
  -- "thf(book_n_of_decl,type,book_n_of: x > $o).\n" ++
  -- "thf(love_v_1_decl,type,love_v_1: x > x > $o).\n" ++
  -- "thf(a_q_decl,type,every_q: (x > $o) > (x > $o) > $o).\n" ++ 
  -- "thf(boy_n_1_decl,type,boy_n_1: x > $o).\n"  ++

  "thf(therein_p_dir_decl,type,therein_p_dir: e > e > $o).\n" ++
  "thf(live_v_1_decl,type,live_v_1: e > x > $o).\n" ++
  "thf(people_n_of_decl,type,people_n_of: x > $o).\n" ++
  "thf(vicitm_n_of_decl,type,victim_n_of: x > $o).\n" ++
  "thf(only_a_1_decl,type,only_a_1: e > x > $o).\n" ++
  "thf(named_decl,type,named: x > name > $o).\n" ++
  "thf(and_c_x_decl,type,and_c_x: x > x > x > $o).\n" ++
  "thf(and_c_e_decl,type,and_c_e: e > e > e > $o).\n" ++
  "thf(butler_n_1_decl,type,butler_n_1: x > $o).\n" ++
  "thf(killer_n_1_decl,type,killer_n_1: x > $o).\n" ++
  "thf(implicit_conj_decl,type,implicit_conj: x > x > x > $o).\n" ++
  "thf(be_v_id_decl,type,be_v_id: e > x > x > $o).\n" ++
  "thf(in_p_loc_decl,type,in_p_loc: e > e > x > $o).\n" ++
  "thf(compound_decl,type,compound: e > x > x > $o).\n" ++
  "thf(person_decl,type,person: x > $o).\n" ++
  "thf(kill_v_1_decl,type,kill_v_1: e > x > x > $o).\n" ++
  "thf(hate_v_1_decl,type,hate_v_1: e > x > x > $o).\n" ++
  "thf(pron_decl,type,pron: x > $o).\n" ++
  "thf(poss,type,poss: e > x > x > $o).\n" ++
  "thf(more_comp,type,more_comp: e > e > x > $o).\n"  ++
  "thf(rich_a_in,type,rich_a_in: e > x > $o).\n" ++
  "thf(always_a_1,type,always_a_1: e > $o).\n" ++
  "thf(aunt_n_of,type,aunt_n_of: x > $o).\n" ++
  "thf(card,type,card: e > x > name > $o).\n" ++
  "thf(generic_entity,type,generic_entity: x > $o).\n" ++
  "thf(except_p,type,except_p: e > x > x > $o).\n" ++
  "thf(therefore_a_1,type,therefore_a_1: ($o) > $o).\n" ++
  "thf(unknown,type,unknown: e > $o).\n"

def removeQuotes (s : String) : String :=
  if s.startsWith "\"" && s.endsWith "\"" then s.extract ⟨1⟩ ⟨s.length - 1⟩ else s

def formatId (s : String) : String :=
  let str := removeQuotes s
  s!"id_{str}"

def fixName (ep : EP) : String :=
  let checkEonly (ep : EP) : Bool :=
    let ret2 := ep.rargs.filter (fun item => item.2.sort == 'e')
    (ret2.length == ep.rargs.length)
  let selectAndForm (ep : EP) : String :=
    if (checkEonly ep) then "_and_c_e" else "_and_c_x"
  let PredName := if ep.predicate == "_and_c" then selectAndForm ep else ep.predicate
  match (PredName.get? 0) with
    | '_' => PredName.drop 1
    | _ => PredName

def joinSep (l : List String) (sep : String) : String := l.foldr (fun s r => (if r == "" then s else r ++ sep ++ s)) ""

def Var.format.typeOnly (var : Var) : String :=
  if var.sort == 'e' then
    s!"{var.sort}"
  else if var.sort == 'h' then
    s!"{var.sort}{var.id}"
  else
    s!"{var.sort}"

def Var.format.labelOnly (sentenceNumber : Nat) (var : Var) : String :=
  if var.sort == 'x' then
    s!"S{sentenceNumber}_{var.sort.toUpper}{var.id}"
  else
    s!"s{sentenceNumber}_{var.sort}{var.id}"

def Var.format.labelOnlyGround (sentenceNumber : Nat) (var : Var) : String :=
  s!"s{sentenceNumber}_{var.sort}{var.id}"

def Var.format.pair (sentenceNumber : Nat) (var : Var) : String :=
  if var.sort == 'x' then
    s!"S{sentenceNumber}_{var.sort.toUpper}{var.id} : {var.sort}"
  else if var.sort == 'e' then
    s!"s{sentenceNumber}_{var.sort}{var.id} : {var.sort}"
  else if var.sort == 'h' then 
    s!"s{sentenceNumber}_{var.sort}{var.id} : {var.sort}{var.id}"
  else
    unreachable!

def Var.format.labelWithDeps (sentenceNumber : Nat) (ep : EP) (var : Var) (qm : HashMap Var Var) (em : Multimap Var Var) : String :=
  let defaultExpr := 
    if var.sort == 'h' then
      Var.format.labelOnlyGround sentenceNumber var
    else
      Var.format.labelOnly sentenceNumber var
  match qm.find? var with
  | some iterVar => 
    match (em.find? var) with
    | some extraList => 
      let l := extraList.filter (fun evar => iterVar != evar)
      let l1 := (insertionSort l).eraseDups
      let l2 := l1.map (fun item => Var.format.labelOnly sentenceNumber item)
      let l2str := joinSep l2 " @ "
      let lab := Var.format.labelOnlyGround sentenceNumber var
      if l2str == "" then lab else "(" ++ lab ++ " @ " ++ l2str ++ ")" 
    | none => defaultExpr
  | none =>
    match (em.find? var) with
    | some extraList => 
      let l := (insertionSort extraList).eraseDups
      "(" ++ (Var.format.labelOnlyGround sentenceNumber var) ++ " @ " ++ (joinSep (l.map (fun item => Var.format.labelOnly sentenceNumber item)) " @ ") ++ ")"
    | none => defaultExpr

def EP.format.type (sentenceNumber : Nat) (qm : HashMap Var Var) (em : Multimap Var Var) (hm : Multimap Var EP) (rootHandle : Var) (handle : Var) : String :=
  let preds := match (hm.find? handle) with
  | some value => value
  | none => []

  let firstEp := match preds.head? with
  | some value => value
  | none => 
    dbg_trace "firstEP"; sorry

  let getArgs (ep : EP) : List (String × Var) :=
    -- let ret1 := if lastTwoChars ep.predicate == "_q" then
    --              ep.rargs.filter (fun item => item.1 != "ARG0") 
    --            else
    --              ep.rargs
    let ret1 := ep.rargs
    let ret2 := ret1.filter (fun item => item.2.sort == 'x' || item.2.sort == 'h' || item.2.sort == 'e') 
    ret2

  let extraArgs (qm : HashMap Var Var) (labelVar : Var) : String := 
    match (em.find? labelVar) with
    | some value => 
      let l := match (qm.find? labelVar) with
               | some larg => value.filter (fun arg => arg != larg)
               | none => value
      let ls : List Var := (insertionSort l).eraseDups
      let estr : String := (joinSep (ls.map (fun var => Var.format.typeOnly var)) " > ")
      estr
    | none => ""

  let printNormal (l : Var) (preds : List EP) : String :=
    let lookupArg (labelVar : Var) : String :=
      match (qm.find? labelVar) with
      | some value => Var.format.typeOnly value
      | none => ""
    let joinArgs0 (ep : EP) := joinSep ((getArgs ep).map fun a => Var.format.typeOnly a.2)  " > "
    let joinArgs (ep : EP) := 
      match ep.carg with
      | some str => joinArgs0 ep ++ " > name"
      | none => joinArgs0 ep
    let lstr := lookupArg l
    let estr := extraArgs qm l
    let combined := estr ++ (if lstr == "" then "" else (if estr == "" then "" else " > ") ++ lstr)
    let (lparen,rparen) := if preds.length > 1 then ("(",")") else ("","")
    let lab := if handle == rootHandle then s!"s{sentenceNumber}_root" else Var.format.labelOnlyGround sentenceNumber l
    "thf(" ++ lab ++ "_decl,type," ++ lab ++ ": " ++ combined ++ (if combined == "" then "" else " > ") ++ "$o)."
  printNormal firstEp.label preds

def EP.format.defn (sentenceNumber : Nat) (qm : HashMap Var Var) (em : Multimap Var Var) (hm : Multimap Var EP) (rootHandle : Var) (handle : Var) : String :=
  let preds := match (hm.find? handle) with
  | some value => value
  | none => []

  let firstEp := match preds.head? with
  | some value => value
  | none => 
    dbg_trace "firstEP"; sorry

  let getArgs (ep : EP) : List (String × Var) :=
    -- let ret1 := if lastTwoChars ep.predicate == "_q" then
    --              ep.rargs.filter (fun item => item.1 != "ARG0") 
    --            else
    --              ep.rargs
    let ret1 := ep.rargs
    let ret2 := ret1.filter (fun item => item.2.sort == 'x' || item.2.sort == 'h' || item.2.sort == 'e') 
    ret2

  let extraArgs (qm : HashMap Var Var) (labelVar : Var) : String := 
    match (em.find? labelVar) with
    | some value => 
      let l := match (qm.find? labelVar) with
               | some larg => value.filter (fun arg => arg != larg)
               | none => value
      let ls : List Var := (insertionSort l).eraseDups
      let estr : String := (joinSep (ls.map (fun var => Var.format.pair sentenceNumber var)) ",")
      estr
    | none => ""

  let printNormal (l : Var) (preds : List EP) : String :=
    let lookupArg (labelVar : Var) : String :=
      match (qm.find? labelVar) with
      | some value => Var.format.pair sentenceNumber value
      | none => ""
    let joinArgs0 (ep : EP) := joinSep ((getArgs ep).map fun a => Var.format.labelWithDeps sentenceNumber ep a.2 qm em)  " @ "
    let joinArgs (ep : EP) := 
      match ep.carg with
      | some str => joinArgs0 ep ++ " @ " ++ (formatId str)
      | none => joinArgs0 ep
    let lstr := lookupArg l
    let estr := extraArgs qm l
    let combined := estr ++ (if lstr == "" then "" else (if estr == "" then "" else ",") ++ lstr)
    let (lparen,rparen) := if preds.length > 1 then ("(",")") else ("","")
    let allCalls := preds.foldl (fun acc ep => (acc.1 ++ acc.2 ++ lparen ++ (fixName ep) ++ " @ " ++ (joinArgs ep) ++ rparen," & ")) ("","")
    let lab := if handle == rootHandle then s!"s{sentenceNumber}_root" else Var.format.labelOnlyGround sentenceNumber l
    if combined == "" then
      "thf(" ++ lab ++ ",definition," ++ "\n   " ++ lab ++ " = ((" ++ allCalls.1 ++ ")))."
    else
      "thf(" ++ lab ++ ",definition," ++ "\n   " ++ lab ++ " = ( ^ [" ++ combined ++ "] : " ++ "(" ++ allCalls.1 ++ ")))."

  printNormal firstEp.label preds

def MRS.format (sentenceNumber : Nat) (mrs : MRS.MRS) : (String × List String × List Var) :=
 let strings := mrs.preds.foldl (fun stab pred =>
  match pred with
  | {predicate := p, link := some (n,m), label := l, rargs := rs, carg := some c} =>
    stab.insert c l
  | {predicate := p, link := some (n,m), label := l, rargs := rs, carg := none} =>
    stab
  | {predicate := p, link := none, label := l, rargs := rs, carg := some c} =>
    stab.insert c l
  | {predicate := p, link := none, label := l, rargs := rs, carg := none} =>
    stab) Multimap.empty
 let eSet := collectEvents mrs.preds 
 let qm := collectQuantifierVars mrs.preds
 let em := collectHOExtraVarsForEPs mrs.preds $
           collectHOExtraVarsForEPs mrs.preds $
           collectHOExtraVarsForEPs mrs.preds $
           collectHOExtraVarsForEPs mrs.preds $ collectExtraVarsForEPs mrs.preds qm
 let hm := collectEPsByHandle mrs.preds
 let rlt := hm.keys.map (EP.format.type sentenceNumber qm em hm mrs.top) 
 let rla := hm.keys.map (EP.format.defn sentenceNumber qm em hm mrs.top) 
 let etypes := (joinSep (eSet.map (fun (var : Var) => s!"thf(s{sentenceNumber}_{var.sort}{var.id}_decl,type,(s{sentenceNumber}_{var.sort}{var.id} : e)).")) "\n")
 let str := etypes ++ "\n\n" ++ (joinSep rlt "\n") ++ "\n\n" ++ (joinSep rla "\n")
 (str,strings.keys,eSet)

end THF
