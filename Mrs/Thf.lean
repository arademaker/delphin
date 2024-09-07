import Lean.Data.HashMap
import Lean.Data.RBMap
import Mrs.Basic

namespace MM

open Lean (RBMap)

structure Multimap (α β : Type) [BEq α] [Ord α] where
  map : RBMap α (List β) compare

class MultimapMethods (α β : Type) [BEq α] [Ord α] where
  empty : Multimap α β
  insert : Multimap α β → α → β → Multimap α β
  find? : Multimap α β → α → Option (List β)
  keys : Multimap α β → List α

instance [BEq α] [Ord α] : MultimapMethods α β where
  empty := ⟨RBMap.empty⟩
  insert m k v :=
    let newValues := match m.map.find? k with
      | some list => v :: list
      | none => [v]
    ⟨m.map.insert k newValues⟩
  find? m k := m.map.find? k
  keys m := m.map.toList.map Prod.fst

def Multimap.empty [BEq α] [Ord α] : Multimap α β := MultimapMethods.empty
def Multimap.insert [BEq α] [Ord α] (m : Multimap α β) (k : α) (v : β) : Multimap α β := MultimapMethods.insert m k v
def Multimap.find? [BEq α] [Ord α] (m : Multimap α β) (k : α) : Option (List β) := MultimapMethods.find? m k
def Multimap.keys [BEq α] [Ord α] (m : Multimap α β) : List α := MultimapMethods.keys m

end MM

namespace THF

open MRS (Var EP Constraint MRS)
open Lean (HashMap)
open MM

def libraryRoutines : String := 
  "thf(every_q_decl,type,a_q: (x > $o) > (x > $o) > $o).\n" ++
  "thf(the_q_decl,type,the_q: (x > $o) > (x > $o) > $o).\n" ++
  "thf(proper_q_decl,type,proper_q: (x > $o) > (x > $o) > $o).\n" ++
  "thf(udef_q_decl,type,udef_q: (x > $o) > (x > $o) > $o).\n" ++
  "\n" ++ 
  -- "thf(book_n_of_decl,type,book_n_of: x > $o).\n" ++
  -- "thf(love_v_1_decl,type,love_v_1: x > x > $o).\n" ++
  -- "thf(a_q_decl,type,every_q: (x > $o) > (x > $o) > $o).\n" ++ 
  -- "thf(boy_n_1_decl,type,boy_n_1: x > $o).\n"  ++

  "thf(therein_p_dir_decl,type,therein_p_dir: e > e > $o).\n" ++
  "thf(live_v_1_decl,type,live_v_1: e > x > $o).\n" ++
  "thf(people_n_of_decl,type,people_n_of: x).\n" ++
  "thf(only_a_1_decl,type,only_a_1: e > x > $o).\n" ++
  "thf(named_decl,type,named: x > string > $o).\n" ++
  "thf(and_c_decl,type,and_c: x > x > x > $o).\n" ++
  "thf(butler_n_1_decl,type,butler_n_1: x > $o).\n" ++
  "thf(implicit_conj_decl,type,implicit_conj: x > x > x > $o).\n" ++
  "thf(be_v_id_decl,type,be_v_id: e > x > x > $o).\n" ++
  "thf(in_p_loc_decl,type,in_p_loc: e > e > x > $o).\n" ++
  "thf(live_v_1_decl,type,live_v_1: e > x > $o).\n" ++
  "thf(compound_decl,type,compound: e > x > x > $o).\n" 

def insert [Ord α] (x : α) : List α → List α
  | [] => [x]
  | y :: ys => if Ord.compare x y == .lt || Ord.compare x y == .eq 
                then x :: y :: ys 
                else y :: insert x ys

def insertionSort [Ord α] : List α → List α
  | [] => []
  | x :: xs => insert x (insertionSort xs)

def joinSep (l : List String) (sep : String) : String := l.foldr (fun s r => (if r == "" then s else r ++ sep ++ s)) ""

def Var.format.typeOnly (var : Var) : String :=
  if var.sort == 'e' then
    s!"{var.sort}"
  else if var.sort == 'h' then
    s!"{var.sort}{var.id}"
  else
    s!"{var.sort}"

def Var.format.labelOnly (var : Var) : String :=
  if var.sort == 'x' then
    s!"{var.sort.toUpper}{var.id}"
  else
    s!"{var.sort}{var.id}"

def Var.format.labelOnlyGround (var : Var) : String :=
  s!"{var.sort}{var.id}"

def Var.format.pair (var : Var) : String :=
  if var.sort == 'x' then
    s!"{var.sort.toUpper}{var.id} : {var.sort}"
  else if var.sort == 'e' then
    s!"{var.sort}{var.id} : {var.sort}"
  else if var.sort == 'h' then 
    s!"{var.sort}{var.id} : {var.sort}{var.id}"
  else
    unreachable!

def Var.format.labelWithDeps (ep : EP) (var : Var) (qm : HashMap Var Var) (em : Multimap Var Var) : String :=
  let defaultExpr := 
    if var.sort == 'h' then
      Var.format.labelOnlyGround var
    else
      Var.format.labelOnly var
  match qm.find? var with
  | some iterVar => 
    match (em.find? var) with
    | some extraList => 
      let l := extraList.filter (fun evar => iterVar != evar)
      let l1 := (insertionSort l).eraseDups
      let l2 := l1.map (fun item => Var.format.labelOnly item)
      let l2str := joinSep l2 " @ "
      let lab := Var.format.labelOnlyGround var
      if l2str == "" then lab else "(" ++ lab ++ " @ " ++ l2str ++ ")" 
    | none => defaultExpr
  | none =>
    match (em.find? var) with
    | some extraList => "(" ++ (Var.format.labelOnlyGround var) ++ " @ " ++ (joinSep (extraList.map (fun item => Var.format.labelOnly item)) " @ ") ++ ")"
    | none => defaultExpr

def Var.format.all (var : Var) : String :=
  s!"{var.sort}{var.id}{var.props}"

def lastTwoChars (s : String) : String :=
  if s.length <= 1 then
    s
  else
    s.drop (s.length - 2)

def insertArgsForEP (qm : HashMap Var Var) (ep : EP) : HashMap Var Var :=
  if lastTwoChars ep.predicate == "_q" then
    match ep.rargs with
    | a :: b :: c :: [] => 
      if a.1 == "ARG0" then
        (qm.insert b.2 a.2).insert c.2 a.2
      else if b.1 == "ARG0" then
        (qm.insert a.2 b.2).insert c.2 b.2
      else 
        (qm.insert a.2 c.2).insert b.2 c.2
    | _ => unreachable!
  else
    qm 

def collectQuantifierVars (preds : List EP) : HashMap Var Var :=
  preds.foldl (fun hmacc ep => insertArgsForEP hmacc ep) HashMap.empty

def collectExtraVarsForEPs (preds : List EP) (qm : HashMap Var Var) : Multimap Var Var :=
  let insertExtra (em : Multimap Var Var) (ep : EP) : Multimap Var Var :=
    if lastTwoChars ep.predicate == "_q" then
      em
    else
      let add (emac : Multimap Var Var) (pair : (String × Var)) : Multimap Var Var :=
        match emac.find? ep.label with
          | some l => if l.contains pair.2 then emac else emac.insert ep.label pair.2
          | none => emac.insert ep.label pair.2
      let condAdd (emac : Multimap Var Var) (pair : (String × Var)) : Multimap Var Var :=
         match (qm.find? ep.label) with
         | some value => 
             (if pair.2.sort == 'x' then --  && pair.2 != value then 
               add emac pair
              else
                emac)
         | none => 
             (if pair.2.sort == 'x' then
               add emac pair
             else
               emac)
      ep.rargs.foldl condAdd em
  preds.foldl insertExtra Multimap.empty

def augmentIndirect (em : Multimap Var Var) (ep : EP) : Multimap Var Var :=
  let add (emin : Multimap Var Var) (var : Var) : Multimap Var Var := 
    match (emin.find? var) with
    | some vals => vals.foldl (fun acc arg => 
                               match (acc.find? arg) with
                               | some _ => acc
                               | none => dbg_trace ("Augmenting " ++ (Var.format.pair ep.label) ++ " with " ++ (Var.format.pair arg));
                                         acc.insert ep.label arg)
                              emin
    | none => emin
  if lastTwoChars ep.predicate == "_q" then
    match ep.rargs with
    | a :: b :: c :: [] => 
      if a.1 == "ARG0" then
        add (add em b.2) c.2
      else if b.1 == "ARG0" then
        add (add em a.2) c.2
      else 
        add (add em a.2) b.2
    | _ => 
        dbg_trace "augmentIndirect" ;
        sorry
  else
    em

def collectHOExtraVarsForEPs (preds : List EP) (em : Multimap Var Var) : Multimap Var Var :=
  preds.foldl augmentIndirect em
  
def EP.format.type (qm : HashMap Var Var) (em : Multimap Var Var) (ep : EP) : String :=
  let lookupArg (labelVar : Var) : String :=
    match (qm.find? labelVar) with
    | some value => Var.format.typeOnly value
    | none => ""

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

  let lstr := lookupArg ep.label
  let estr := extraArgs qm ep.label
  let combined := (if estr == "" then estr else estr ++ " > ") ++ (if lstr == "" then lstr else lstr ++ " > ")
  
  match ep with
  | {predicate := p, link := some (n,m), label := l, rargs := rs, carg := some c} =>
    "thf(" ++ Var.format.labelOnlyGround l ++ "_decl,type," ++ Var.format.labelOnlyGround l ++ ": " ++ combined ++ "string > $o)."
  | {predicate := p, link := some (n,m), label := l, rargs := rs, carg := none} =>
    "thf(" ++ Var.format.labelOnlyGround l ++ "_decl,type," ++ Var.format.labelOnlyGround l ++ ": " ++ combined ++ "$o)."
  | {predicate := p, link := none, label := l, rargs := rs, carg := some c} =>
    "thf(" ++ Var.format.labelOnlyGround l ++ "_decl,type," ++ Var.format.labelOnlyGround l ++ ": " ++ combined ++ "string > $o)."
  | {predicate := p, link := none, label := l, rargs := rs, carg := none} =>
    "thf(" ++ Var.format.labelOnlyGround l ++ "_decl,type," ++ Var.format.labelOnlyGround l ++ ": " ++ combined ++ "$o)."


def EP.format.axiom (qm : HashMap Var Var) (em : Multimap Var Var) (hm : Multimap Var EP) (handle : Var) : String :=
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
      let estr : String := (joinSep (ls.map (fun var => Var.format.pair var)) ",")
      estr
    | none => ""

  let fixName (PredName : String) : String :=
    match (PredName.get? 0) with
      | '_' => PredName.drop 1
      | _ => PredName

  let printNormal (l : Var) (preds : List EP) : String :=
    let lookupArg (labelVar : Var) : String :=
      match (qm.find? labelVar) with
      | some value => Var.format.pair value
      | none => ""
    let joinArgs0 (ep : EP) := joinSep ((getArgs ep).map fun a => Var.format.labelWithDeps ep a.2 qm em)  " @ "
    let joinArgs (ep : EP) := 
      match ep.carg with
      | some str => joinArgs0 ep ++ " @ " ++ str
      | none => joinArgs0 ep
    let lstr := lookupArg l
    let estr := extraArgs qm l
    let combined := estr ++ (if lstr == "" then "" else (if estr == "" then "" else ",") ++ lstr)
    if combined == "" then
      "thf(" ++ Var.format.labelOnlyGround l ++ ",axiom," ++ "\n   " ++ Var.format.labelOnlyGround l ++ " = (" ++ fixName (firstEp.predicate) ++ " @ " ++ (joinArgs firstEp)  ++ "))."
    else
      let (lparen,rparen) := if preds.length > 1 then ("(",")") else ("","")
      let allCalls := preds.foldl (fun acc ep => (acc.1 ++ acc.2 ++ lparen ++ (fixName ep.predicate) ++ " @ " ++ (joinArgs ep) ++ rparen," & ")) ("","")
      "thf(" ++ Var.format.labelOnlyGround l ++ ",axiom," ++ "\n   " ++ Var.format.labelOnlyGround l ++ " = ( ^ [" ++ combined ++ "] : " ++ "(" ++ allCalls.1 ++ ")))."

  printNormal firstEp.label preds

def collectEPsByHandle (preds : List EP) : Multimap Var EP :=
  preds.foldl (fun acc ep => acc.insert ep.label ep) Multimap.empty

def collectEvents (preds : List EP) : List Var :=
  let insertUnique (xs : List Var) (x : Var) : List Var :=
    if xs.contains x then xs else x :: xs
  let collectEventsForArgs (acc : List Var) (rs : List (String × Var)) : List Var := 
    rs.foldl (fun acc pair => if pair.2.sort == 'e' then insertUnique acc pair.2 else acc) acc
  preds.foldl (fun acc ep => collectEventsForArgs acc ep.rargs) []

def MRS.format (mrs : MRS.MRS) : String :=
 let header0 := "thf(x_decl,type,x : $tType)."
 let header1 := "thf(e_decl,type,e : $tType)."
 let header2 := "thf(string_decl,type,string : $i)."
 let headers := header0 ++ "\n" ++ header1 ++ header2 ++ "\n\n" ++ libraryRoutines ++ "\n"
 let eSet := collectEvents mrs.preds 
 let qm := collectQuantifierVars mrs.preds
 let em := collectHOExtraVarsForEPs mrs.preds $ collectHOExtraVarsForEPs mrs.preds $ collectHOExtraVarsForEPs mrs.preds $ collectExtraVarsForEPs mrs.preds qm
 let hm := collectEPsByHandle mrs.preds
 let rlt := (List.map (EP.format.type qm em) mrs.preds).eraseDups
 let rla := List.map (EP.format.axiom qm em hm) hm.keys 
 let etypes := (joinSep (eSet.map (fun (var : Var) => s!"thf({var.sort}{var.id}_decl,type,({var.sort}{var.id} : $int)).")) "\n")
 let eaxioms := (joinSep (eSet.map (fun (var : Var) => s!"thf({var.sort}{var.id}_value,axiom,({var.sort}{var.id} = {var.id})).")) "\n")
 headers ++ etypes ++ "\n" ++ eaxioms ++ "\n" ++ (joinSep rlt "\n") ++ "\n" ++ (joinSep rla "\n")

end THF


