import Mrs.Basic
import Lean.Data.HashMap

namespace MM

open Lean (HashMap)
open MRS (Var)

def Multimap := HashMap Var (List Var)

def mm.empty : Multimap := HashMap.empty

def mm.insert (m : Multimap) (k : Var) (v : Var) : Multimap :=
  match m.find? k with
  | some vs => m.insert k (v :: vs)
  | none    => m.insert k [v]

def mm.find? (m : Multimap) (k : Var) : Option (List Var) :=
  m.find? k

end MM

namespace THF

open MRS (Var EP Constraint MRS)
open Lean (HashMap)
open MM

def joinSep (l : List String) (sep : String) : String := l.foldr (fun s r => (if r == "" then s else r ++ sep ++ s)) ""

def Var.format.typeOnly (var : Var) : String :=
  match var with
  | {id := n, sort := s, props := #[]} =>
       if s == 'h' then
         s!"{s}{n}"
       else
         s!"{s}"
  | {id := n, sort := s, props := ps} =>
       if s == 'h' then
         s!"{s}{n}"
       else
         s!"{s}"

def Var.format.labelOnly (var : Var) : String :=
  match var with
  | {id := n, sort := s, props := #[]} =>
       s!"{s.toUpper}{n}"
  | {id := n, sort := s, props := ps} =>
       s!"{s.toUpper}{n}"

def Var.format.labelOnlyGround (var : Var) : String :=
  match var with
  | {id := n, sort := s, props := #[]} =>
       s!"{s}{n}"
  | {id := n, sort := s, props := ps} =>
       s!"{s}{n}"

def Var.format.labelWithDep (var : Var) (em : Multimap) : String :=
  match (mm.find? em var) with
  | some extraList => "(" ++ (Var.format.labelOnlyGround var) ++ " @ " ++ (joinSep (extraList.map (fun item => Var.format.labelOnly item)) " @ ") ++ ")"
  | none => 
    if var.sort == 'h' then
      Var.format.labelOnlyGround var
    else
      Var.format.labelOnly var

def Var.format.pair (var : Var) : String :=
  match var with
  | {id := n, sort := s, props := #[]} =>
       if s == 'h' then
         s!"{s.toUpper}{n} : {s}{n}"
       else
         s!"{s.toUpper}{n} : {s}"
  | {id := n, sort := s, props := ps} =>
       if s == 'h' then
         s!"{s.toUpper}{n} : {s}{n}"
       else
         s!"{s.toUpper}{n} : {s}"

def lastTwoChars (s : String) : String :=
  if s.length <= 1 then
    s
  else
    s.drop (s.length - 2)

def insertArgsForEP (hm : HashMap Var Var) (ep : EP) : HashMap Var Var :=
  if lastTwoChars ep.predicate == "_q" then
    match ep.rargs with
    | a :: b :: c :: [] => 
      if a.1 == "ARG0" then
        (hm.insert b.2 a.2).insert c.2 a.2
      else if b.1 == "ARG0" then
        (hm.insert a.2 b.2).insert c.2 b.2
      else 
        (hm.insert a.2 c.2).insert b.2 c.2
    | _ => unreachable!
  else
    hm

def collectVarsForEPs (preds : List EP) : HashMap Var Var :=
  preds.foldl (fun hmacc ep => insertArgsForEP hmacc ep) HashMap.empty

def collectExtraVarsForEPs (preds : List EP) (hm : HashMap Var Var) : Multimap :=
  let insertExtra (em : Multimap) (ep : EP) : Multimap :=
    if lastTwoChars ep.predicate == "_q" then
      em
    else
      ep.rargs.foldl (fun emac pair => 
         match (hm.find? ep.label) with
         | some value => 
           if pair.2.sort = 'x' && pair.2 != value then
             mm.insert emac ep.label pair.2
           else
             emac
         | none => emac) em
  preds.foldl insertExtra mm.empty

def EP.format.type (hm : HashMap Var Var) (em : Multimap) (ep : EP) : String :=
  let lookupArg (labelVar : Var) : String :=
    match (hm.find? labelVar) with
    | some value => (Var.format.typeOnly value) ++ " > "
    | none => ""
  let getArgs (rs : List (String × Var)) : List (String × Var) :=
    let ret1 := if lastTwoChars ep.predicate == "_q" then
                  rs.filter (fun item => item.1 != "ARG0") 
                else
                  rs
    ret1.filter (fun item => item.2.sort == 'x' || item.2.sort == 'h') 

  let extraArgs (labelVar : Var) : String := 
    match (mm.find? em labelVar) with
    | some value => (joinSep (value.map (fun var => Var.format.typeOnly var)) " > ") ++ " > "
    | none => ""

  match ep with
  | {predicate := p, link := some (n,m), label := l, rargs := rs, carg := some c} =>
    "thf(" ++ Var.format.labelOnlyGround l ++ "_decl,type," ++ (extraArgs l) ++ (lookupArg l) ++ "string > $o)."
  | {predicate := p, link := some (n,m), label := l, rargs := rs, carg := none} =>
    "thf(" ++ Var.format.labelOnlyGround l ++ "_decl,type," ++ (extraArgs l) ++ (lookupArg l) ++ "$o)."
  | {predicate := p, link := none, label := l, rargs := rs, carg := some c} =>
    "thf(" ++ Var.format.labelOnlyGround l ++ "_decl,type," ++ (extraArgs l) ++ (lookupArg l) ++ "string > $o)."
  | {predicate := p, link := none, label := l, rargs := rs, carg := none} =>
    "thf(" ++ Var.format.labelOnlyGround l ++ "_decl,type," ++ (extraArgs l) ++ (lookupArg l) ++ "$o)."


def EP.format.axiom (hm : HashMap Var Var) (em : Multimap) (ep : EP) : String :=
  let lookupArg (labelVar : Var) : String :=
    match (hm.find? labelVar) with
    | some value => Var.format.labelOnly value
    | none => ""

  let getArgs (rs : List (String × Var)) : List (String × Var) :=
    let ret1 := if lastTwoChars ep.predicate == "_q" then
                  rs.filter (fun item => item.1 != "ARG0") 
                else
                  rs
    ret1.filter (fun item => item.2.sort == 'x' || item.2.sort == 'h') 

  let extraArgs (labelVar : Var) : String := 
    match (mm.find? em labelVar) with
    | some value => (joinSep (value.map (fun var => Var.format.labelOnly var)) ",") ++ ","
    | none => ""

  match ep with
  | {predicate := p, link := some (n,m), label := l, rargs := rs, carg := some c} =>
    let args := joinSep ((getArgs rs).map fun a => Var.format.labelWithDep a.2 em)  " @ "
    "thf(" ++ Var.format.labelOnlyGround l ++ ",axiom," ++ "\n   " ++ Var.format.labelOnlyGround l ++ " = ( ^ [" ++ (extraArgs l) ++ (lookupArg l) ++ ",'" ++ s!"{c}" ++ "'] : (" ++ p ++ " @ " ++ args ++ ")))."
  | {predicate := p, link := some (n,m), label := l, rargs := rs, carg := none} =>
    let args := joinSep ((getArgs rs).map fun a => Var.format.labelWithDep a.2 em)  " @ "
    "thf(" ++ Var.format.labelOnlyGround l ++ ",axiom," ++ "\n   " ++ Var.format.labelOnlyGround l ++ " = ( ^ [" ++ (extraArgs l) ++ (lookupArg l) ++ "] : (" ++ p ++ " @ " ++ args ++ ")))."
  | {predicate := p, link := none, label := l, rargs := rs, carg := some c} =>
    let args := joinSep ((getArgs rs).map fun a => Var.format.labelWithDep a.2 em)  " @ "
    "thf(" ++ Var.format.labelOnlyGround l ++ ",axiom," ++ "\n   " ++ Var.format.labelOnlyGround l ++ " = ( ^ [" ++ (extraArgs l) ++ (lookupArg l) ++ ",'" ++ s!"{c}" ++ "'] : (" ++ p ++ " @ " ++ args ++ ")))."
  | {predicate := p, link := none, label := l, rargs := rs, carg := none} =>
    let args := joinSep ((getArgs rs).map fun a => Var.format.labelWithDep a.2 em)  " @ "
    "thf(" ++ Var.format.labelOnlyGround l ++ ",axiom," ++ "\n   " ++ Var.format.labelOnlyGround l ++ " = (^ [" ++ (extraArgs l) ++ (lookupArg l) ++ "] : (" ++ p ++ " @ " ++ args ++ ")))."

def MRS.format (mrs : MRS.MRS) : String :=
 let header0 := "thf(x_decl,type,x : $tType)."
 let header1 := "thf(e_decl,type,e : $tType)."
 let headers := header0 ++ "\n" ++ header1 ++ "\n"
 let printIt ps :=
   let hm := collectVarsForEPs ps
   let em := collectExtraVarsForEPs ps hm
   let rlt := List.map (EP.format.type hm em) ps
   let rla := List.map (EP.format.axiom hm em) ps
   headers ++ (joinSep rlt "\n") ++ "\n" ++ (joinSep rla "\n")
 match mrs with
 | {top := t, index := i, preds := ps, icons := [], hcons := hs} => printIt ps
 | {top := t, index := i, preds := ps, icons := is, hcons := hs} => printIt ps

end THF


