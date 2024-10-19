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

namespace HOF

open MRS (Var EP Constraint MRS)
open Lean (HashMap)
open MM

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
    if lastTwoChars ep.predicate == "_q" || ep.predicate == "_never_a_1" then
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
                               | none => acc.insert ep.label arg)
                              emin
    | none => emin
  if lastTwoChars ep.predicate == "_q" || ep.predicate == "_colon_p_namely" then
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
  else if ep.predicate == "_never_a_1" then
      match ep.rargs with
      | a :: b :: [] => 
        if a.1 == "ARG0" then
          add em b.2
        else
          add em a.2
      | _ => sorry
  else if ep.predicate == "neg" then
      match ep.rargs with
      | a :: b :: [] => 
        if a.1 == "ARG1" then
          add em a.2
        else if b.1 == "ARG1" then
          add em b.2
        else
          sorry
      | _ => sorry
  else
    em

def collectHOExtraVarsForEPs (preds : List EP) (em : Multimap Var Var) : Multimap Var Var :=
  preds.foldl augmentIndirect em

def collectEPsByHandle (preds : List EP) : Multimap Var EP :=
  preds.foldl (fun acc ep => acc.insert ep.label ep) Multimap.empty

def collectEvents (preds : List EP) : List Var :=
  let insertUnique (xs : List Var) (x : Var) : List Var :=
    if xs.contains x then xs else x :: xs
  let collectEventsForArgs (acc : List Var) (rs : List (String × Var)) : List Var := 
    rs.foldl (fun acc pair => if pair.2.sort == 'e' then insertUnique acc pair.2 else acc) acc
  preds.foldl (fun acc ep => collectEventsForArgs acc ep.rargs) []

end HOF
