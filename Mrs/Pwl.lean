import Lean.Data.HashMap
import Mrs.Basic
import Mrs.Hof
import Util.InsertionSort

namespace PWL

open Lean (HashMap)
open MRS (Var EP Constraint MRS)
open HOF
open MM
open InsertionSort

def joinSep (sep : String) (l : List String) : String := 
  l.foldr (fun s r => (if r == "" then s else r ++ sep ++ s)) ""

def removeQuotes (s : String) : String :=
  if s.startsWith "\"" && s.endsWith "\"" then s.extract ⟨1⟩ ⟨s.length - 1⟩ else s

def formatId (s : String) : String :=
  s!"\"{removeQuotes s}\""

def fixName (predicate : String) : String :=
  if predicate == "_neg" || predicate == "neg" then
    "neg"
  else if predicate.startsWith "_" then 
    predicate.drop 1 
  else 
    predicate

def Var.format.pwlVar (var : Var) : String :=
  match var.sort with
  | 'x' => s!"X{var.id}"
  | 'e' => s!"e{var.id}"
  | 'h' => s!"H{var.id}"
  | _ => s!"{var.sort}{var.id}"

def getVarsForScope (preds : List EP) (handle : Var) (em : Multimap Var Var) : List Var :=
  let directVars := preds.foldl (fun acc ep =>
    acc ++ (ep.rargs.filter (fun arg => arg.2.sort == 'x')).map (fun arg => arg.2)
  ) []
  match em.find? handle with
  | some extraVars => insertionSort (directVars ++ extraVars) |>.eraseDups
  | none => insertionSort directVars |>.eraseDups

mutual

partial def expandHandle (
    sentenceNumber : Nat)
    (qm : HashMap Var Var)
    (em : Multimap Var Var)
    (hm : Multimap Var EP)
    (visited : Array Var)
    (h : Var) : String :=
  match (hm.find? h) with
  | some hPreds =>
    dbg_trace s!"DEBUG HANDLE CONTENT: Handle {Var.format.pwlVar h} contains {(hPreds.map (·.predicate)).toString}"
    format_definition sentenceNumber qm em hm visited h none
  | none => 
    dbg_trace s!"DEBUG HANDLE MISSING: No content for handle {Var.format.pwlVar h}"
    s!"_{Var.format.pwlVar h}"

partial def formatPredicate (
    sentenceNumber : Nat)
    (qm : HashMap Var Var)
    (em : Multimap Var Var)
    (hm : Multimap Var EP)
    (visited : Array Var)
    (ep : EP)
    (replacement : Option (String × String)) : String :=
  let name := fixName ep.predicate
  if name == "be_v_id" then
    match ep.rargs with
    | (_,_)::(_, x1)::(_, x2)::_ =>
      s!"?[b]:(same(b) & arg1(b)={Var.format.pwlVar x1} & arg2(b)={Var.format.pwlVar x2})"
    | _ => s!"{name}({joinSep ", " (ep.rargs.map (fun arg => Var.format.pwlVar arg.2))})"
  else if name == "neg" then
    match ep.rargs with
    | (_, h)::(_, e)::_ =>
      dbg_trace s!"DEBUG NEG: Handle {Var.format.pwlVar h} Event {Var.format.pwlVar e}"
      match (hm.find? h) with
      | some hPreds =>
        dbg_trace s!"DEBUG NEG CONTENT: Handle {Var.format.pwlVar h} contains {(hPreds.map (·.predicate)).toString}"
        let handleContent := format_definition sentenceNumber qm em hm visited h replacement
        dbg_trace s!"DEBUG NEG EXPANDED: {handleContent}"
        s!"~({handleContent})"
      | none => 
        dbg_trace s!"DEBUG NEG MISSING: No content for handle {Var.format.pwlVar h}"
        s!"~(_)"
    | _ => "~(error)"
  else
    let args := ep.rargs.map fun arg => 
      if arg.2.sort == 'h' then
        dbg_trace s!"DEBUG HANDLE ARG: Expanding handle {Var.format.pwlVar arg.2} in {name}"
        s!"({expandHandle sentenceNumber qm em hm visited arg.2})"
      else
        match replacement with
        | none => Var.format.pwlVar arg.2
        | some pair => 
            if Var.format.pwlVar arg.2 == pair.1 then pair.2
            else Var.format.pwlVar arg.2
    match ep.carg with
    | some carg => s!"{name}({joinSep ", " (args ++ [formatId carg])})"
    | none => s!"{name}({joinSep ", " args})"

partial def format_definition (
    sentenceNumber : Nat)
    (qm : HashMap Var Var)
    (em : Multimap Var Var)
    (hm : Multimap Var EP)
    (visited : Array Var)
    (handle : Var)
    (replacement : Option (String × String)) : String :=
  if visited.contains handle then
    s!"/* Cycle detected at {Var.format.pwlVar handle} */"
  else
    match (hm.find? handle) with
    | none => ""
    | some preds =>
      match preds.find? (fun p => p.predicate == "neg") with
      | some negPred =>
        match negPred.rargs with
        | (_, h)::(_, e)::_ =>
          dbg_trace s!"DEBUG NEG: Handle {Var.format.pwlVar h} Event {Var.format.pwlVar e}"
          match (hm.find? h) with
          | some hPreds =>
            dbg_trace s!"DEBUG NEG CONTENT: Handle {Var.format.pwlVar h} contains {(hPreds.map (·.predicate)).toString}"
            let handleContent := format_definition sentenceNumber qm em hm visited h replacement
            dbg_trace s!"DEBUG NEG EXPANDED: {handleContent}"
            s!"~({handleContent})"
          | none => 
            dbg_trace s!"DEBUG NEG MISSING: No content for handle {Var.format.pwlVar h}"
            s!"~(_)"
        | _ => "~(error)"
      | none =>
        match preds.head? with
        | none => sorry
        | some firstEp =>
          let visited' := visited.push handle
          if firstEp.predicate.endsWith "_q" then
            let name := fixName firstEp.predicate
            let rstr := firstEp.rargs.find? (·.1 == "RSTR") 
            let body := firstEp.rargs.find? (·.1 == "BODY")
            let boundVar := firstEp.rargs.find? (·.1 == "ARG0")
            match rstr, body, boundVar with
            | some (_, rvar), some (_, bvar), some (_, xvar) =>
              let bv := Var.format.pwlVar xvar
              let rstrContent := format_definition sentenceNumber qm em hm visited' rvar (if name == "the_q" then some (bv, "s") else none)
              let bodyContent := format_definition sentenceNumber qm em hm visited' bvar none
              match (hm.find? rvar), (hm.find? bvar) with
              | some rstrPreds, some bodyPreds =>
                let rstrVars := getVarsForScope rstrPreds rvar em
                let bodyVars := getVarsForScope bodyPreds bvar em
                if name == "the_q" then
                  s!"?[S]:(S=^[s]:{rstrContent} & size(S)=1 & S({bv}) & {bodyContent})"
                else if name == "proper_q" then
                  if rstrContent.startsWith "named" then 
                    let args := rstrContent.drop 6
                    let argsNoClose := args.dropRight 1
                    let parts := String.splitOn argsNoClose ","
                    match parts with
                    | [s, x] => 
                      s!"?[n]:(name(n) & arg1(n)={String.trim x} & arg2(n)={String.trim s}) & {bodyContent}"
                    | _ => s!"{name}({bv}, {rstrContent}, {bodyContent})"
                  else
                    s!"{name}({bv}, {rstrContent}, {bodyContent})"
                else
                  s!"{name}({bv}, {rstrContent}, {bodyContent})"
              | _, _ => s!"{name}({bv}, {rstrContent}, {bodyContent})"
            | _, _, _ => "error: malformed quantifier"
          else
            let nonQuantPreds := preds.filter (fun p => !(p.predicate.endsWith "_q"))
            let formatted := nonQuantPreds.map (formatPredicate sentenceNumber qm em hm visited' · replacement)
            joinSep " & " formatted

end

def collectEPsByHandle (preds : List EP) : Multimap Var EP := 
  let initial := preds.foldl (fun acc ep => acc.insert ep.label ep) Multimap.empty
  dbg_trace s!"DEBUG HANDLES: Full handle map: {(initial.keys.map Var.format.pwlVar).toString}"
  initial

def MRS.format (sentenceNumber : Nat) (mrs : MRS.MRS) : (String × List String × List Var) :=
  let strings := mrs.preds.foldl (fun stab pred =>
    match pred with
    | {predicate := _, link := some (_, _), label := l, rargs := _, carg := some c} => stab.insert c l
    | {predicate := _, link := some (_, _), label := l, rargs := _, carg := none} => stab
    | {predicate := _, link := none, label := l, rargs := _, carg := some c} => stab.insert c l
    | {predicate := _, link := none, label := l, rargs := _, carg := none} => stab) Multimap.empty

  let qm := collectQuantifierVars mrs.preds
  let em := collectHOExtraVarsForEPs mrs.preds $
            collectHOExtraVarsForEPs mrs.preds $
            collectHOExtraVarsForEPs mrs.preds $
            collectHOExtraVarsForEPs mrs.preds $ 
            collectExtraVarsForEPs mrs.preds qm
  let hm := collectEPsByHandle mrs.preds

  let eSet := collectEvents mrs.preds
  match (hm.find? mrs.top) with
  | some preds =>
    let topVars := getVarsForScope preds mrs.top em
    let varStr := joinSep "," (topVars.map Var.format.pwlVar)
    let mainContent := format_definition sentenceNumber qm em hm #[] mrs.top none
    (s!"?[{varStr}]:({mainContent})", strings.keys, eSet)
  | none => ("error: no content", strings.keys, eSet)

end PWL
