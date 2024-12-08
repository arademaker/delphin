import Lean.Data.HashMap
import Mrs.Basic
import Mrs.Hof
import Util.InsertionSort

namespace PWL

open Lean (HashMap)
open MRS (Var EP Constraint MRS)
open HOF
open MM (Multimap)
open InsertionSort

/-- Extract numeric part of ARG label -/
def getArgNum (label : String) : Option Nat :=
  let num := label.dropWhile (fun c => c != '0' && c != '1' && c != '2' && c != '3' && c != '4' && c != '5')
  if num == "" then none
  else some num.toNat!

/-- Compare ARG labels for ordering -/
def compareArgs (a b : String × Var) : Ordering :=
  match getArgNum a.1, getArgNum b.1 with
  | some n1, some n2 => if n1 ≤ n2 then .lt else .gt
  | some _, none => .lt
  | none, some _ => .gt
  | none, none => .eq

instance : Ord (String × Var) where
  compare := compareArgs

def getArg (ep: EP) (name: String) : Option Var :=
  ep.rargs.find? (fun r => r.1 == name)
  |>.map (fun r => r.2)

def joinSep (sep : String) (l : List String) : String := 
  l.foldr (fun s r => (if r == "" then s else r ++ sep ++ s)) ""

def joinComma (l : List String) : String := joinSep "," l

def removeQuotes (s : String) : String :=
  if s.startsWith "\"" && s.endsWith "\"" then s.extract ⟨1⟩ ⟨s.length - 1⟩ else s

def formatId (s : String) : String :=
  s!"\"{removeQuotes s}\""

def reformQuotedPair (s : String) : String :=
  let parts := String.split s (· == ' ')
  let unquoted := parts.map (fun s => 
    if String.startsWith s "\"" && String.endsWith s "\"" then
      String.dropRight (String.drop s 1) 1
    else s)
  "\"" ++ " ".intercalate unquoted ++ "\""

def fixName (predicate : String) : String :=
  if predicate == "_neg" || predicate == "neg" then
    "neg"
  else if predicate.startsWith "_" then 
    predicate.drop 1 
  else 
    predicate

def collectEPsByHandle (preds : List EP) : Multimap Var EP := 
  let initial := preds.foldl (fun acc ep => acc.insert ep.label ep) Multimap.empty
  dbg_trace s!"DEBUG HANDLES: Full handle map: {(initial.keys.map (fun h => s!"{h.sort}{h.id}")).toString}"
  initial

end PWL

export PWL (joinComma joinSep)
