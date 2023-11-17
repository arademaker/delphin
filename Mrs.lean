
import Lean.Data.Parsec
open Lean Parsec

/- the data structure
   https://github.com/delph-in/docs/wiki/MrsRFC
-/

structure Var where
 name  : String
 sort  : Option String
 props : List (String × String)
deriving Repr

structure Handle where
 name : String
deriving Repr

structure Constraint where
  lhs : Var
  rhs : Var
deriving Repr

structure EP where
  predicate : String
  label : Handle
  link  : Option (Int × Int)
  rargs : List (String × Var)
  carg  : Option String
deriving Repr

structure MRS where
  top : Handle
  index : Var
  preds : List EP
  icons : List Constraint
  hcons : List Constraint
deriving Repr


/- the parser
   defined from the BNF https://github.com/delph-in/docs/wiki/MrsRFC#simple
-/

def Array.asString (a : Array Char) : String :=
  Array.foldl (λ s c => s ++ c.toString) "" a

def pVariable : Parsec String := do
  let p ← asciiLetter
  let n ← many (satisfy $ fun c => c.isAlpha)
  let s ← many1 digit
  return (#[p] ++ n ++ s).asString

def VarSort : Parsec String := do
  let p ← asciiLetter
  let s ← many asciiLetter
  return (#[p] ++ s).asString


def integer : Parsec Int := do
 let a ← (pchar '-' >>= (fun a => manyCore digit #[a])) <|> (many digit)
 return a.asString.toInt!

def Lnk : Parsec (Int × Int) := do
  let _ ← pchar '<'
  let a ← integer
  let _ ← pchar ':'
  let b ← integer
  let _ ← pchar '>'
  return (a, b)

def Label : Parsec Var := do
  let _ ← pstring "LBL: "
  let v ← pVariable
  return (Var.mk v)

def letter : Parsec Char := satisfy fun c =>
  0x41 ≤ c.val ∧ c.val ≤ 0x5A ∨
  0x61 ≤ c.val ∧ c.val ≤ 0x7A

def identifier : Parsec Var := do
  let p ← letter
  let a ← many $ letter <|> pchar '-'
  let b ← many digit
  return (Var.mk (Array.asString $ #[p] ++ a) (b.asString).toNat!)

-- new version

def parseVarName : Parsec String := do
  let p ← asciiLetter
  let n ← many (satisfy $ fun c => c.isAlpha)
  let s ← many1 digit
  return (#[p] ++ n ++ s).asString

def parsePath : Parsec String := do
  let p ← asciiLetter
  let s ← many (satisfy $ fun c => c.isAlphanum)
  return (#[p] ++ s).asString

def parseToken : Parsec String := do
  let p ← many1 $ satisfy $ fun c =>
    "\\:]>".data.notElem c ∧ ¬ c.isWhitespace
  return p.asString

def parseQuotedString : Parsec String := do
  let a1 ← pchar '"'
  let a2 ← many $ satisfy $ fun c => ['"', '\\'].notElem c
  let a3 ← many (do
    let a ← pchar '\\'
    let b ← (satisfy $ fun c => c ≠ '\n')
    let c ← many $ satisfy $ fun c => ['"', '\\'].notElem c
    return (#[a,b] ++ c).asString)
  let a4 ← pchar '"'
  return (a1.toString ++ a2.asString ++
    (Array.foldl (λ s c => s ++ c) "" a3) ++ a4.toString)

def parseVarProps : Parsec (String × Array (String × String)) := do
  let _ ← pchar '['
  let varSort ← (do
     let p ← asciiLetter
     let n ← many (satisfy $ fun c => c.isAlpha)
     return (#[p] ++ n).asString)
  let extraPairs ← many (do
    let p ← parsePath
    let v ← parseToken <|> parseQuotedString
    return (p, v))
  let _ ← pchar ']'
  return (varSort, extraPairs)

def parseVar : Parsec Var := do
  let nm ← parseVarName
  let varProps ← parseVarProps <|> pure none
  pure $ Var.mk nm (some varProps.1) varProps.2

def parseHandle : Parsec Var := do
  let p ← parseVarName
  return { name := p, sort := some "h", props := [] : Var }

def parseTop : Parsec Var :=
  pstring "LTOP: " *> parseHandle

def parseIndex : Parsec Var :=
  pstring "INDEX: " *> parseVar

def parseMRS : Parsec MRS := do
    let _ ← pchar '['
    let top ← parseTop
    let index ← parseIndex
    let rels ← parseRels
    let icons ← parseIcons
    let hcons ← parseHcons
    let _ ← pchar ']'
    pure $ MRS.mk top index rels hcons
