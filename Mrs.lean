
import Lean.Data.Parsec
open Lean Parsec

/- the data structure
   https://github.com/delph-in/docs/wiki/MrsRFC
-/

structure Var where
 name  : String
 sort  : Option String
 props : Array (String × String)
deriving Repr

structure Constraint where
  lhs : Var
  rhs : Var
deriving Repr

structure EP where
  predicate : String
  label : Var
  link  : Option (Int × Int)
  rargs : List (String × Var)
  carg  : Option String
deriving Repr

structure MRS where
  top : Var
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

def parseSpace : Parsec String := do
  let a ← many (satisfy $ fun c => c.isWhitespace)
  return a.asString

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

def parseTypePred : Parsec String := do
 let aux : Char → Bool := fun c => ("[]<>:\"".data.notElem c ∧ ¬ c.isWhitespace)
 let p ← many1 $ satisfy aux
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
  let _ ← pstring "[" *> parseSpace
  let varSort ← (do
     let p ← asciiLetter
     let n ← many (satisfy $ fun c => c.isAlpha)
     let _ ← parseSpace
     return (#[p] ++ n).asString)
  let extraPairs ← many (do
    let p ← parsePath
    let _ ← pstring ":" *> parseSpace
    let v ← parseToken <|> parseQuotedString
    let _ ← parseSpace
    return (p, v))
  let _ ← parseSpace *> pstring "]"
  return (varSort, extraPairs)

def parseVar1 : Parsec Var := do
  let nm ← parseVarName <* parseSpace
  let ps ← parseVarProps <* parseSpace
  return { name := nm, sort := ps.1, props := ps.2 : Var }

def parseVar : Parsec  Var := do
  attempt parseVar1 <|> (do
   let nm ← parseVarName
   return { name := nm , sort := none, props := #[]})

def parseHandle : Parsec Var := do
  let p ← parseVarName
  return { name := p, sort := some "h", props := #[] : Var }

def pinteger : Parsec Int := do
 let a ← (pchar '-' >>= (fun a => manyCore digit #[a])) <|> (many digit)
 return a.asString.toInt!

def parseLnk : Parsec (Int × Int) := do
  let _ ← pchar '<'
  let a ← pinteger
  let _ ← pchar ':'
  let b ← pinteger
  let _ ← pchar '>'
  return (a, b)

def parseRArgs : Parsec (Array (String × Var)) := do
  let pairs ← many (do
    let p ← parseToken
    let _ ← pstring ":" *> parseSpace
    let v ← parseVar <* parseSpace
    return (p, v))
  return pairs

def parseEP : Parsec EP := do
  let _   ← pstring "[" <* parseSpace
  let p   ← parseTypePred <|> parseQuotedString
  let lnk ← parseLnk <* parseSpace
  let lbl ← pstring "LBL:" *> parseSpace *> parseHandle
  let ras  ← parseSpace *> parseRArgs
  let _   ← parseSpace *> pstring "]"
  return EP.mk p lbl (some lnk) ras.toList none

def pTest := "[_car_n_1<1:2> LBL: h8 ARG0: x3 [ x PERS: 3 NUM: SG IND: + ] ARG1: x9 [ x PERS: 3 NUM: SG IND: ind ] ]"
#eval parseEP pTest.mkIterator

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
