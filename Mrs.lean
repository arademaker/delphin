
import Lean.Data.Parsec
import Std.Data.hashMap

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
  link  : Option (Int × Int)
  label : Var
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
  let ps ← parseVarProps
  return { name := nm, sort := ps.1, props := ps.2 : Var }

def parseVar : Parsec Var := do
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

def parseArg : Parsec (String × Sum Var String) := do
 let p ← parseToken
 let _ ← pstring ":" *> parseSpace
 let v ← (Sum.inl <$> parseVar) <|> (Sum.inr <$> parseQuotedString)
 return (p, v)

def procArgs (acc : List $ String × Sum Var String)
             (p : String) (lnk : Option $ Int × Int)
             (lbl : Option Var) (carg : Option String)
             (ras : List $ String × Var) : Option EP :=
  match acc with
  | []  => match lbl with
           | some a => EP.mk p lnk a ras carg
           | none   => none
  | a :: as => if a.1 == "LBL"
               then match a.2 with
                    | .inl b => procArgs as p lnk b carg ras
                    |      _ => none
               else
                if a.1 == "CARG"
                then match a.2 with
                     | .inr b => procArgs as p lnk lbl b ras
                     |      _ => none
                else match a.2 with
                     | .inl b => procArgs as p lnk lbl carg ((a.1, b) :: ras)
                     | _      => none

def parseEP : Parsec EP := do
  let _   ← pstring "[" <* parseSpace
  let p   ← parseTypePred <|> parseQuotedString
  let lnk ← parseLnk <* parseSpace
  let ras ← many (parseArg <* parseSpace)
  let _   ← parseSpace *> pstring "]"
  let res := procArgs ras.toList p (some lnk) none none []
  match res with
  | none => fail "invalid EP"
  | some e => return e

def pTest := "[_car_n_1<1:2> LBL: h8 ARG0: x3 [ x PERS: 3 NUM: PL IND: + ]
                             CARG: \"are\" ARG1: x9 [ x PERS: 3 NUM: SG IND: ind ] ]"
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
