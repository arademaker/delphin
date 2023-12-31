
import Lean.Data.Parsec
import Mrs.Basic

open Lean Parsec
open Std

def Array.asString (a : Array Char) : String :=
  Array.foldl (λ s c => s ++ c.toString) "" a

/- the parser
   defined from the BNF https://github.com/delph-in/docs/wiki/MrsRFC#simple
-/

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
 let aux : Char → Bool := fun c => ("<>\"".data.notElem c ∧ ¬ c.isWhitespace)
 let p ← many1 $ satisfy aux
 return p.asString

-- #eval parseTypePred "_m]achines/NNS_u_unknown".mkIterator

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

def parseTop : Parsec Var :=
  pstring "LTOP: " *> parseHandle

def parseIndex : Parsec Var :=
  pstring "INDEX: " *> parseVar


def parseHcons : Parsec (Array Constraint) := do
  let _ ← pstring "HCONS:" <* parseSpace
  let _ ← pchar '<' <* parseSpace
  let cs ← many (do
    let a ← parseHandle <* parseSpace
    let r ← parseToken <* parseSpace
    let b ← parseHandle <* parseSpace
    return (Constraint.mk r a b))
  let _ ← pchar '>'
  return cs

def parseIcons : Parsec (Array Constraint) := do
  let _ ← pstring "ICONS:" <* parseSpace
  let _ ← pchar '<' <* parseSpace
  let cs ← many (do
    let a ← parseVar <* parseSpace
    let r ← parseToken <* parseSpace
    let b ← parseVar <* parseSpace
    return (Constraint.mk r a b))
  let _ ← pchar '>'
  return cs

def parseRels : Parsec (Array EP) := do
  let _  ← pstring "RELS:" <* parseSpace
  let _  ← pchar '<' <* parseSpace
  let rs ← many (parseEP <* parseSpace)
  let _  ← pchar '>'
  return rs

def parseMRS : Parsec MRS := do
    let _ ← pchar '['      <* parseSpace
    let top ← parseTop     <* parseSpace
    let idx ← parseIndex   <* parseSpace
    let rls ← parseRels    <* parseSpace
    let hcons ← parseHcons <* parseSpace
    let icons ← (attempt parseIcons <|> pure #[]) <* parseSpace
    let _ ← pchar ']'
    pure $ MRS.mk top idx rls.toList hcons.toList icons.toList
