
import Std.Internal.Parsec
import Mrs.Basic

namespace MRS
open Array
open Lean
open Std.Internal.Parsec
open Std.Internal.Parsec.String

def _root_.Array.asString (a : Array Char) : String :=
  a.foldl (λ s c => s ++ c.toString) ""

/- the parser
   defined from the BNF https://github.com/delph-in/docs/wiki/MrsRFC
-/

def parseSpace : Parser String := do
  let a ← many (satisfy $ fun c => c.isWhitespace)
  return a.asString

def parsePath : Parser String := do
  let p ← asciiLetter
  let s ← many (satisfy $ fun c => c.isAlphanum)
  return (#[p] ++ s).asString

def parseToken : Parser String := do
  let p ← many1 $ satisfy $ fun c =>
    ¬ "\\:]>".data.elem c ∧ ¬ c.isWhitespace
  return p.asString

def parseTypePred : Parser String := do
 let aux : Char → Bool := fun c => (¬ "<>\"".data.elem c ∧ ¬ c.isWhitespace)
 let p ← many1 $ satisfy aux
 return p.asString


def parseQuotedString : Parser String := do
  let a1 ← pchar '"'
  let a2 ← many $ satisfy $ fun c => ¬ ['"', '\\'].elem c
  let a3 ← many (do
    let a ← pchar '\\'
    let b ← (satisfy $ fun c => c ≠ '\n')
    let c ← many $ satisfy $ fun c => ¬ ['"', '\\'].elem c
    return (#[a,b] ++ c).asString)
  let a4 ← pchar '"'
  return (a1.toString ++ a2.asString ++
    (Array.foldl (λ s c => s ++ c) "" a3) ++ a4.toString)

def parseVarProps : Parser (Char × Array (String × String)) := do
  let _ ← pstring "[" *> parseSpace
  let varSort ← (do
     let p ← asciiLetter
     let _ ← parseSpace
     return p)
  let extraPairs ← many (do
    let p ← parsePath
    let _ ← pstring ":" *> parseSpace
    let v ← parseToken <|> parseQuotedString
    let _ ← parseSpace
    return (p, v))
  let _ ← parseSpace *> pstring "]"
  return (varSort, extraPairs)

def parseVarName : Parser (Char × Nat) := do
  let p ← asciiLetter
  let s ← many1 digit
  return (p, s.asString.toNat!)


/- we would like to check if the type in the props match the type of the
   variable, `x3 [ x ...]` is valid but `x3 [ e ...]` is not

#eval parseVar "e44 [x rel: t SUB: -]".mkIterator
-/

def parseVar : Parser Var := do
  let nm ← parseVarName <* parseSpace
  let ps ← attempt parseVarProps <|> pure ('u', #[])
  return {id := nm.2, sort := nm.1, props := ps.2 : Var}

def parseHandle : Parser Var := do
  let p ← parseVarName
  return { id := p.2 , sort := p.1, props := #[] : Var }

def pinteger : Parser Int := do
 let a ← (pchar '-' >>= (fun a => manyCore digit #[a])) <|> (many digit)
 return a.asString.toInt!

def parseLnk : Parser (Int × Int) := do
  let _ ← pchar '<'
  let a ← pinteger
  let _ ← pchar ':'
  let b ← pinteger
  let _ ← pchar '>'
  return (a, b)

def parseArg : Parser (String × Sum Var String) := do
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

def parseEP : Parser EP := do
  let _   ← pstring "[" <* parseSpace
  let p   ← parseTypePred <|> parseQuotedString
  let lnk ← parseLnk <* parseSpace
  let ras ← many (parseArg <* parseSpace)
  let _   ← parseSpace *> pstring "]"
  let res := procArgs ras.toList p (some lnk) none none []
  match res with
  | none => fail "invalid EP"
  | some e => return e

def parseTop : Parser Var :=
  pstring "LTOP: " *> parseHandle

def parseIndex : Parser Var :=
  pstring "INDEX: " *> parseVar

def parseHcons : Parser (Array Constraint) := do
  let _ ← pstring "HCONS:" <* parseSpace
  let _ ← pchar '<' <* parseSpace
  let cs ← many (do
    let a ← parseHandle <* parseSpace
    let r ← parseToken <* parseSpace
    let b ← parseHandle <* parseSpace
    return (Constraint.mk r a b))
  let _ ← pchar '>'
  return cs

def parseIcons : Parser (Array Constraint) := do
  let _ ← pstring "ICONS:" <* parseSpace
  let _ ← pchar '<' <* parseSpace
  let cs ← many (do
    let a ← parseVar <* parseSpace
    let r ← parseToken <* parseSpace
    let b ← parseVar <* parseSpace
    return (Constraint.mk r a b))
  let _ ← pchar '>'
  return cs

def parseRels : Parser (Array EP) := do
  let _  ← pstring "RELS:" <* parseSpace
  let _  ← pchar '<' <* parseSpace
  let rs ← many (parseEP <* parseSpace)
  let _  ← pchar '>'
  return rs

def parseMRS : Parser MRS := do
    let _ ← pchar '['      <* parseSpace
    let top ← parseTop     <* parseSpace
    let idx ← parseIndex   <* parseSpace
    let rls ← parseRels    <* parseSpace
    let hcons ← parseHcons <* parseSpace
    let icons ← (attempt parseIcons <|> pure #[]) <* parseSpace
    let _ ← pchar ']'
    pure $ MRS.mk top idx rls.toList hcons.toList icons.toList

end MRS
