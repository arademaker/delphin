
import Lean.Data.Parsec

open Lean Parsec
open Std

/- the data structure
   https://github.com/delph-in/docs/wiki/MrsRFC
-/

structure Var where
 name  : String
 sort  : Option String
 props : Array (String × String)

instance : ToFormat Var where
 format
  | {name := n, sort := some s, props := #[]} =>
    f!"{n} [{s}]"
  | {name := n, sort := some s, props := ps} =>
    let a := Format.joinSep (ps.toList.map fun p => f!"{p.1} {p.2}") " "
    f!"{n} [{s} {a}]"
  | {name := n, sort := none, props := #[]} =>
    f!"{n}"
  | {name := n, sort := none, props := ps} =>
    let a := Format.joinSep ps.toList " "
    f!"{n} [{a}]"

instance : Repr Var where
 reprPrec v _ := f!"{v}"


structure Constraint where
  rel : String
  lhs : Var
  rhs : Var

instance : ToFormat Constraint where
 format
  | {rel := r, lhs := l, rhs := h} => f!"{l} {r} {h}"

instance : Repr Constraint where
 reprPrec c _ := f!"{c}"


structure EP where
  predicate : String
  link  : Option (Int × Int)
  label : Var
  rargs : List (String × Var)
  carg  : Option String
deriving Repr

instance : ToFormat EP where
 format
  | {predicate := p, link := some (n,m), label := l, rargs := rs, carg := some c} =>
    let as := Format.joinSep (rs.map fun a => f!"{a.1}: {a.2}") " "
    f!"[ {p}<{n},{m}> LBL: {l} {as} CARG: {c} ]"
  | {predicate := p, link := some (n,m), label := l, rargs := rs, carg := none} =>
    let as := Format.joinSep (rs.map fun a => f!"{a.1}: {a.2}") " "
    f!"[ {p}<{n},{m}> LBL: {l} {as} ]"
  | {predicate := p, link := none, label := l, rargs := rs, carg := some c} =>
    let as := Format.joinSep (rs.map fun a => f!"{a.1}: {a.2}") " "
    f!"[ {p} LBL: {l} {as} CARG: {c} ]"
  | {predicate := p, link := none, label := l, rargs := rs, carg := none} =>
    let as := Format.joinSep (rs.map fun a => f!"{a.1}: {a.2}") " "
    f!"[ {p} LBL: {l} {as} ]"

instance : Repr EP where
 reprPrec e _ := f!"{e}"


structure MRS where
  top : Var
  index : Var
  preds : List EP
  hcons : List Constraint
  icons : List Constraint
deriving Repr

instance : ToFormat MRS where
 format
 | {top := t, index := i, preds := ps, icons := [], hcons := hs} =>
   f!"[ LTOP: {t}
        INDEX: {i}
        RELS: < {Format.joinSep (ps.map fun a => format a) " "} >
        HCONS: < {Format.joinSep (hs.map fun a => format a) " "} > ]"
 | {top := t, index := i, preds := ps, icons := is, hcons := hs} =>
   f!"[ LTOP: {t}
        INDEX: {i}
        RELS: < {Format.joinSep (ps.map fun a => format a) " "} >
        HCONS: < {Format.joinSep (hs.map fun a => format a) " "} >
        ICONS: < {Format.joinSep (is.map fun a => format a) " "} > ]"

instance : Repr MRS where
 reprPrec m _ := f!"{m}"

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

def parseTop : Parsec Var :=
  pstring "LTOP: " *> parseVar

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
    let a ← parseHandle <* parseSpace
    let r ← parseToken <* parseSpace
    let b ← parseHandle <* parseSpace
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

def test := "[ LTOP: h1
  INDEX: e2 [ e SF: PROP TENSE: PRES MOOD: INDICATIVE PROG: - PERF: - ]
  RELS: < [ _the_q_rel<0:3> LBL: h3 ARG0: x5 [ x PERS: 3 NUM: SG IND: + ] RSTR: h6 BODY: h4 ]
          [ \"_road_n_1_rel\"<4:8> LBL: h7 ARG0: x5 ]
          [ \"_rise_v_1_rel\"<9:14> LBL: h8 ARG0: e2 ARG1: x5 ]
          [ _from_p_dir_rel<15:19> LBL: h8 ARG0: e9 [ e SF: PROP TENSE: UNTENSED MOOD: INDICATIVE PROG: - PERF: - ] ARG1: e2 ARG2: x10 [ x PERS: 3 NUM: SG ] ]
          [ place_n_rel<20:26> LBL: h11 ARG0: x10 ]
          [ def_implicit_q_rel<20:26> LBL: h12 ARG0: x10 RSTR: h13 BODY: h14 ]
          [ _there_a_1_rel<20:26> LBL: h11 ARG0: e15 [ e SF: PROP TENSE: UNTENSED MOOD: INDICATIVE PROG: - PERF: - ] ARG1: x10 ] >
  HCONS: < h6 qeq h7 h13 qeq h11 > ]"

#eval parseMRS test.mkIterator
