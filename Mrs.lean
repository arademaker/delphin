
import Lean.Data.Parsec
open Lean Parsec

/- the data structure -/

structure Var where
 p : String
 n : Nat
deriving Repr

structure Constraint where
  lhs : Var
  rhs : Var
deriving Repr

structure EP where
  handle : Var
  predicate : String
  arg : List Var
  s : List Var
  carg : String
deriving Repr

structure MRS where
  gt : Var
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


def Path : Parsec String := do
  let p ← asciiLetter
  let s ← many (satisfy $ fun c => c.isAlphanum)
  return (#[p] ++ s).asString

def Variable : Parsec String := do
  let p ← asciiLetter
  let n ← many (satisfy $ fun c => c.isAlpha)
  let s ← many1 digit
  return (#[p] ++ n ++ s).asString

def VarSort : Parsec String := do
  let p ← asciiLetter
  let s ← many asciiLetter
  return (#[p] ++ s).asString

def Token : Parsec String := do
  let p ← many1 $ satisfy $ fun c =>
    ['\\',':',']','>'].notElem c ∧ ¬ c.isWhitespace
  return p.asString


def letter : Parsec Char := satisfy fun c =>
  0x41 ≤ c.val ∧ c.val ≤ 0x5A ∨
  0x61 ≤ c.val ∧ c.val ≤ 0x7A

def identifier : Parsec Var := do
  let p ← letter
  let a ← many $ letter <|> pchar '-'
  let b ← many digit
  return Var.mk (Array.asString $ #[p] ++ a) (b.asString).toNat!

def top : Parsec Var :=
  pstring "LTOP: " *> identifier

def index : Parsec Var :=
  pstring "INDEX: " *> identifier
