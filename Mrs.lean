
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

/- the parser -/

def array2string (a : Array Char) : String :=
 Array.foldl (λ s (c : Char) => s ++ c.toString) "" a

-- /[A-Za-z][-A-Za-z]*\d+/

def letter : Parsec Char := satisfy fun c =>
  0x41 ≤ c.val ∧ c.val ≤ 0x5A ∨
  0x61 ≤ c.val ∧ c.val ≤ 0x7A

def identifier : Parsec Var := do
  let p ← letter
  let a ← many $ letter <|> pchar '-'
  let b ← many digit
  return Var.mk (array2string $ #[p] ++ a) (array2string b).toNat!

def top : Parsec Var :=
  pstring "LTOP: " *> identifier

def index : Parsec Var :=
  pstring "INDEX: " *> identifier

#eval top "LTOP: h3".mkIterator
