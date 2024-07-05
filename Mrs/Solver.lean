import Mrs.Basic
import Ace

open Lean

def valid_assignments( m : MRS) : List (Var × Var) :=
 let qeqs := m.hcons.filter (λ h => h.rel == "qeq")
 sorry



/-

inductive Tree where
 | node : List EP → List Tree → Tree

-/
