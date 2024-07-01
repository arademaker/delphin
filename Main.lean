import Init.System.IO
import Std
import Mrs
import Lean.Data.Parsec
import Ace

def test1 := do
  let as ← run_ace "Every happy dog barks."
  return as.head?

-- set_option pp.oneline true
-- set_option pp.proofs true
-- set_option trace.profiler true in

#eval test1


def report (r : String × Except String MRS) :=
 match r.2 with
 | Except.ok b => IO.println $ List.length b.preds
 | Except.error e => IO.println (r.1, e)

def main : IO Unit := do
  let ls ← IO.FS.lines "ws201.txt"
  let rs := ls.toList.map (λ l => (l, Lean.Parsec.run parseMRS l))
  let _ ← rs.mapM report
  return ()

-- #eval main


/-
https://ncatlab.org/nlab/show/dependent+type+theoretic+methods+in+natural+language+semantics
-/

section MTT

  variable (_farmer_n_1 : Type α)
  variable (_donkey_n_1 : Type α)
  variable (_own_v_1 : _farmer_n_1 → _donkey_n_1 → Type α)
  variable (_beat_v_to : _farmer_n_1 → _donkey_n_1 → Type α)

  #check ∀ z : (Σ x : _farmer_n_1, Σ y : _donkey_n_1, _own_v_1 x y), _beat_v_to z.1 z.2.1
  #check fun z : (Σ x : _farmer_n_1, Σ y : _donkey_n_1, _own_v_1 x y) => _beat_v_to z.1 z.2.1

end MTT


/-

inductive Tree where
 | node : List EP → List Tree → Tree

variable (x : Type) (e : Type)

variable (inter : String → Prop)
variable (inter_ex : String → (e → x → Prop))
variable (inter_xx : String → (x → x → Prop))
variable (inter_ee : String → (e → e → Prop))


def ep2prop (e : EP) : Prop :=
 inter e.predicate

def mrs2prop (m : MRS) : Prop :=
 MRS.preds

def b :=
 ep2prop inter (EP.mk "mortal" none (Var.mk "h0" none #[]) [("ARG1", Var.mk "x3" none #[])] none)

-/
