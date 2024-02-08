import Init.System.IO
import Std
import Mrs
import Lean.Data.Parsec
import Ace

#eval run_ace "Socrates is mortal."
-- def test := "..."
-- #eval test.drop 2415
-- #eval parseMRS test.mkIterator

def report (r : String × Except String MRS) :=
 match r.2 with
 | Except.ok b => IO.println $ List.length b.preds
 | Except.error e => IO.println (r.1, e)

def main : IO Unit := do
  let ls ← IO.FS.lines "ws201.txt"
  let rs := ls.toList.map (λ l => (l, Lean.Parsec.run parseMRS l))
  let _ ← rs.mapM report
  return ()


--- tests

def mrs1 := "[ LTOP: h0
        INDEX: e2 [e SF prop TENSE pres MOOD indicative PROG - PERF -]
        RELS: <
         [ proper_q<0,8> LBL: h4 BODY: h6 RSTR: h5 ARG0: x3 [x PERS 3 NUM sg IND +] ]
         [ named<0,8> LBL: h7 ARG0: x3 CARG: \"Socrates\" ]
         [ _mortal_a_1<12,18> LBL: h1 ARG1: x3 ARG0: e2 ] >
        HCONS: < h0 qeq h1 h5 qeq h7 > ]"



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


def test1 := do
  let as ← run_ace "every boy loves a woman"
  return as

#eval test1
