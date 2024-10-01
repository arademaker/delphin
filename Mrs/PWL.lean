
inductive HOLTerm where
| and  : List HOLTerm -> HOLTerm
| or   : List HOLTerm -> HOLTerm
| eq   : HOLTerm -> HOLTerm -> HOLTerm
| cond : HOLTerm -> HOLTerm -> HOLTerm
| not  : HOLTerm -> HOLTerm
| all  : String → HOLTerm → HOLTerm
| ex   : String → HOLTerm → HOLTerm
| lmb  : String → HOLTerm → HOLTerm
| app  : HOLTerm → List HOLTerm → HOLTerm
| str  : String → HOLTerm
| var  : String → HOLTerm
| const : String → HOLTerm
| num   : Nat → HOLTerm
| TRUE  : HOLTerm
| FALSE : HOLTerm
 deriving Repr

/-
  ∃ E, (E = (λ e => person e)
   ∧ ¬ ∃ p, (person p
    ∧ ∀ e, (E e → ∃ h, hate h ∧ arg1 h = p ∧ arg2 h = e))))
-/

open HOLTerm in
def test : HOLTerm :=
  ex "E"
  (and [
    (eq (var "E") (lmb "e" (app (const "person") [(var "e")]))),
    (not (ex "p"
     (and
      [(app (const "person") [(var "p")]),
       (all "e"
        (cond (app (const "E") [(var "e")])
         (ex "h" (and
           [(app (const "hate") [(var "h")]),
            (eq (app (const "arg1") [(var "h")]) (var "p")),
            (eq (app (const "arg2") [(var "h")]) (var "e"))]))))])))])

#reduce test

def convert : MRS → HOLTerm  := sorry
