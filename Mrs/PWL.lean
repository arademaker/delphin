
inductive HOLTerm where
| and  : List HOLTerm -> HOLTerm
| or   : List HOLTerm -> HOLTerm
| eq   : HOLTerm -> HOLTerm -> HOLTerm
| cond : HOLTerm -> HOLTerm -> HOLTerm
| not  : HOLTerm -> HOLTerm
| all  : Nat → HOLTerm → HOLTerm
| ex   : Nat → HOLTerm → HOLTerm
| lmb  : Nat → HOLTerm → HOLTerm
| app  : HOLTerm → List HOLTerm → HOLTerm
| str  : String → HOLTerm
| var  : Nat → HOLTerm
| const : Nat → HOLTerm
| num   : Nat → HOLTerm
| TRUE  : HOLTerm
| FALSE : HOLTerm
