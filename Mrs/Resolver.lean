
open Std

/-
[ LTOP: h0
        INDEX: e2 [e SF prop TENSE pres MOOD indicative PROG - PERF -]
        RELS: <
         [ _every_q<0,5> LBL: h4 BODY: h6 RSTR: h5 ARG0: x3 [x PERS 3 NUM sg IND +] ]
         [ _boy_n_1<6,9> LBL: h7 ARG0: x3 ]
         [ _love_v_1<10,15> LBL: h1 ARG2: x8 [x PERS 3 NUM sg IND +] ARG1: x3 ARG0: e2 ]
         [ _a_q<16,17> LBL: h9 BODY: h11 RSTR: h10 ARG0: x8 ]
         [ _woman_n_1<18,23> LBL: h12 ARG0: x8 ] >
        HCONS: < h0 qeq h1 h5 qeq h7 h10 qeq h12 > ]
-/


def nonquantified_var_p (var : Var) : Prop := sorry

def find_unbound_vars (m : MRS) : List String := sorry

def make_scoped_mrs (m : MRS) : List (List String) := sorry

def bindings (max_holes : Nat) (m : Mrs) : Nat × Nat := sorry

-- def construct_initial_bindings := sorry

-- def create_scoped_structures := sorry
