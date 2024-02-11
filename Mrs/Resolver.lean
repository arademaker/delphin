
import Mrs.Basic
import Mrs.Parser
import Ace

open Std

def mrs2 := "[ LTOP: h0
         INDEX: e2 [e SF: prop TENSE: pres MOOD: indicative PROG: - PERF: -]
         RELS: < [ _the_q<0:3> LBL: h4 BODY: h6 RSTR: h5 ARG0: x3 [x PERS: 3 NUM: sg IND: +] ] [ _cat_n_1<4:7> LBL: h7 ARG0: x3 ] [ _blue_a_1<11:15> LBL: h1 ARG1: x3 ARG0: e2 ] >
         HCONS: < h0 qeq h1 h5 qeq h7 > ]"

def is_quantifier (rel : EP) : Bool :=
 match rel.rargs.find? (λ fv => fv.1 == "BODY") with
 | none => false
 | some _ => true

def get_arg_value (nm : String) (rel : EP) : Option Var :=
 match rel.rargs.find? (λ fv => fv.1 == nm) with
 | none => none
 | some (_, v) => some v

def collect_vars (rel : EP) : List Var :=
  (rel.rargs.map (λ fv => fv.2))

def find_unbound_vars (m : MRS) : List Var :=
  let quant_rels := m.preds.filter is_quantifier
  let quant_vars := quant_rels.map (get_arg_value "ARG0")
  let as := m.preds.foldl (λ vs e => vs.append $ collect_vars e) []
  let is_q (v₁ : Var) : Option Var → Bool
  | none => false
  | some v₂ => v₁ == v₂
  as.filter (λ v => not $ quant_vars.any (is_q v))

def nonquantified_var (v : Var) : Bool :=
  v.sort ≠ 'x'

def map_quantifiers_preds (m : MRS) : List (EP × List EP) :=
  let  quant_rels := m.preds.filter is_quantifier
  let nquant_rels := m.preds.filter (not ∘ is_quantifier)
  let   quant_map := (quant_rels.foldl (λ rs p =>
    match get_arg_value "ARG0" p with
    | none => rs
    | some v => (v, p) :: rs) []).toAssocList
  let f (p : EP) (rs : List (EP × List EP)) (v : Var) :=
     match quant_map.find? v with
     | none => rs
     | some pq => match rs.toAssocList.find? pq with
        | none => (pq, [p]) :: rs
        | some ps => (rs.toAssocList.replace pq (p :: ps)).toList
  let g (rs : List (EP × List EP)) (p : EP) :=
   (collect_vars p).foldl (f p) rs
  (nquant_rels.foldl g [])

-- def create_scoped_structures (m : MRS) := sorry

def test1 := do
  let as ← run_ace "Every boy wants to love a girl."
  let rs := as.map (λ r : MRS => map_quantifiers_preds r)
  return rs.head?

#eval test1
