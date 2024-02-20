
import Mrs.Basic
import Ace

open Std

def is_quantifier (rel : EP) : Bool :=
 match rel.rargs.find? (λ fv => fv.1 == "BODY") with
 | none => false
 | some _ => true

def get_arg_value (nm : String) (rel : EP) : Option Var :=
 match rel.rargs.find? (λ fv => fv.1 == nm) with
 | none => none
 | some (_, v) => some v

def get_handel_args_with_features (p : EP) : List (String × Var) :=
 p.rargs.filter (λ fvp => fvp.2.sort == 'h')

def get_handel_args (p : EP) : List Var :=
 get_handel_args_with_features p |>.map (λ fvp => fvp.2)

def nonquantified_var (v : Var) : Bool :=
  v.sort ≠ 'x'

def collect_vars (rel : EP) : List Var :=
  (rel.rargs.map (λ fv => fv.2))

def has_duplicated_handle_argument (m : MRS) : Bool :=
  let hs := m.preds.map get_handel_args
  let ha := hs.foldl List.append []
  ha.length ≠ ha.eraseDups.length

def holes (m : MRS) : List Nat :=
  let hs := m.preds.map get_handel_args |>.foldl List.append []
  hs ++ [m.top] |>.map (λ v => v.id)

def fst_and_rest : List α → List (α × List α)
 | [] => []
 | (n :: ns) => (n, ns) :: fst_and_rest ns

def check_all_qeqs (m : MRS) : Bool :=
  let labels := m.preds.map (λ p => p.label.id)
  let  holes := holes m
  let c₁ (h : Constraint) : Bool := ¬ (h.lhs == h.rhs)
  let c₂ (h : Constraint) : Bool := holes.elem h.lhs.id ∧ ¬ labels.elem h.lhs.id
  let c₃ (h : Constraint) : Bool := labels.elem h.rhs.id ∧ ¬ holes.elem h.rhs.id
  let rec f (h : Constraint) : List Constraint → Bool
   | [] => true
   | (h' :: hs) => (h.lhs.id ≠ h'.lhs.id) ∧ (h.rhs.id ≠ h'.rhs.id) ∧ f h hs
  let c₄ := (fst_and_rest m.hcons).all (λ h => f h.1 h.2)
  c₄ ∧ m.hcons.all (λ h => c₁ h ∧ c₂ h ∧ c₃ h)

def find_unbound_vars (m : MRS) : List Var :=
  let q_rels := m.preds.filter is_quantifier
  let q_vars := q_rels.map (get_arg_value "ARG0")
  let as := m.preds.foldl (λ vs e => vs.append $ collect_vars e) []
  let is_q (v₁ : Var) : Option Var → Bool
  | none => false
  | some v₂ => v₁ == v₂
  as.filter (λ v => not $ q_vars.any (is_q v))

def map_quantifiers_preds (m : MRS) : AssocList EP (List EP) :=
  let  γ : Type := AssocList EP (List EP)
  let  q_rels := m.preds.filter is_quantifier
  let   q_map := (q_rels.foldl (λ rs p =>
   match get_arg_value "ARG0" p with
    | none => rs
    | some v => (v, p) :: rs) []).toAssocList
  let rec f (p : EP) (tb : γ) : List Var → γ
   | [] => tb
   | (v :: vs) => match q_map.find? v with
     | none => tb
     | some pq => match tb.find? pq with
       | none => f p (tb.cons pq [p]) vs
       | some ps => f p (tb.replace pq (p :: ps)) vs
  let rec g (tb : γ) : List EP → γ
   | [] =>  tb
   | (p :: ps) => g (f p tb $ collect_vars p) ps
  g AssocList.nil $ m.preds.filter (not ∘ is_quantifier)


def label_hole_pairs (m : MRS) : List (Nat × List Nat) :=
  let f (p : EP) := (p.label.id, (get_handel_args p).map (λ v => v.id))
  m.preds.map f

def construct_initial_bindings (m : MRS) : List (Nat × List Nat) :=
  let labels := m.preds.map (λ p => p.label.id)
  let  holes := holes m
  let rs := labels ++ holes
  rs.eraseDups.map (λ n => (n , [n]))

def equated_list (m : MRS) : List Nat :=
  let labels := m.preds.map (λ p => p.label.id)
  let g (e : EP) : List Nat :=
    get_handel_args e |>.foldl (λ rs v =>
      if labels.elem v.id then v.id :: rs else rs) []
  let rec f : List EP -> List Nat -> List Nat
   | [], rs => rs
   | (e :: es), rs => f es $ rs.append (g e)
  f m.preds []


partial def outscoped_labels (m : MRS) (lhp : AssocList Nat (List Nat))
  (eql : List Nat) (hole : Nat) : List Nat :=
  let f := outscoped_labels m lhp eql
  let g (rs : List Nat) (l : Nat) :=
    if l == hole then rs else rs ++ (f l)

  let a :=
   if (eql.elem hole) then
    match lhp.find? hole with
    | none => []
    | some ls => ls.foldl g []
   else []

  let b := m.hcons.foldl
    (λ rs h =>
     if h.lhs.id == hole then
      match lhp.find? h.rhs.id with
      | none => rs ++ [h.rhs.id]
      | some ls => rs ++ (h.rhs.id :: (ls.foldl g []))
     else rs) []
  a ++ b

def define_qeq_chains (m : MRS) : List (Nat × List Nat) :=
  let lhps := (label_hole_pairs m).toAssocList
  let eqs := equated_list m
  let f := outscoped_labels m lhps eqs
  holes m |>.map (λ h => (h, f h))

def test1 := do
  let as ← run_ace "Every boy loves a woman."
  let rs := as.map define_qeq_chains
  return rs

#eval test1
