
section Test2

 -- grammatical
 axiom x : Type
 axiom e : Type
 
 axiom _be_v_id : e → x → x → Prop
 axiom _benzene_n_1 : x → Prop
 axiom _toxicity_n_1 : x → Prop
 axiom poss : e → x → x → Prop
 axiom _of_p : e → x → x → Prop 
 axiom compound : e → x → x → Prop  
 axiom thing : x → Prop

 -- wikidata
 axiom t : Type

 axiom benzene : t  -- Q123
 axiom toxicity : t → t → Prop  -- P2240
 
  
 def txt := "What is the toxicity of benzene?"


/-
 inductive SenseMap where
  | map1 (a : x → Prop) (b : x) : SenseMap
  | map2 (a : x → Prop) (b : x → x → Prop) : SenseMap

inductive NodeMap : (x → Prop) → t → Type :=
 | nm_benzene : NodeMap _benezene_n_1 benzene
  
 inductive EdgeMap : (x -> Prop) -> (t -> t -> Prop) -> Type :=
 | em_toxicity : EdgeMap _toxicity_n_1 toxicity
-/

structure NodeMap : Type := 
  (a : x → Prop) (b : t)

structure EdgeMap : Type := 
  (a : x → Prop) (b : t → t → Prop)  


 def do_query (x : Prop) := List 

 def process (txt : String) := 
   ∃ x3, (∃ x10, _benzene_n_1 x10 ∧ (∃ e18, _of_p e18 x3 x10 ∧ _toxicity_n_1 x3)) 
     ∧ (∃ x5, thing x5 ∧ (∃ e2, _be_v_id e2 x3 x5))

 def guess (txt : String) : Prop := ∃ v, toxicity benzene v 

 def h₁ (Qs Ps : x → Prop) (Qt : t) (Pt : t → t → Prop) : Prop := 
   ∀ x y e, (Qs x ∧ Ps y ∧ (poss e y x ∨ _of_p e y x ∨ compound e y x)) → ∃ v, Pt Qt v

 def h₂ : Prop := ∀ e a b, _be_v_id e a b → ∀ P : x → Prop, P a → P b

 example (h : h₁ _benzene_n_1 _toxicity_n_1 benzene toxicity) : process txt → guess txt := by
   unfold process guess 
   intro ⟨x1,⟨⟨x2,⟨h1,⟨x4,⟨h3,h4⟩⟩⟩⟩,_⟩⟩
   apply (h x2 x1 x4)
   constructor; assumption 
   constructor; assumption
   apply Or.inr; apply Or.inl; assumption 

end Test2
 
/- 
-- approach 1: validando uma query
-- Δ = nao explicitamente codificado ~> retrived de KG

((... ∧ a -> Q) ∧ f) -> Q
(Δ₁ ∧ Δ₂ ∧ f) -> Q

-- approach 2: derivando uma query

def guess'(f):
 for (a -> Q) in Δ: 
  if f |- a return Q

-- approach 3: heuristica estrutual não logica

def guess''(f):
 if inspection(f) |- Q 
  return Q

word disambiguation
1. ukb
2. mccard
3. LLM / ?

-/

#generate "isto é um teste" my_theorem




