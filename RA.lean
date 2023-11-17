
section RA

 axiom u : Type

 axiom _water_n_1  : u → Prop
 axiom _negligible_a_1 : u → Prop
 axiom _solubility_n_1 : u → Prop
 axiom _of_p : u → u → Prop
 axiom _bioaccumulation_nn_u_unknown : u → Prop
 axiom _low_a_on : u → Prop
 axiom _potential_n_1 : u → Prop
 axiom _have_v_1 : u → u → Prop
 axiom _log_n_of : u → u → Prop

 axiom Kow : u
 axiom X : u
 
 axiom poss : u → u → Prop
 axiom compound : u → u → Prop

 axiom bioaccumulation_potential : u → Prop
 axiom water_solubility : u → Prop
 axiom log_kow : u → Prop

 axiom f_bioaccumulation_potential : u → u
 axiom f_water_solubility : u → u
 axiom f_log_kow : u → u

 axiom low : u → Prop
 axiom high : u → Prop

 -- from NL questions

 -- The water solubility of X is negligible.
 def PA : Prop := ∃ x9, _water_n_1 x9 
  ∧ (∃ x3, (∃ x15, X = x15 ∧ compound x3 x9 ∧ _solubility_n_1 x3 ∧ _of_p x3 x15) ∧ _negligible_a_1 x3)

 -- X has a low bioaccumulation potential. 
 def PB : Prop := ∃ x9, (∃ x17, _bioaccumulation_nn_u_unknown x17 ∧ _low_a_on x9 ∧ compound x9 x17 ∧ _potential_n_1 x9) 
  ∧ (∃ x3, X = x3 ∧ _have_v_1 x3 x9)

 -- X's log of Kow is low. 
 def PC : Prop := ∃ x3, (∃ x5, X = x5 ∧ (∃ x15, Kow = x15 ∧ poss x3 x5 ∧ _log_n_of x3 x15)) ∧ _low_a_on x3


 -- axioms from the Domain and Grammar

 variable (g1 : ∀ x p, _have_v_1 x p ↔ _of_p p x)
 variable (g2 : ∀ x p, poss p x ↔ _of_p p x)
 
 variable (g3 : ∀ x y, compound x y → _water_n_1 y → _solubility_n_1 x → water_solubility x)
 variable (g4 : ∀ x y, compound x y → _bioaccumulation_nn_u_unknown y → _potential_n_1 x → bioaccumulation_potential x)
 variable (g5 : ∀ x, _log_n_of x Kow → log_kow x)

 variable (g6 : ∀ x, _negligible_a_1 x ↔ _low_a_on x)

 variable (g7 : ∀ x p, _have_v_1 x p → _low_a_on p → water_solubility p → low (f_water_solubility x))
 variable (g8 : ∀ x p, _have_v_1 x p → _low_a_on p → bioaccumulation_potential p → low (f_bioaccumulation_potential x))
 variable (g9 : ∀ x p, _have_v_1 x p → _low_a_on p → log_kow p → low (f_log_kow x))

 -- may not be necessary
 variable (g10 : ∀ x, low x ↔ ¬ high x )
 variable (g11 : ∀ x, high (f_log_kow x) → high (f_bioaccumulation_potential x))
 variable (g12 : ∀ x, low (f_log_kow x) → high (f_water_solubility x))

 theorem test : PA → PB → PC → False := by 
   unfold PA PB PC 
   intro ⟨a1, ⟨h1, ⟨a2, ⟨⟨a3,⟨h2,h4,h5,h6⟩⟩, h3⟩⟩⟩⟩   
   intro _
   intro ⟨c1, ⟨⟨c2,⟨k1,⟨c4,⟨k2,k3,k4⟩⟩⟩⟩, k5⟩⟩
   subst k1 h2 k2
   have g31 := (g3 a2 a1) h4 h1 h5
   -- have g41 := (g4 b1 b2) l3 l1 l4
   have g51 := (g5 c1) k4
   have g61 := (g6 a2).1 h3
   -- have g81 := (g8 X b1) l6 l2 g41 
   have g₁ := (g1 X c1).2 $ (g2 X c1).1 k3
   have g₂ := (g1 X a2).2 h6
   have g71 := (g7 X a2) g₂ g61 g31 
   have g91 := (g9 X c1) g₁ k5 g51 
   clear h4 h1 h5 k4 g3 g4 g5 g6 h3 k3 h6 g₁ g₂ k5 g61 g7 g8 g9 g1 g2
   have g₃ := (g12 X) g91    
   exact (g10 $ f_water_solubility X).1 g71 g₃

end RA

