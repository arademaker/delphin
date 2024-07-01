
section Test0

 axiom e : Type
 axiom x : Type

 axiom _motorcycle_n_1 : x → Prop
 axiom _white_a_1 : e → x → Prop
 axiom _rider_n_1 : x → Prop
 axiom _on_p_state : e → e → x → Prop
 axiom _stand_v_up : e → x → Prop
 axiom _seat_n_1 : x → Prop

 axiom compound : e → x → x → Prop
 axiom _of_p : e → x → x → Prop 

 axiom _on_p_loc : e → x → x → Prop
 axiom _table_n_1 : x → Prop

 axiom R₁ : x → x → Prop -- (∃ e2 e14, _stand_v_up e2 x3 ∧ _on_p_state e14 e2 x15)
 axiom R₂ : x → x → Prop -- (∃ e8, compound e8 x3 x9 ∧ _rider_n_1 x3)
 axiom R₃ : x → x → Prop -- (∃ e20, _seat_n_1 x15 ∧ _of_p e20 x15 x21)

 def f01 := ∃ x9, _motorcycle_n_1 x9 ∧ (∃ x21, (∃ e26, _white_a_1 e26 x21 ∧ _motorcycle_n_1 x21) 
    ∧ (∃ x15, R₃ x15 x21 ∧ (∃ x3, R₂ x3 x9 ∧ R₁ x3 x15)))

 def f02 := ∃ x9, _motorcycle_n_1 x9 ∧ (∃ x21, _motorcycle_n_1 x21 
    ∧ (∃ x15, R₃ x15 x21 ∧ (∀ x3, R₂ x3 x9 → ¬ R₁ x3 x15)))

 example : (f01 ∧ f02) → False := by 
   unfold f01 f02
   intro ⟨⟨a1,⟨h1,⟨a2,⟨⟨a5,⟨h7,h8⟩⟩,⟨a6,⟨h5,h9⟩⟩⟩⟩⟩⟩,⟨a3,⟨h3,⟨a4,⟨h4,⟨a7,⟨a8,h6⟩⟩⟩⟩⟩⟩⟩ 
   -- there is no way to unify the two existentials
   sorry 

example : 
  ((∀ x3, _rider_n_1 x3 → ¬(∃ x8, _table_n_1 x8 ∧ (∃ e2, _on_p_loc e2 x3 x8))) ∧
   (∃ x8, _table_n_1 x8 ∧ (∃ x3, _rider_n_1 x3 ∧ (∃ e2, _on_p_loc e2 x3 x8)))) → False := 
  by 
   intro ⟨h1,⟨t,⟨h2,⟨r,⟨h3,⟨s,h4⟩⟩⟩⟩⟩⟩
   specialize h1 r h3
   apply h1
   apply Exists.intro t
   apply And.intro
   exact h2
   apply Exists.intro s
   exact h4
   
end Test0


section Test1

 axiom nominalization : x → Prop → Prop
 axiom card : Nat → e → x → Prop
 
 axiom _water_n_1 : x → Prop
 axiom _fish_v_1 : e → Prop
 axiom _stand_v_1 : e → x → Prop
 axiom _hold_v_1 : e → x → x → Prop
 axiom _pole_n_1 : x → Prop
 axiom _near_p_state : e → e → x → Prop
 axiom _and_c : e → e → e → Prop 

 def f1b := ∃ e9 x24, (∃ e29, nominalization x24 (_fish_v_1 e29)) 
   ∧ (∃ x12, _water_n_1 x12 ∧ (∃ x18, (∃ e23, compound e23 x18 x24 ∧ _pole_n_1 x18) 
   ∧ (∃ x3, (card 2 e9 x3 ∧ _man_n_1 x3) ∧ (∃ e10 e11 e2 e17, _stand_v_1 e10 x3 ∧ _near_p_state e11 e10 x12 
   ∧ _and_c e2 e10 e17 ∧ _hold_v_1 e17 x3 x18))))

 def f2b := ∃ e9 x10, (∃ x16, (∃ e21, nominalization x16 (_fish_v_1 e21)) 
   ∧ (∃ e15, compound e15 x10 x16 ∧ _pole_n_1 x10)) ∧ (∃ x3, (card 2 e9 x3 ∧ _man_n_1 x3) 
   ∧ (∃ e2, _hold_v_1 e2 x3 x10))

 theorem test1 : f1b → f2b := by 
  unfold f1b f2b
  intro ⟨n1, ⟨n2, ⟨⟨n10,h9⟩, ⟨_, ⟨_, ⟨n4, ⟨⟨n11, ⟨h10,h3⟩⟩ , ⟨n5, ⟨⟨ h11,h4⟩, ⟨_, ⟨_, ⟨_, ⟨n9, ⟨_, ⟨_, ⟨_,h8⟩⟩⟩⟩⟩⟩⟩⟩⟩⟩⟩⟩⟩⟩⟩⟩ 
  apply Exists.intro n1
  apply Exists.intro n4
  apply And.intro 
  apply Exists.intro n2
  apply And.intro
  apply Exists.intro n10
  exact h9
  apply Exists.intro n11
  apply And.intro
  exact h10
  exact h3
  apply Exists.intro n5
  apply And.intro
  apply And.intro
  exact h11
  exact h4
  apply Exists.intro n9
  exact h8

  
end Test1


section TEST

 axiom _walk_v_1 : e → x → Prop

 -- a man is walking
 def h1a : Prop := ∃ e2 x3, _man_n_1 x3 ∧ _walk_v_1 e2 x3
 def h1b : Prop := ∃ x3, _man_n_1 x3 ∧ ∃ e2, _walk_v_1 e2 x3
 
 -- no man is walking
 def h3  : Prop := ∀ x3, _man_n_1 x3 → ¬ ∃ e2, _walk_v_1 e2 x3

 -- a man is not walking
 def h2a : Prop := ¬(∃ x3, _man_n_1 x3 ∧ ∃ e2, _walk_v_1 e2 x3)
 def h2b : Prop := ∃ x3, _man_n_1 x3 ∧ ¬ ∃ e2, _walk_v_1 e2 x3

 theorem test2 : (h1a ∧ h3) → False := by
  unfold h1a h3
  intro ⟨⟨n, h₁⟩, h₂⟩ 
  apply Exists.elim h₁ 
  intro b h₃
  apply (h₂ b)
  { exact h₃.left }
  { exists n; exact h₃.right }

 theorem test3 : (h1b ∧ h3) → False := by
  unfold h1b h3
  intro ⟨⟨n, h₁⟩ , h₂⟩ 
  apply (h₂ n)
  { exact h₁.left }
  { exact h₁.right }

  
 theorem test4 : (h1b ∧ h2a) → False := by
  unfold h1b h2a
  intro ⟨h₁, h₂⟩
  apply h₂ 
  exact h₁ 

 theorem test5 : (h1b ∧ h2b) → False := by
  unfold h1b h2b
  intro ⟨⟨n,h₁⟩ , ⟨ m, h₂⟩⟩  
  -- I don't know how to prove this one, 
  sorry

end TEST
