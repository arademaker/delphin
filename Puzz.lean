/-
https://dmcommunity.org/challenge/challenge-nov-2014/
https://ece.uwaterloo.ca/~agurfink/stqam.w20/assets/pdf/W07-FOL.pdf
https://arxiv.org/pdf/1204.6615.pdf
-/

import Std.Logic
import Std.Tactic.RCases

variable (u : Type)
variable (lives : u → Prop)
variable (killed hates richer : u → u → Prop)
variable (agatha butler charles : u)

variable (p1 : ∃ x : u, lives x ∧ killed x agatha)
-- variable (pel55_2_1 : lives agatha)
-- variable (pel55_2_2 : lives butler)
-- variable (pel55_2_3 : lives charles)

variable (p3 : ∀ x, lives x → x = agatha ∨ x = butler ∨ x = charles)
variable (p4 : ∀ x y, killed x y → hates x y)
variable (p5 : ∀ x y, killed x y → ¬ richer x y)
variable (p6 : ∀ x, hates agatha x → ¬ hates charles x)

variable (p7 : ∀ x, x ≠ butler → hates agatha x)
variable (p8 : ∀ x, ¬ richer x agatha → hates butler x)
variable (p9 : ∀ x, hates agatha x → hates butler x)

variable (p10 : ∀ x, ∃ y, ¬ hates x y)
variable (p10' : ¬ ∃ x, ∀ y, lives y → hates x y)
variable (p10'' : ¬ ∃ x, ∀ y, hates x y)

variable (p11 : agatha ≠ butler)

theorem result : killed agatha agatha := by
  have ⟨n,h1,h2⟩ := p1
  have h3 := p3 n h1
  have nkca : ¬killed charles agatha := by
    have haa : hates agatha agatha := p7 agatha p11
    have nhca : ¬hates charles agatha := p6 agatha haa
    intro kca
    exact nhca (p4 charles agatha kca)

  cases h3 with
  | inl h => rw [h] at h2; exact h2
  | inr h => cases h with
    | inl h =>
        rw [h] at h1 h2; clear h
        cases (p3 butler) h1 with
        | inl h => rw [h] at h2; exact h2
        | inr h => cases h with
          | inl h => rw [h]; exact p8 butler (p5 butler agatha h2)
          | inr h => rw [h]
                     apply p9 charles
                     apply p7 charles
                     intro H
                     rw [←H] at h2
                     exact nkca h2
    | inr h =>
       rw [h] at h2
       apply False.elim
       exact nkca h2

/- In other words, assuming the [butler killed Agatha] would involve
   that the [butler hates everyone], which is impossible. -/


theorem result' : killed agatha agatha := by
  have ⟨n,h1,h2⟩ := p1
  have h3 := p3 n h1
  have nkca : ¬killed charles agatha := by
    have haa : hates agatha agatha := p7 agatha p11
    have nhca : ¬hates charles agatha := p6 agatha haa
    intro kca
    exact nhca (p4 charles agatha kca)

  cases h3 with
  | inl h => rw [h] at h2; exact h2
  | inr h => cases h with
    | inl h =>
        rw [h] at h1 h2; clear h
        apply False.elim
        apply p10'; exists butler; intro y hy
        cases p3 y hy with
        | inl h => rw [h] at *; exact p4 butler agatha h2
        | inr h => cases h with
          | inl h => rw [h]; exact p8 butler (p5 butler agatha h2)
          | inr h => rw [h]
                     apply p9 charles
                     apply p7 charles
                     intro H
                     rw [←H] at h2
                     exact nkca h2
    | inr h =>
       rw [h] at h2
       apply False.elim
       exact nkca h2

#print axioms result'
