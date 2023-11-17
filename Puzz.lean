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

variable (pel55_1 : ∃ x : u, lives x ∧ killed x agatha)
variable (pel55_2_1 : lives agatha)
variable (pel55_2_2 : lives butler)
variable (pel55_2_3 : lives charles)

variable (pel55_3 : ∀ x, lives x → x = agatha ∨ x = butler ∨ x = charles)
variable (pel55_4 : ∀ x y, killed x y → hates x y)
variable (pel55_5 : ∀ x y, killed x y → ¬ richer x y)
variable (pel55_6 : ∀ x, hates agatha x → ¬ hates charles x)

variable (pel55_7 : ∀ x, x ≠ butler → hates agatha x)
variable (pel55_8 : ∀ x, ¬ richer x agatha → hates butler x)
variable (pel55_9 : ∀ x, hates agatha x → hates butler x)

variable (pel55_10 : ∀ x, ∃ y, ¬ hates x y)
variable (pel55_11 : agatha ≠ butler)

theorem result : killed agatha agatha := by
  have ⟨n,h1,h2⟩ := pel55_1
  clear pel55_2_1 pel55_2_2 pel55_2_3
  have h3 := pel55_3 n h1
  -- let ⟨m, h₂⟩ := pel55_10 n
  cases h3 with
  | inl h => rw [h] at h2; exact h2
  | inr h => cases h with
    | inl h =>
       -- let ⟨m,h₁⟩ := pel55_10 n
       rw [h] at h2
       have h3 := pel55_4 butler agatha h2
       have h4 := pel55_8 butler (pel55_5 butler agatha h2)
       have h5 := pel55_10 agatha
       sorry
    | inr h =>
       have h4 := pel55_4 n agatha h2
       rw [h] at h4
       have h6 := pel55_6 agatha $ pel55_7 agatha pel55_11
       apply False.elim; apply h6; exact h4

/- In other words, assuming the [butler killed Agatha] would involve
   that the [butler hates everyone], which is impossible. -/

theorem result' : killed agatha agatha := by
  have haa : hates agatha agatha := pel55_7 agatha pel55_11
  have nkca : ¬killed charles agatha := by
    by_contra kca
    have hca := pel55_4 charles agatha kca
    have nhca := pel55_6 agatha haa
    contradiction
  have ⟨n, ln, kna⟩ := pel55_1
  obtain la | lb | lc := pel55_3 n ln
  . rw [la] at kna
    exact kna
  . rw [lb] at kna
    have nrba : ¬richer butler agatha := pel55_5 butler agatha kna
    have hbb : hates butler butler := pel55_8 butler nrba
    simp [Classical.skolem] at pel55_10
    have ⟨sk, nhsk⟩ := pel55_10
    have nhbsk : ¬hates butler (sk butler) := nhsk butler
    have nhask : ¬hates agatha (sk butler) := by
      by_contra hask
      have hbsk := pel55_9 (sk butler) hask
      contradiction
    have eqsk : sk butler = butler := by
      by_contra nesk
      have hbsk := pel55_7 (sk butler) nesk
      contradiction
    have nhbb := eqsk ▸ nhbsk
    contradiction
  . rw [lc] at kna
    contradiction
