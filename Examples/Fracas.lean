

/-
https://ncatlab.org/nlab/show/dependent+type+theoretic+methods+in+natural+language+semantics
-/

section MTT

  variable (_farmer_n_1 : Type α)
  variable (_donkey_n_1 : Type α)
  variable (_own_v_1 : _farmer_n_1 → _donkey_n_1 → Type α)
  variable (_beat_v_to : _farmer_n_1 → _donkey_n_1 → Type α)

  #check ∀ z : (Σ x : _farmer_n_1, Σ y : _donkey_n_1, _own_v_1 x y), _beat_v_to z.1 z.2.1
  #check fun z : (Σ x : _farmer_n_1, Σ y : _donkey_n_1, _own_v_1 x y) => _beat_v_to z.1 z.2.1

end MTT


def third (xs : List ℕ) (ok : xs.length > 2): ℕ := xs[2]

#eval third [1,2,3] (by decide)

example (a : Type) (A B : a → a → Prop) (C : a → Prop) :
  (∃ x, (∃ y, A x y ∧ B x y) ∧ C x) → (∃ y, (∃ x, (A x y ∧ C x)) ∧ B x y) := by
  intro h
  cases h with
  | intro b pb =>
     cases pb.1 with
     | intro c pc => exists c; exists b; exact ⟨pc, pb.2⟩
