-- Checked with Agda 2.7.0.1 and Standard Library 2.2

{-# OPTIONS --safe #-}

open import Data.Product
open import Relation.Binary.PropositionalEquality

module InductionPropositional (ℕ : Set) (zero : ℕ) (suc : ℕ → ℕ) where

  IndType : Set₁
  IndType = (P : ℕ → Set) → P zero → (∀ {n} → P n → P (suc n)) → (n : ℕ) → P n

  Comp : IndType → Set₁
  Comp f = (P : ℕ → Set) (pz : P zero) (ps : ∀ {n} → P n → P (suc n))
         → f P pz ps zero ≡ pz
         × (∀ {n} → f P pz ps (suc n) ≡ ps (f P pz ps n))

  Ind : Set₁
  Ind = Σ IndType Comp

  Ind-isProp : ((f , _) (g , _) : Ind)
             → (P : ℕ → Set) (pz : P zero) (ps : ∀ {n} → P n → P (suc n))
             → (n : ℕ) → f P pz ps n ≡ g P pz ps n
  Ind-isProp (f , f-comp) (g , g-comp) P pz ps =
    f (λ n → f P pz ps n ≡ g P pz ps n)
      (begin
         f P pz ps zero
           ≡⟨ proj₁ (f-comp P pz ps) ⟩
         pz
           ≡⟨ proj₁ (g-comp P pz ps) ⟨
         g P pz ps zero
       ∎)
      (λ {n} eq →
       begin
         f P pz ps (suc n)
           ≡⟨ proj₂ (f-comp P pz ps) ⟩
         ps (f P pz ps n)
           ≡⟨ cong ps eq ⟩
         ps (g P pz ps n)
           ≡⟨ proj₂ (g-comp P pz ps) ⟨
         g P pz ps (suc n)
       ∎)
    where open ≡-Reasoning

  Param : IndType → Set₁
  Param g = (P  : ℕ → Set)                 (Q  : ∀ {n} → P n → Set)
            (pz : P zero)                  (qz : Q pz)
            (ps : ∀ {n} → P n → P (suc n)) (qs : ∀ {n} {p : P n} → Q p → Q (ps p))
          → (n : ℕ) → Q (g P pz ps n)

  uniqueness : (f g : IndType) → Comp f → Param g
             → (P : ℕ → Set) (pz : P zero) (ps : {n : ℕ} → P n → P (suc n))
             → (n : ℕ) → f P pz ps n ≡ g P pz ps n
  uniqueness f g f-comp g-param P pz ps =
    g-param P  (λ {n} p → f P pz ps n ≡ p)
            pz (proj₁ (f-comp P pz ps))
            ps (λ eq → trans (proj₂ (f-comp P pz ps)) (cong ps eq))
