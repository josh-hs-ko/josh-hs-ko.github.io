-- Checked with Agda 2.6.4.1 and Standard Library 2.0

{-# OPTIONS --safe --large-indices --no-forced-argument-recursion #-}

open import Function
open import Data.Empty
open import Data.Nat renaming (suc to succ)
open import Data.Nat.Properties
open import Data.Vec using (Vec; []; _∷_)
open import Relation.Binary.PropositionalEquality

variable
  A     : Set
  m n k : ℕ
  x     : A
  xs ys : Vec A n
  P     : Vec A k → Set

infix 2 _⊆_

data _⊆_ {A : Set} : {n k : ℕ} → Vec A k → Vec A n → Set where
  nil  :               [] ⊆ []
  keep : ys ⊆ xs → x ∷ ys ⊆ x ∷ xs
  drop : ys ⊆ xs →     ys ⊆ x ∷ xs

⊆-refl : {xs : Vec A n} → xs ⊆ xs
⊆-refl {xs = []    } = nil
⊆-refl {xs = x ∷ xs} = keep ⊆-refl

⊆-trans : {xs : Vec A n} {ys : Vec A m} {zs : Vec A k}
        → zs ⊆ ys → ys ⊆ xs → zs ⊆ xs
⊆-trans l        (drop r) = drop (⊆-trans l r)
⊆-trans nil      nil      = nil
⊆-trans (keep l) (keep r) = keep (⊆-trans l r)
⊆-trans (drop l) (keep r) = drop (⊆-trans l r)

Forall_-sublistsOf_⇒_ : ({n} k : ℕ) → Vec A n → (Vec A k → Set) → Set
Forall k -sublistsOf xs ⇒ P = {ys : Vec _ k} → ys ⊆ xs → P ys

ImmSubInd : Set₁
ImmSubInd = {A : Set} (P : ∀ {k} → Vec A k → Set)
          → P []
          → (∀ {k} {ys : Vec A (succ k)} → (Forall k -sublistsOf ys ⇒ P) → P ys)
          → ∀ {n} (xs : Vec A n) → P xs

td : ImmSubInd
td P e g []         = e
td P e g xs@(_ ∷ _) = g (λ {zs} _ → td P e g zs)

-- Pointwise reasoning is easier?
upgrade : (Forall k -sublistsOf xs ⇒ P)
        → (Forall succ k -sublistsOf xs ⇒ λ ys → Forall k -sublistsOf ys ⇒ P)
upgrade t {ys} ys⊆xs {zs} zs⊆ys = t (⊆-trans zs⊆ys ys⊆xs)

bu : ImmSubInd
bu P e g xs = loop (≤⇒≤‴ z≤n) init ⊆-refl
  where
    init : Forall 0 -sublistsOf xs ⇒ P
    init {[]} _ = e

    loop : k ≤‴ n → (Forall k -sublistsOf xs ⇒ P) → (Forall n -sublistsOf xs ⇒ P)
    loop  ≤‴-refl    t = t
    loop (≤‴-step d) t = loop d (g ∘ upgrade t)

data BT : (n k : ℕ) → (Vec A k → Set) → Vec A n → Set where
  tipZ : P []                                → BT n         zero    P xs
  tipS : P xs                                → BT (succ k) (succ k) P xs
  bin  : BT n (succ k)        P           xs
       → BT n       k (λ zs → P (x ∷ zs)) xs → BT (succ n) (succ k) P (x ∷ xs)

⊆-length : {xs : Vec A n} {ys : Vec A m} → ys ⊆ xs → m ≤ n
⊆-length nil      = z≤n
⊆-length (keep s) = s≤s (⊆-length s)
⊆-length (drop s) = m≤n⇒m≤1+n (⊆-length s)

⊆⇒≡ : {xs ys : Vec A n} → ys ⊆ xs → ys ≡ xs
⊆⇒≡  nil     = refl
⊆⇒≡ (keep s) = cong (_ ∷_) (⊆⇒≡ s)
⊆⇒≡ (drop s) = ⊥-elim (1+n≰n (⊆-length s))

fromBT : BT n k P xs → Forall k -sublistsOf xs ⇒ P
fromBT (tipZ p)       nil     = p
fromBT (tipS p)      (keep s) = subst _ (sym (⊆⇒≡ s)) p
fromBT (bin t u)     (keep s) = fromBT u s
fromBT (tipZ p) {[]} (drop s) = p
fromBT (tipS p)      (drop s) = ⊥-elim ((1+n≰n (⊆-length s)))
fromBT (bin t u)     (drop s) = fromBT t s
