{-# OPTIONS --safe --large-indices --no-forced-argument-recursion #-}

-- Agda version: 2.6.4.1
-- Standard library version: 2.0

module UniquenessOfInductionPrinciples where

open import Function
open import Data.Empty
open import Data.Unit
open import Data.Product
open import Data.Nat
open import Data.Nat.Properties
open import Data.Vec
open import Relation.Binary.PropositionalEquality

module Nat where

  Ind : Set₁
  Ind = (P : ℕ → Set) → P zero → (∀ {n} → P n → P (suc n)) → (n : ℕ) → P n

  ind : Ind
  ind P pz ps  zero   = pz
  ind P pz ps (suc n) = ps (ind P pz ps n)

  Ind-unary-parametricity : Ind → Set₁
  Ind-unary-parametricity f =
      (P : ℕ → Set) (Q : ∀ {n} → P n → Set)
    → (pz : P zero)                  → Q pz
    → (ps : ∀ {n} → P n → P (suc n)) → (∀ {n} {p : P n} → Q p → Q (ps p))
    → (n : ℕ) → Q (f P pz ps n)

  ind-parametric : Ind-unary-parametricity ind
  ind-parametric P Q pz qz ps qs = ind (Q ∘ ind P pz ps) qz qs

  uniqueness :
      (f : Ind) → Ind-unary-parametricity f
    → (P : ℕ → Set) (pz : P zero) (ps : ∀ {n} → P n → P (suc n))
    → (n : ℕ) → f P pz ps n ≡ ind P pz ps n
  uniqueness f param P pz ps n =
    param P (λ {n} p → p ≡ ind P pz ps n) pz refl ps (cong ps) n

module ImmediateSublists where

  variable
    A     : Set
    m n k : ℕ
    P Q   : Vec _ _ → Set
    x     : A
    xs    : Vec _ _

  data BT : (n k : ℕ) → (Vec A k → Set) → Vec A n → Set where
    tipZ : P []                        → BT      n   zero   P xs
    tipS : P xs                        → BT (suc n) (suc n) P xs
    bin  : BT n (suc k) P           xs
         → BT n      k (P ∘ (x ∷_)) xs → BT (suc n) (suc k) P (x ∷ xs)

  bounded : BT n k P xs → n ≥ k
  bounded (tipZ _)  = z≤n
  bounded (tipS _)  = ≤-refl
  bounded (bin _ u) = s≤s (bounded u)

  unbounded : BT n (suc n) P xs → ⊥
  unbounded t = ≤⇒≯ (bounded t) ≤-refl

  mapBT : (∀ {xs} → P xs → Q xs) → ∀ {xs} → BT n k P xs → BT n k Q xs
  mapBT f (tipZ p)  = tipZ (f p)
  mapBT f (tipS p)  = tipS (f p)
  mapBT f (bin t u) = bin (mapBT f t) (mapBT f u)

  Ind : Set₁
  Ind = {A : Set} (S : ∀ {n} → Vec A n → Set)
      → S [] → (∀ {k xs} → BT (suc k) k S xs → S xs)
      → (n : ℕ) (xs : Vec A n) → S xs

  blanks : (n : ℕ) → {xs : Vec A (suc n)} → BT (suc n) n (const ⊤) xs
  blanks  zero           = tipZ tt
  blanks (suc n) {_ ∷ _} = bin (tipS tt) (blanks n)

  td : Ind
  td S e g  zero   [] = e
  td S e g (suc n) xs = g (mapBT (const (td S e g n _)) (blanks n))

  BT' : (Q : {ys : Vec A k} → P ys → Set) → {xs : Vec A n} → BT n k P xs → Set
  BT' Q (tipZ p)  = Q p
  BT' Q (tipS p)  = Q p
  BT' Q (bin t u) = BT' Q t × BT' Q u

  Ind-unary-parametricity : Ind → Set₁
  Ind-unary-parametricity f =
      {A : Set} (S : ∀ {n} → Vec A n → Set) (T : ∀ {n} {xs : Vec A n} → S xs → Set)
    → (e : S []) → T e
    → (g : ∀ {k xs} → BT (suc k) k S xs → S xs)
    → (∀ {k xs} {t : BT (suc k) k S xs} → BT' T t → T (g t))
    → (n : ℕ) (xs : Vec A n) → T (f S e g n xs)

  _+′_ : ℕ → ℕ → ℕ
  zero  +′ n = n
  suc m +′ n = m +′ suc n

  revcat : Vec A m → Vec A n → Vec A (m +′ n)
  revcat []       ys = ys
  revcat (x ∷ xs) ys = revcat xs (x ∷ ys)

  td-parametric : Ind-unary-parametricity td
  td-parametric {A} S T e e' g g' = td (T ∘ td S e g _) e' (g' ∘ see-below [] (td S e g _))
    where
      see-below : ∀ {m n xs} (ys : Vec A m) (h : (zs : Vec A n) → S (revcat ys zs))
                → BT (suc n) n (T ∘ h) xs
                → BT' T {xs} (mapBT (λ {zs} _ → h zs) (blanks n))
      see-below ys h (tipZ t)           = t
      see-below ys h (bin (tipS t) ts)  = t , see-below (_ ∷ ys) (h ∘ (_ ∷_)) ts
      see-below ys h (bin (bin ts _) _) = ⊥-elim (unbounded ts)

  uniqueness :
      (f : Ind) → Ind-unary-parametricity f
    → {A : Set} (S : ∀ {n} → Vec A n → Set)
    → (e : S []) (g : ∀ {k xs} → BT (suc k) k S xs → S xs)
    → (n : ℕ) (xs : Vec A n) → f S e g n xs ≡ td S e g n xs
  uniqueness f param {A} S e g =
    param S (λ {n} {xs} s → s ≡ td S e g n xs) e refl g (λ t' → cong g (see-below S (td S e g _) _ t'))
    where
      see-below : (S' : Vec A k → Set) (h : (xs : Vec A k) → S' xs)
                → (t : BT (suc k) k S' xs) → BT' (λ {xs} s → s ≡ h xs) t
                → t ≡ mapBT (λ {xs} _ → h xs) (blanks k)
      see-below S' h (tipZ s)          eq       = cong tipZ eq
      see-below S' h (bin (tipS s) t) (eq , t') = cong₂ bin (cong tipS eq) (see-below (S' ∘ (_ ∷_)) (h ∘ (_ ∷_)) t t')
      see-below S' h (bin (bin t _) _) _        = ⊥-elim (unbounded t)
