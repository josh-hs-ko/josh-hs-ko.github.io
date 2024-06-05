-- Checked with Agda 2.6.4.3 and Standard Library 2.0

{-# OPTIONS --safe --guardedness --large-indices --no-forced-argument-recursion #-}

module AlwaysInhabited where

open import Function
open import Data.Unit
open import Data.Empty.Irrelevant
open import Data.Nat
open import Data.Nat.Properties
open import Data.List hiding (drop)
open import Relation.Binary.PropositionalEquality


variable
  a b   : Set
  x y   : a
  p q r : a → Set

pure : a → List a
pure x = x ∷ []

_<$>_ : (a → b) → List a → List b
f <$> xs = Data.List.map f xs

empty : List a
empty = []

_<|>_ : List a → List a → List a
_<|>_ = Data.List._++_


module UsingVec where

  open import Data.Vec

  variable
    k m n : ℕ
    xs    : Vec a n

  data BT : (n k : ℕ) → (Vec a k → Set) → Vec a n → Set where
    tipZ : p []                        → BT n        zero   p xs
    nil  : .⦃ n < suc k ⦄              → BT n       (suc k) p xs
    tipS : p xs                        → BT (suc k) (suc k) p xs
    bin  : .⦃ n > k ⦄
        → BT n (suc k) p           xs
        → BT n      k (p ∘ (x ∷_)) xs → BT (suc n) (suc k) p (x ∷ xs)

  module Choose where

    open import Relation.Binary.Definitions

    choose : (n k : ℕ) → Vec a n → List (Vec a k)
    choose n        zero   xs       = pure []
    choose n       (suc k) xs       with <-cmp n (suc k)
    choose n       (suc k) xs       | tri< n<sk _ _      = empty
    choose (suc k) (suc k) xs       | tri≈ _ refl _      = pure xs
    choose (suc n) (suc k) (x ∷ xs) | tri> _ _ (s≤s n>k) = choose n (suc k) xs <|> ((x ∷_) <$> choose n k xs)

  mapBT : (∀ {ys} → p ys → q ys) → ∀ {xs} → BT n k p xs → BT n k q xs
  mapBT f (tipZ z)  = tipZ (f z)
  mapBT f  nil      = nil
  mapBT f (tipS z)  = tipS (f z)
  mapBT f (bin t u) = bin (mapBT f t) (mapBT f u)

  unTip : BT n n p xs → p xs
  unTip           (tipS y)  = y
  unTip {xs = []} (tipZ y)  = y
  -- impossible cases
  unTip            nil      = ⊥-elim (<-irrefl refl it)
  unTip           (bin _ _) = ⊥-elim (<-irrefl refl it)

  zipBTWith : (∀ {ys} → p ys → q ys → r ys) → ∀ {xs} → BT n k p xs → BT n k q xs → BT n k r xs
  zipBTWith f (tipZ z)   (tipZ w)   = tipZ (f z w)
  zipBTWith f  nil        nil       = nil
  zipBTWith f (tipS z)   (tipS w)   = tipS (f z w)
  zipBTWith f (bin t t') (bin u u') = bin (zipBTWith f t u) (zipBTWith f t' u')
  -- impossible cases
  zipBTWith f  nil       (tipS _)   = ⊥-elim (<-irrefl refl it)  -- SMT solving?
  zipBTWith f  nil       (bin _ _)  = ⊥-elim (≤⇒≯ (<⇒≤ (≤-pred it)) it)
  zipBTWith f (tipS _)    nil       = ⊥-elim (<-irrefl refl it)
  zipBTWith f (tipS _)   (bin _ _)  = ⊥-elim (1+n≰n it)
  zipBTWith f (bin _ _)   nil       = ⊥-elim (≤⇒≯ (<⇒≤ it) (≤-pred it))
  zipBTWith f (bin _ _)  (tipS _)   = ⊥-elim (1+n≰n it)

  instance
    _ : n ≤ n
    _ = ≤-refl

  _∷ᴮᵀ_ : p xs → BT (1 + k) k (p ∘ (x ∷_)) xs → BT (2 + k) (1 + k) p (x ∷ xs)
  y ∷ᴮᵀ t = bin (tipS y) t

  retabulate : BT n k p xs → BT n (1 + k) (BT (1 + k) k p) xs
  retabulate                   nil           = nil ⦃ m≤n⇒m≤1+n it ⦄
  retabulate {xs = []       } (tipZ y)       = nil
  retabulate {xs = _ ∷ []   } (tipZ y)       = tipS (tipZ y)
  retabulate {xs = _ ∷ _ ∷ _} (tipZ y)       = bin ⦃ s≤s z≤n ⦄ (retabulate (tipZ y)) (tipZ (tipZ y))
  retabulate                  (tipS y)       = nil
  retabulate (bin   (tipS y)    u          ) = tipS (y ∷ᴮᵀ u)
  retabulate (bin t@(bin _ _)     (tipZ z) ) = bin ⦃ s≤s it ⦄ (retabulate t) (mapBT (_∷ᴮᵀ (tipZ z)) t)
  retabulate (bin t@(bin _ _)   u@(bin _ _)) = bin ⦃ s≤s it ⦄ (retabulate t) (zipBTWith _∷ᴮᵀ_ t (retabulate u))
  -- impossible cases
  retabulate (bin   (nil ⦃ l ⦄) u          ) = ⊥-elim (1+n≰n (≤-trans l it))
  retabulate (bin t@(bin _ _)     (tipS _) ) = ⊥-elim (1+n≰n it)
  retabulate (bin t@(bin _ _)      nil     ) = ⊥-elim (≤⇒≯ (<⇒≤ it) it)

module UsingList where

  variable
    xs ys : List a

  infix 2 _⊆_

  data _⊆_ {a : Set} : List a → List a → Set where
    nil  :               [] ⊆ []
    keep : ys ⊆ xs → x ∷ ys ⊆ x ∷ xs
    drop : ys ⊆ xs →     ys ⊆ x ∷ xs

  data BT' : (List a → Set) → List a → Set where
    tip : p []                       → BT' p []
    bin : BT' (λ ys → p (x ∷ ys)) xs
        → BT'         p           xs → BT' p (x ∷ xs)

  -- testBT' : BT' p (0 ∷ 1 ∷ 2 ∷ [])
  -- testBT' = bin (bin (bin (tip {!   !}) (tip {!   !})) (bin (tip {!   !}) (tip {!   !}))) (bin (bin (tip {!   !}) (tip {!   !})) (bin (tip {!   !}) (tip {!   !})))

  mutual

    record ST (p : List a → Set) (ys : List a) : Set where
      coinductive
      constructor st
      field
        decon : STF p ys

    data STF : (List a → Set) → List a → Set where
      nil  : p []
           → ST (λ xs → p (x ∷ xs)) []        → STF p []
      cons : ST (λ xs → p (y ∷ xs))      ys
           → ST (λ xs → p (x ∷ xs)) (y ∷ ys)  → STF p (y ∷ ys)

  -- testST : ST p (0 ∷ 1 ∷ 2 ∷ [])
  -- testST = st (cons (st (cons {!   !} (st (cons (st (cons {!   !} (st (cons {!   !} {!   !})))) {!   !})))) {!   !})

  sublist : List a → List (List a)
  sublist []       = pure []
  sublist (x ∷ xs) = ((x ∷_) <$> sublist xs) <|> sublist xs
