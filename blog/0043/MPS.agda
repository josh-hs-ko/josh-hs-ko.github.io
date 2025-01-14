-- Checked with Agda 2.6.4.3 and Standard Library 2.0

{-# OPTIONS --safe #-}

open import Function using (id; _∘_; const; flip)
open import Data.Unit using (⊤)
open import Data.Product using (Σ; _,_; proj₁; Σ-syntax; _×_; map-Σ; map₂)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Data.List using (List; []; _∷_; foldr)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong)

variable
  A B   : Set
  P Q   : A → Set
  x     : A
  xs ys : List A

record Exactly {A : Set} (x : A) : Set where
  constructor exactly
  field
    payload      : A
    ⦃ equality ⦄ : payload ≡ x

mapExactly : (f : A → B) → Exactly x → Exactly (f x)
mapExactly f (exactly x ⦃ eq ⦄) = exactly (f x) ⦃ cong f eq ⦄

data Init {A : Set} : List A → List A → Set where
  nil       :              Init []       []
  cons-nil  :              Init (x ∷ xs) []
  cons-cons : Init xs ys → Init (x ∷ xs) (x ∷ ys)

module InductiveInitC where

  data InitC {A : Set} (P : List A → Set) : List A → Set where
    nil       : P []                  → InitC P []
    cons-nil  : P []                  → InitC P (x ∷ xs)
    cons-cons : InitC (P ∘ (x ∷_)) xs → InitC P (x ∷ xs)

record InitC {A : Set} (P : List A → Set) (xs : List A) : Set where
  constructor initC
  field
    {prefix} : List A
    isPrefix : Init xs prefix
    payload  : P prefix

record MaxFam {A : Set} (P : A → Set) : Set₁ where
  constructor maxFam
  field
    _≤_     : ∀ {x y} → P x → P y → Set
    ≤-refl  : ∀ {x} {p : P x} → p ≤ p
    ≤-trans : ∀ {x y z} {p : P x} {q : P y} {r : P z} → p ≤ q → q ≤ r → p ≤ r
    ≤-dec   : ∀ {x y} (p : P x) (q : P y) → p ≤ q ⊎ q ≤ p

MaxFam-fun : (f : A → B) → MaxFam P → MaxFam (P ∘ f)
MaxFam-fun f (maxFam _≤_ ≤-refl ≤-trans dec) = maxFam _≤_ ≤-refl ≤-trans dec

MaxFam-Exactly : MaxFam {⊤} (const B) → MaxFam (Exactly {B})
MaxFam-Exactly mf = let open MaxFam mf in record
  { _≤_     = λ (exactly x) (exactly y) → x ≤ y
  ; ≤-refl  = ≤-refl
  ; ≤-trans = ≤-trans
  ; ≤-dec   = λ (exactly x) (exactly y) → ≤-dec x y }

data Inits (P : List A → Set) : List A → Set where
  [_] : P []                         → Inits P []
  _∷_ : P [] → Inits (P ∘ (x ∷_)) xs → Inits P (x ∷ xs)

fromInits : Inits P xs → (ys : List A) → Init xs ys → P ys
fromInits [ p ]    _  nil          = p
fromInits (p ∷ ps) _  cons-nil     = p
fromInits (p ∷ ps) _ (cons-cons i) = fromInits ps _ i

toInits : ((ys : List A) → Init xs ys → P ys) → Inits P xs
toInits {xs = []    } f = [ f _ nil ]
toInits {xs = x ∷ xs} f = f _ cons-nil ∷ toInits (λ _ i → f _ (cons-cons i))

mapInits : (∀ {ys} → P ys → Q ys) → ∀ {xs} → Inits P xs → Inits Q xs
mapInits f [ p ]    = [ f p ]
mapInits f (p ∷ ps) = f p ∷ mapInits f ps

inits : (xs : List A) → Inits Exactly xs
inits []       = [ exactly [] ]
inits (x ∷ xs) = exactly [] ∷ mapInits (mapExactly (x ∷_)) (inits xs)

maxInits : {P : List A → Set} (mf : MaxFam P) → let open MaxFam mf in
           Inits P xs → Σ[ (initC _ q) ∈ InitC P xs ] Inits (λ ys → Σ[ p ∈ P ys ] p ≤ q) xs
maxInits mf [ p ]    = initC nil p , [ (p , mf .MaxFam.≤-refl) ]
maxInits mf (p ∷ ps) with maxInits (MaxFam-fun (_ ∷_) mf) ps
maxInits mf (p ∷ ps) | initC i q , ps' with mf .MaxFam.≤-dec p q
maxInits mf (p ∷ ps) | initC i q , ps' | inj₁ p≤q =
  initC (cons-cons i) q , (p , p≤q) ∷ ps'
maxInits mf (p ∷ ps) | initC i q , ps' | inj₂ q≤p =
  initC cons-nil p ,
  (p , mf .MaxFam.≤-refl) ∷ mapInits (map₂ (flip (mf .MaxFam.≤-trans) q≤p)) ps'

module _
  (Val : Set)
    (MaxFam-Val : MaxFam {⊤} (const Val))
    (zero : Val)
    (_+_ : Val → Val → Val)
      (+-mono-r : let open MaxFam MaxFam-Val in
                  ∀ x {y z} → y ≤ z → (x + y) ≤ (x + z))
  where

  open MaxFam MaxFam-Val

  sum : List Val → Val
  sum = foldr _+_ zero

  PrefixSums : Set
  PrefixSums = (xs ys : List Val) → Init xs ys → Exactly (sum ys)

  IndPrefixSums : Set
  IndPrefixSums = (xs : List Val) → Inits (Exactly ∘ sum) xs

  prefixSums : IndPrefixSums
  prefixSums = mapInits (mapExactly sum) ∘ inits

  prefixSums' : IndPrefixSums
  prefixSums' []       = [ exactly zero ]
  prefixSums' (x ∷ xs) = exactly zero ∷ mapInits (mapExactly (x +_)) (prefixSums' xs)

  IndPrefixSums-soundness : IndPrefixSums → PrefixSums
  IndPrefixSums-soundness = fromInits ∘_

  MaxPrefixSum : Set
  MaxPrefixSum = (xs : List Val)
               → Σ[ m ∈ Val ] Σ[ ys ∈ List Val ] Init xs ys × m ≡ sum ys
               × ((zs : List Val) → Init xs zs → sum zs ≤ m)

  IndMaxPrefixSum : Set
  IndMaxPrefixSum = (xs : List Val)
                  → Σ[ (initC _ (exactly m)) ∈ InitC (Exactly ∘ sum) xs ]
                    Inits (λ ys → Σ[ (exactly n) ∈ Exactly (sum ys) ] n ≤ m) xs

  IndMaxPrefixSum-soundness : IndMaxPrefixSum → MaxPrefixSum
  IndMaxPrefixSum-soundness f xs =
    let (initC {ys} i (exactly m ⦃ meq ⦄) , ps) = f xs
    in  m , ys , i , meq , λ zs j → see-below zs (fromInits ps zs j)
    where
      see-below : {m : Val} (zs : List Val)
                → Σ[ (exactly n) ∈ Exactly (sum zs) ] n ≤ m → sum zs ≤ m
      see-below _ (exactly _ ⦃ refl ⦄ , ineq) = ineq

  maxPrefixSum : IndMaxPrefixSum
  maxPrefixSum = maxInits (MaxFam-fun sum (MaxFam-Exactly MaxFam-Val))
               ∘ mapInits (mapExactly sum) ∘ inits

  maxPrefixSum' : IndMaxPrefixSum
  maxPrefixSum' [] = initC nil (exactly zero) , [ (exactly zero , ≤-refl) ]
  maxPrefixSum' (x ∷ xs) with maxPrefixSum' xs
  maxPrefixSum' (x ∷ xs) | initC i (exactly m) , ps with ≤-dec (x + m) zero
  maxPrefixSum' (x ∷ xs) | initC i em , ps | inj₁ x+m≤0 =
    initC cons-nil (exactly zero) ,
    (exactly zero , ≤-refl) ∷ mapInits (map-Σ (mapExactly (x +_)) (flip ≤-trans x+m≤0 ∘ +-mono-r x)) ps
  maxPrefixSum' (x ∷ xs) | initC i em , ps | inj₂ 0≤x+m =
    initC (cons-cons i) (mapExactly (x +_) em) ,
    (exactly zero , 0≤x+m) ∷ mapInits (map-Σ (mapExactly (x +_)) (+-mono-r x)) ps

  maxPrefixSum'' : MaxPrefixSum
  maxPrefixSum'' [] = zero , _ , nil , refl , λ { _ nil → ≤-refl }
  maxPrefixSum'' (x ∷ xs) with maxPrefixSum'' xs
  maxPrefixSum'' (x ∷ xs) | m , _ , i , meq , f with ≤-dec (x + m) zero
  maxPrefixSum'' (x ∷ xs) | m , _ , i , meq , f | inj₁ x+m≤0 =
    zero , _ , cons-nil , refl ,
    λ { _ cons-nil → ≤-refl; _ (cons-cons j) → ≤-trans (+-mono-r x (f _ j)) x+m≤0 }
  maxPrefixSum'' (x ∷ xs) | m , ys , i , meq , f | inj₂ 0≤x+m =
    (x + m) , _ , cons-cons i , cong (x +_) meq ,
    λ { _ cons-nil → 0≤x+m; _ (cons-cons j) → +-mono-r x (f _ j) }
