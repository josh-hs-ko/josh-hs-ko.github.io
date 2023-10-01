open import Function
open import Data.Empty
open import Data.Nat
open import Data.Nat.Properties
open import Data.List as L hiding (map)
open import Data.Vec as V hiding (map)
open import Data.Vec.Relation.Unary.All hiding (map)


variable
  A B : Set
  k n : ℕ
  x   : A
  xs  : Vec A _
  P Q : A → Set


choose₀ : (n k : ℕ) → n ≥′ k → Vec A n → List (Vec A k)
choose₀ _        zero    _                   _   = L.[ [] ]
choose₀ (suc k) (suc k)  ≤′-refl             xs  = L.[ xs ]
choose₀ (suc n) (suc k) (≤′-step n≥1+k) (x ∷ xs) =
  choose₀ n (suc k) n≥1+k xs L.++ L.map (x ∷_) (choose₀ n k n≥k xs)
  where n≥k = ≤′-trans (≤′-step ≤′-refl) n≥1+k

td₀ : {A S : Set} (f : A → S) (g : List S → S) {n : ℕ} → Vec A (suc n) → S
td₀ f g {zero } = f ∘ V.head
td₀ f g {suc n} = g ∘ L.map (td₀ f g {n}) ∘ choose₀ (2 + n) (1 + n) 2+n≥1+n
  where 2+n≥1+n = ≤′-step ≤′-refl


module BT₁ where

  data BT₁ (A : Set) : ℕ → ℕ → Set where
    tipL : A                           → BT₁ A      n       0
    tipR : A                           → BT₁ A (suc n) (suc n)
    bin  : BT₁ A n (suc k) → BT₁ A n k → BT₁ A (suc n) (suc k)

  map : (A → B) → BT₁ A n k → BT₁ B n k
  map f (tipL x)  = tipL (f x)
  map f (tipR x)  = tipR (f x)
  map f (bin t u) = bin (map f t) (map f u)

open BT₁ using (BT₁; tipL; tipR; bin)

choose₁ : (n k : ℕ) → n ≥′ k → Vec A n → BT₁ (Vec A k) n k
choose₁ _        zero    _                   _   = tipL []
choose₁ (suc k) (suc k)  ≤′-refl             xs  = tipR xs
choose₁ (suc n) (suc k) (≤′-step n≥1+k) (x ∷ xs) =
  bin (choose₁ n (suc k) n≥1+k xs) (BT₁.map (x ∷_) (choose₁ n k n≥k xs))
  where n≥k = ≤′-trans (≤′-step ≤′-refl) n≥1+k

td₁ : {A S : Set} (f : A → S) (g : ∀ {k} → BT₁ S (suc k) k → S) {n : ℕ} → Vec A (suc n) → S
td₁ f g {zero } = f ∘ V.head
td₁ f g {suc n} = g ∘ BT₁.map (td₁ f g {n}) ∘ choose₁ (2 + n) (1 + n) 2+n≥1+n
  where 2+n≥1+n = ≤′-step ≤′-refl


_⇉_ : (A → Set) → (A → Set) → (A → Set)
(P ⇉ Q) x = P x → Q x

infixr 2 _⇉_

∀[_] : (A → Set) → Set
∀[ P ] = ∀ {x} → P x

∀⟨_⟩[_] : (A : Set) → (A → Set) → Set
∀⟨ A ⟩[ P ] = {x : A} → P x

module BT where

  data BT : (n k : ℕ) (P : Vec A k → Set) → Vec A n → Set where

    tipL : P [] → BT      n       0  P xs
    tipR : P xs → BT (suc n) (suc n) P xs

    bin  : BT      n  (suc k)        P           xs
         → BT      n       k (λ ys → P (x ∷ ys)) xs
         → BT (suc n) (suc k)        P      (x ∷ xs)

  map : ∀[ P ⇉ Q ] → ∀[ BT n k P ⇉ BT n k Q ]
  -- map : (∀ {xs} → P xs → Q xs) → ∀ {xs} → BT n k P xs → BT n k Q xs
  map f (tipL p)  = tipL (f p)
  map f (tipR p)  = tipR (f p)
  map f (bin t u) = bin (map f t) (map f u)

open BT using (BT; tipL; tipR; bin)

choose : (n k : ℕ) → n ≥′ k → ∀[ All P ⇉ BT n k (All P) ]
choose _        zero    _                   _   = tipL []
choose (suc k) (suc k)  ≤′-refl             ps  = tipR ps
choose (suc n) (suc k) (≤′-step n≥1+k) (p ∷ ps) =
  bin (choose n (suc k) n≥1+k ps) (BT.map (p ∷_) (choose n k n≥k ps))
  where n≥k = ≤′-trans (≤′-step ≤′-refl) n≥1+k

head′ : ∀⟨ Vec A 1 ⟩[ All (P ∘ V.[_]) ⇉ P ]
head′ (p ∷ []) = p

td : {A : Set} {S : ∀ {k} → Vec A (suc k) → Set}
   → (g : ∀ {k} → ∀[ BT (2 + k) (1 + k) S ⇉ S ])
   → ∀ {n} → ∀⟨ Vec A (suc n) ⟩[ All (S ∘ V.[_]) ⇉ S ]
td {S = S} g {zero } = head′ {P = S}
td         g {suc n} = g ∘ BT.map (td g {n}) ∘ choose (2 + n) (1 + n) 2+n≥1+n
  where 2+n≥1+n = ≤′-step ≤′-refl

postulate
  upgrade : n > k → ∀[ BT n k P ⇉ BT n (suc k) (BT (suc k) k P) ]

bounded : BT n k P xs → n ≥ k
bounded (tipL _)  = z≤n
bounded (tipR _)  = ≤-refl
bounded (bin _ u) = s≤s (bounded u)

unbounded : BT n (suc n) P xs → ⊥
unbounded t = ≤⇒≯ (bounded t) ≤-refl

unTip : ∀[ BT n n P ⇉ P ]
unTip (tipL {xs = []} p) = p
unTip (tipR p)  = p
unTip (bin t _) = ⊥-elim (unbounded t)

bu : {A : Set} {S : ∀ {k} → Vec A (suc k) → Set}
   → (g : ∀ {k} → ∀[ BT (2 + k) (1 + k) S ⇉ S ])
   → ∀ {n} → ∀⟨ Vec A (suc n) ⟩[ All (S ∘ V.[_]) ⇉ S ]
bu {S = S} g =
  unTip ∘ loop (≤⇒≤‴ z≤n) ∘ BT.map (head′ {P = S}) ∘ choose _ 1 (≤⇒≤′ (s≤s z≤n))
  where
    loop : n ≥‴ k → ∀[ BT (suc n) (suc k) S ⇉ BT (suc n) (suc n) S ]
    loop  ≤‴-refl        = id
    loop (≤‴-step n≥1+k) = loop n≥1+k ∘ BT.map g ∘ upgrade (s≤s (≤‴⇒≤ n≥1+k))
