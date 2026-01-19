-- Checked with Agda 2.8.0 and Standard Library 2.3

{-# OPTIONS --safe --with-K #-}

open import Function
open import Data.Product
open import Relation.Binary.PropositionalEquality

module WG21-Sep25 where
-- Specifying queue order using parametric types: some concrete experiments
-- Josh Ko (Academia Sinica, Taiwan)
-- Viana do Castelo, Portugal, 9 Sep 2025

variable
  A B C      : Set
  P          : A → Set
  R S T      : A → A → Set
  l m n u u' : A

module SimplyTypedTwoListQueue (A : Set) where

  open import Data.Maybe
  open import Data.List

  State : Set
  State = List A × List A

  empty : State
  empty = [] , []

  push : A → State → State
  push x (front , rear) = front , x ∷ rear

  pop : State → Maybe (A × State)
  pop ([]        , rear) with reverse rear
  pop ([]        , rear) | []     = nothing
  pop ([]        , rear) | x ∷ xs = just (x , (xs , []))
  pop (x ∷ front , rear) = just (x , (front , rear))

  wrongPop : State → Maybe (A × State)
  wrongPop (front , rear) with reverse rear
  wrongPop (front , rear) | []     = nothing
                                     -- forget the front elements
  wrongPop (front , rear) | x ∷ xs = just (x , (front ++ xs , []))
                                     -- wrong order

  -- Substructural typing?
  -- Want a less drastic solution (in Agda)

module McBride-ICFP14 where
  -- Includes a generic definition of ordered datatypes

  -- An inhabitant of OList A _≤_ l u looks like
  --   ‘l ≤ x₀ ≤ x₁ ≤ x₂ ≤ x₃ ≤ u’
  data OList (A : Set) (R : A → A → Set) : A → A → Set where
    []  :           ⦃ R l u ⦄                 → OList A R l u
    _∷_ : (x : A) → ⦃ R l x ⦄ → OList A R x u → OList A R l u

  infixr 5 _∷_

  open import Data.Maybe
  open import Data.Nat

  -- First problem: don’t want to order the elements by their value
  -- Solution: order the elements by their time of insertion
  Seq : Set → ℕ → ℕ → Set
  Seq A l u = OList (Maybe A × ℕ) (λ (_ , t) (_ , t') → t < t')
                    (nothing , l) (nothing , u)

  -- Alternatively: index (and distinguish) the element types by time
  --                (see module IndexedElementTypes below)

  -- Second problem: awkward function definitions

  -- To write the usual list append, we’d have to write some proofs manually.
  -- For example, given ‘l ≤ x₀ ≤ x₁ ≤ m’ and ‘m ≤ x₂ ≤ x₃ ≤ u’
  --           we want  ‘l ≤ x₀ ≤ x₁ ≤ x₂ ≤ x₃ ≤ u’
  --          but have  ‘l ≤ x₀ ≤ x₁ ≤ m ≤ x₂ ≤ x₃ ≤ u’.
  -- _++_ : OList A R l m → OList A R m u → OList A R l u
  -- []       ++ ys = {!  !}          -- requires transitivity of R
  -- (x ∷ xs) ++ ys = x ∷ (xs ++ ys)  -- proof handled by instance resolution

  -- Conor insisted on writing no proofs and (luckily) the following version
  --   sufficed for them.
  -- This is not a solution for us, but there are lessons to be learned:
  --   if we use/assume no laws of R, we are pretty much forced to write
  --   this definition — the type acts as a pretty tight specification.
  append' : (m : A) → OList A R l m → OList A R m u → OList A R l u
  append' m []       ys = m ∷ ys
  append' m (x ∷ xs) ys = x ∷ append' m xs ys

  -- How many inhabitants does this type have?
  -- f : OList A R l u → OList A R l u
  -- f []           = []
  -- f (x ∷ [])     = {!  !}
  -- f (x ∷ y ∷ xs) = {!  !}

  -- Transitivity ~ deleting
  -- Reflexivity  ~ copying
  -- Symmetry     ~ reordering

  -- Managing list elements as resources without a substructural type system
  -- More precise relationship to substructural typing? Another time, perhaps.
  --
  -- Zhixuan Yang (Imperial, soon Exeter 🎉) last night: directly working with
  -- the monoidal model A → A → Set where the monoidal product is
  --
  --   (R □ S) a b = Σ[ c ∈ A ] R a c × S c b
  --
  -- with no symmetry, projection, or duplication

module IndexedElementTypes where

  data Seq {Time : Set} (P : Time → Set) (R : Time → Time → Set)
        : Time → Time → Set where
    []  :       ⦃ R l u ⦄               → Seq P R l u
    _∷_ : P m → ⦃ R l m ⦄ → Seq P R m u → Seq P R l u

-- The solution that came to me: ordered lists like
--   ‘l ≤ x₀ ≤ m₀ ≤ x₁ ≤ m₁ ≤ x₂ ≤ m₂ ≤ x₃ ≤ u’
data OList (A : Set) (R : A → A → Set) : A → A → Set where
  []  :                                                   OList A R u u
  _∷_ : (x : A) → ⦃ R l x ⦄ → ⦃ R x m ⦄ → OList A R m u → OList A R l u

--   ‘l ≤ x₀ ≤ m₀ ≤ x₁ ≤ m’ ++ ‘m ≤ x₂ ≤ m₂ ≤ x₃ ≤ u’
-- = ‘l ≤ x₀ ≤ m₀ ≤ x₁ ≤ m ≤ x₂ ≤ m₂ ≤ x₃ ≤ u’
_++_ : OList A R l m → OList A R m u → OList A R l u
[]       ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

-- slowReverse ‘l ≤ x₀ ≤ m₀ ≤ x₁ ≤ ⋯ ≤ u’ = ‘u ≥ ⋯ ≥ x₁ ≥ m₀ ≥ x₀ ≥ l’
slowReverse : OList A R l u → OList A (flip R) u l
slowReverse []       = []
slowReverse (x ∷ xs) = slowReverse xs ++ (x ∷ [])

-- revcat ‘l ≤ ⋯ ≤ m ≤ ⋯ ≤ u’ [] = revcat ‘m ≤ ⋯ ≤ u’ ‘m ≥ ⋯ ≥ l’ = ⋯
revcat : OList A R m u → OList A (flip R) m l → OList A (flip R) u l
revcat []       ys = ys
revcat (x ∷ xs) ys = revcat xs (x ∷ ys)

reverse : OList A R l u → OList A (flip R) u l
reverse xs = revcat xs []

module TwoListQueue (A : Set) (R : A → A → Set) where

  record State (l u : A) : Set where
    constructor _,_
    field
      {mid} : A
      front : OList A R l mid
      rear  : OList A (flip R) u mid

  empty : State u u
  empty = [] , []

  push : (x : A) → ⦃ R u x ⦄ → ⦃ R x u' ⦄ → State l u → State l u'
  push x (front , rear) = front , x ∷ rear

  data OListF (X : A → A → Set) : A → A → Set where
    nil  :                                           OListF X u u
    cons : (x : A) → ⦃ R l x ⦄ → ⦃ R x m ⦄ → X m u → OListF X l u

  pop : State l u → OListF State l u
  pop ([]        , rear) with reverse rear
  pop ([]        , rear) | []     = nil
  pop ([]        , rear) | x ∷ xs = cons x (xs , [])
  pop (x ∷ front , rear) = cons x (front , rear)

  -- wrongPop : State l u → OListF State l u
  -- wrongPop (front , rear) with reverse rear
  -- wrongPop (front , rear) | []     = {! nil !}
  -- wrongPop (front , rear) | x ∷ xs = {! cons x (front ++ xs , []) !}

-- When we use the queue, set the element type to Maybe A (so that we can
-- supply ‘nothing’ as separators) and R to λ _ _ → ⊤.

-- Time to write some proofs! A simplest exercise is that the type of reverse
--
ReverseType : Set₁
ReverseType = {A : Set} {R : A → A → Set} {l u : A}
            → OList A R l u → OList A (flip R) u l
--
-- is uniquely inhabited (up to extensional equality).

-- We know that, for example, reverse (or slowReverse) inhabits this type,
-- so anything we know about the extensional behaviour of reverse will hold
-- for all other functions of the same type.

-- I learned the proof idea from
--
-- * Janis Voigtländer [2009]. Bidirectionalization for free!
--   In Symposium on Principles of Programming Languages (POPL),
--   pages 165–176. ACM. https://doi.org/10.1145/1480881.1480904.
--
-- Long story short: how do we determine the behaviour of a function
-- f : ∀ {A} → List A → List A from one of its concrete instances?
--
-- Given xs : List A,
--
-- * associate each element of xs with a unique ID, say a natural number,
-- * collect those IDs in another list ns : List ℕ, and
-- * observe f {ℕ} ns, the output IDs, which have to be arranged in the
--     same way as f {A} xs due to parametricity.
--
-- More formally, the associations can be represented as a function
-- assoc : ℕ → A such that map assoc ns ≡ xs, and parametricity
-- (free theorem) will give us
--
--   f {A} xs ≡ f {A} (map assoc ns) ≡ map assoc (f {ℕ} ns).

-- In the case of OList A R, think of A and R as two kinds of element type,
-- and the idea still largely works: for any rev, rev' : ReverseType and
-- xs : OList A R l u, find a list ns and functions assocA and assocR
-- such that mapAR assocA assocR ns ≡ xs, and then
--
--   rev xs
--     ≡ {- specification of ns -}
--   rev (mapAR assocA assocR ns)
--     ≡ {- free theorem -}
--   mapAR assocA (flip assocR) (rev  ns)
--     ≡ {- see below -}
--   mapAR assocA (flip assocR) (rev' ns)
--     ≡ {- free theorem -}
--   rev' (mapAR assocA assocR ns)
--     ≡ {- specification of ns -}
--   rev' xs
--
-- In the middle we need rev ns and rev' ns to be equal: when constructing ns,
-- if we choose concrete types like A = ℕ and R x y = (suc x ≡ y), then the
-- type OList A R l u will fully determine the elements in its inhabitant,
-- so rev ns and rev' ns are equal simply because they have the same type.

module Uniqueness where

  mapAR : (f : A → B) → (∀ x y → R x y → S (f x) (f y)) →
          OList A R l u → OList B S (f l) (f u)
  mapAR f g []       = []
  mapAR f g (x ∷ xs) = _∷_ (f x) ⦃ g _ _ it ⦄ ⦃ g _ _ it ⦄ (mapAR f g xs)

  mapAR-compose :
    (f' : B → C) (g' : ∀ x y → S x y → T (f' x) (f' y))
    (f  : A → B) (g  : ∀ x y → R x y → S (f  x) (f  y))
    (xs : OList A R l u) → mapAR {S = T} f' g' (mapAR f g xs)
                         ≡ mapAR (f' ∘ f) (λ _ _ → g' _ _ ∘ g _ _) xs
  mapAR-compose f' g' f g []       = refl
  mapAR-compose f' g' f g (x ∷ xs) = cong (_∷_ (f' (f x)) ⦃ g' _ _ (g _ _ it) ⦄ ⦃ g' _ _ (g _ _ it) ⦄) (mapAR-compose f' g' f g xs)

  ReverseTypeFreeTheorem : ReverseType → Set₁
  ReverseTypeFreeTheorem rev =
    {A B : Set}                         (f : B → A)
    {R : A → A → Set} {S : B → B → Set} (g : ∀ x y → S x y → R (f x) (f y))
    {x' y' : B} (xs' : OList B S x' y') →
    rev {A} {R} (mapAR f g xs') ≡ mapAR f (flip g) (rev {B} {S} xs')

  data IndexA {A : Set} {R : A → A → Set} : {l u : A} → OList A R l u → Set where
    zeroN     : {u : A}                                                              → IndexA ([] {u = u})
    zeroC one : {x : A} ⦃ _ : R l x ⦄ ⦃ _ : R x m ⦄ {xs : OList A R m u}             → IndexA (x ∷ xs)
    twoPlus   : {x : A} ⦃ _ : R l x ⦄ ⦃ _ : R x m ⦄ {xs : OList A R m u} → IndexA xs → IndexA (x ∷ xs)

  data IndexR {A : Set} {R : A → A → Set} : {l u : A} {xs : OList A R l u} → IndexA xs → IndexA xs → Set where
    zeroC-one : {x : A} ⦃ _ : R l x ⦄ ⦃ _ : R x m ⦄ {xs : OList A R m u}                                → IndexR  zeroC      (one {x = x} {xs})
    one-twoN  : {x : A} ⦃ _ : R l x ⦄ ⦃ _ : R x u ⦄                                                     → IndexR  one        (twoPlus (zeroN {u = u}))
    one-twoC  : {x y : A} ⦃ _ : R l x ⦄ ⦃ _ : R x m ⦄ ⦃ _ : R m y ⦄ ⦃ _ : R y n ⦄ {xs : OList A R n u}  → IndexR  one        (twoPlus (zeroC {x = y} {xs}))
    twoPlus   : {x : A} ⦃ _ : R l x ⦄ ⦃ _ : R x m ⦄ {xs : OList A R m u} {i j : IndexA xs} → IndexR i j → IndexR (twoPlus i) (twoPlus j)

  indexA : {xs : OList A R l u} → IndexA xs → A
  indexA {u  = u}       zeroN      = u
  indexA {l  = l}       zeroC      = l
  indexA {xs = x ∷ xs}  one        = x
  indexA {xs = x ∷ xs} (twoPlus i) = indexA i

  indexR : {xs : OList A R l u}
           (i j : IndexA xs) → IndexR i j → R (indexA i) (indexA j)
  indexR _ _  zeroC-one  = it
  indexR _ _  one-twoN   = it
  indexR _ _  one-twoC   = it
  indexR _ _ (twoPlus r) = indexR _ _ r

  lowerIndex : (xs : OList A R l u) → IndexA xs
  lowerIndex []       = zeroN
  lowerIndex (x ∷ xs) = zeroC

  upperIndex : (xs : OList A R l u) → IndexA xs
  upperIndex []       = zeroN
  upperIndex (x ∷ xs) = twoPlus (upperIndex xs)

  cong-proj₂ : {A : Set} {B : A → Set} {x : A} {y z : B x}
             → (Σ A B ∋ (x , y)) ≡ (x , z) → y ≡ z
  cong-proj₂ refl = refl

  toIndices :
      (xs : OList A R l u)
    → Σ[ is ∈ OList (IndexA xs) IndexR (lowerIndex xs) (upperIndex xs) ]
      (((Σ[ l ∈ A ] Σ[ u ∈ A ] OList A R l u) ∋
        (indexA (lowerIndex xs) , indexA (upperIndex xs) , mapAR indexA indexR is))
      ≡ (l                      , u                      , xs))
  toIndices [] = [] , refl
  toIndices (x ∷ []) = _∷_ one ⦃ zeroC-one ⦄ ⦃ one-twoN ⦄ [] , refl
  toIndices (x ∷ xs@(_ ∷ _)) =
    let (is , eq) = toIndices xs
    in  _∷_ one ⦃ zeroC-one ⦄ ⦃ one-twoC ⦄ (mapAR twoPlus (λ _ _ → twoPlus) is) ,
        cong (_ ,_) (trans (cong (λ ys → _ , x ∷ ys) (mapAR-compose indexA indexR twoPlus (λ _ _ → twoPlus) is))
                           (cong (map₂ (λ ys → x ∷ ys)) (cong-proj₂ eq)))

  minusTwo : {x : A} ⦃ _ : flip R u x ⦄ ⦃ _ : flip R x m ⦄
             {xs : OList A (flip R) m l} {ui li : IndexA xs}
           → OList (IndexA (x ∷ xs)) (flip IndexR) (twoPlus ui) (twoPlus li)
           → OList (IndexA xs) (flip IndexR) ui li
  minusTwo [] = []
  minusTwo (_∷_ one ⦃ one-twoN ⦄ ⦃ zeroC-one ⦄ (_∷_ _ ⦃ () ⦄ ⦃ _ ⦄ _))
  minusTwo (_∷_ one ⦃ one-twoC ⦄ ⦃ zeroC-one ⦄ (_∷_ _ ⦃ () ⦄ ⦃ _ ⦄ _))
  minusTwo (_∷_ (twoPlus i) ⦃ twoPlus r ⦄ ⦃ twoPlus r' ⦄ is) = _∷_ i ⦃ r ⦄ ⦃ r' ⦄ (minusTwo is)
  minusTwo (_∷_ (twoPlus zeroC) ⦃ twoPlus r ⦄ ⦃ one-twoC ⦄ (_∷_ _ ⦃ zeroC-one ⦄ ⦃ () ⦄ _))

  emptyIndices : {xs : OList A (flip R) u l} (j : IndexA xs)
                 (is : OList (IndexA xs) (flip IndexR) j j) → is ≡ []
  emptyIndices zeroN [] = refl
  emptyIndices zeroC [] = refl
  emptyIndices one [] = refl
  emptyIndices one (_∷_ zeroC ⦃ zeroC-one ⦄ ⦃ () ⦄ _)
  emptyIndices (twoPlus j) [] = refl
  emptyIndices (twoPlus j) (_∷_ (twoPlus i) ⦃ twoPlus r ⦄ ⦃ twoPlus r' ⦄ is) with () ← emptyIndices j (_∷_ i ⦃ r ⦄ ⦃ r' ⦄ (minusTwo is))
  emptyIndices (twoPlus j) (_∷_ (twoPlus zeroC) ⦃ twoPlus r ⦄ ⦃ one-twoC ⦄ (_∷_ _ ⦃ zeroC-one ⦄ ⦃ () ⦄ _))
  emptyIndices (twoPlus zeroN) (_∷_ one ⦃ one-twoN ⦄ ⦃ zeroC-one ⦄ (_∷_ _ ⦃ () ⦄ _))
  emptyIndices (twoPlus zeroC) (_∷_ one ⦃ one-twoC ⦄ ⦃ zeroC-one ⦄ (_∷_ _ ⦃ () ⦄ _))

  IndexR-functional : {xs : OList A (flip R) u l} {i j k : IndexA xs}
                    → (ri : IndexR i k) (rj : IndexR j k)
                    → ((Σ[ i ∈ IndexA xs ] IndexR i k) ∋ (i , ri)) ≡ (j , rj)
  IndexR-functional  zeroC-one    zeroC-one   = refl
  IndexR-functional  one-twoN     one-twoN    = refl
  IndexR-functional  one-twoC     one-twoC    = refl
  IndexR-functional (twoPlus ri) (twoPlus rj) = cong (map twoPlus twoPlus) (IndexR-functional ri rj)

  uniqueIndices : {xs : OList A (flip R) u l} {li ui : IndexA xs}
                  (is js : OList (IndexA xs) (flip IndexR) li ui) → is ≡ js
  uniqueIndices [] js = sym (emptyIndices _ js)
  uniqueIndices is@(_ ∷ _) [] = emptyIndices _ is
  uniqueIndices (_∷_ i ⦃ r₀ ⦄ is) (_∷_ j ⦃ r₁ ⦄ js) with IndexR-functional r₀ r₁
  uniqueIndices (_∷_ i ⦃ _ ⦄ ⦃ r₂ ⦄ is) (_∷_ i ⦃ _ ⦄ ⦃ r₃ ⦄ js) | refl with IndexR-functional r₂ r₃
  uniqueIndices (i ∷ is) (i ∷ js) | refl | refl = cong (i ∷_) (uniqueIndices is js)

  uniqueness : (rev rev' : ReverseType)
             → ReverseTypeFreeTheorem rev → ReverseTypeFreeTheorem rev'
             → {A : Set} {R : A → A → Set} {l u : A} (xs : OList A R l u)
             → rev xs ≡ rev' xs
  uniqueness rev rev' freeThm freeThm' {A} {R} {l} {u} xs with (is , iseq) ← toIndices xs =
    cong-proj₂ $ cong-proj₂ $
    begin ((Σ[ u ∈ A ] Σ[ l ∈ A ] OList A (flip R) u l) ∋
      (u , l , rev xs))
        ≡⟨ cong (λ (l , u , xs) → (u , l , rev xs)) (sym iseq) ⟩
      (indexA (upperIndex xs) , indexA (lowerIndex xs) , rev (mapAR indexA indexR is))
        ≡⟨ cong (λ ys → _ , _ , ys) $
           begin
             rev (mapAR indexA indexR is)
               ≡⟨ freeThm indexA indexR is ⟩
             mapAR indexA (flip indexR) (rev is)
               ≡⟨ cong (mapAR indexA (flip indexR)) (uniqueIndices (rev is) (rev' is)) ⟩
             mapAR indexA (flip indexR) (rev' is)
               ≡⟨ sym (freeThm' indexA indexR is) ⟩
             rev' (mapAR indexA indexR is)
             ∎ ⟩
      (indexA (upperIndex xs) , indexA (lowerIndex xs) , rev' (mapAR indexA indexR is))
        ≡⟨ cong (λ (l , u , xs) → (u , l , rev' xs)) iseq ⟩
      (u , l , rev' xs)
    ∎
    where open ≡-Reasoning

open TwoListQueue using (OListF; nil; cons)

-- Actual goal: unique inhabitance of the following abstract queue type
--              (quantified over A and R) up to bisimilarity
record Queue (A : Set) (R : A → A → Set) : Set₁ where
  field
    State : A → A → Set
    empty : State u u
    push  : (x : A) → ⦃ R u x ⦄ → ⦃ R x u' ⦄ → State l u → State l u'
    pop   : State l u → OListF A R State l u

-- Reference implementation, on which we prove extensional properties we want…
oneListQueue : ∀ {A R} → Queue A R
oneListQueue {A} {R} = record
  { State = OList A R
  ; empty = []
  ; push  = λ x xs → xs ++ (x ∷ [])
  ; pop   = λ { []       → nil
              ; (x ∷ xs) → cons x xs } }

-- …and any other implementation will satisfy the same properties.
twoListQueue : ∀ {A R} → Queue A R
twoListQueue {A} {R} = record
  { State = State
  ; empty = empty
  ; push  = push
  ; pop   = pop }
  where open TwoListQueue A R

  -- Still devising a proof

-- Stack order? Priority-queue order?
