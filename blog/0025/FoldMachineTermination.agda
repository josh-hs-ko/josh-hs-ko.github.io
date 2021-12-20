{-# OPTIONS --safe --guardedness #-}

-- Agda version: 2.6.2
-- Standard library version: 1.7

open import Function
open import Data.Product
open import Data.Unit
open import Data.Maybe
open import Data.Maybe.Relation.Binary.Pointwise as Maybe using (Pointwise; just; nothing)
open import Data.Nat
open import Data.List renaming (List to Stack; map to Stack-map) using ([]; _∷_)
open import Data.List.Relation.Binary.Pointwise as Stack using (Pointwise; []; _∷_)
open import Relation.Binary.PropositionalEquality

variable A B C X Y : Set


--------
-- The fold machine

data ITreeF (A X : Set) : Set where
  leaf : ITreeF A X
  node : A → X → X → ITreeF A X

data μITree (A : Set) : Set where
  con : ITreeF A (μITree A) → μITree A

record νITree (A : Set) : Set where
  coinductive
  field decon : ITreeF A (νITree A)

open νITree

toνITree : μITree A → νITree A
decon (toνITree (con  leaf       )) = leaf
decon (toνITree (con (node a t u))) = node a (toνITree t) (toνITree u)

data Focus (A B : Set) : Set where
  ↓_ : νITree A → Focus A B
  ↑_ : B        → Focus A B

data ∂ITree (A B : Set) : Set where
  ○-⟨_,_⟩ : νITree A → (B → B → B) → ∂ITree A B
  ⟨_⟩-○   :                (B → B) → ∂ITree A B

record MachineState (A B : Set) : Set where
  constructor ⟨_,_⟩
  field
    focus : Focus A B
    stack : Stack (∂ITree A B)

step : B → (A → B → B → B)
     → MachineState A B → Maybe (MachineState A B)
step b f ⟨ ↓ t , stk ⟩ with decon t
... | leaf       = just ⟨ ↑ b ,                 stk ⟩
... | node a t u = just ⟨ ↓ t , ○-⟨ u , f a ⟩ ∷ stk ⟩
step _ _ ⟨ ↑ b , [] ⟩ = nothing
step _ _ ⟨ ↑ b , ○-⟨ u , f₁ ⟩ ∷ stk ⟩ = just ⟨ ↓ u    , ⟨ f₁ b ⟩-○ ∷ stk ⟩
step _ _ ⟨ ↑ b , ⟨ f₂ ⟩-○     ∷ stk ⟩ = just ⟨ ↑ f₂ b ,              stk ⟩


--------
-- Lists and colists

data ListF (A X : Set) : Set where
  []  : ListF A X
  _∷_ : A → X → ListF A X

data μList (A : Set) : Set where
  con : ListF A (μList A) → μList A

record νList (A : Set) : Set where
  coinductive
  field decon : ListF A (νList A)

open νList

toνList : μList A → νList A
decon (toνList (con []      )) = []
decon (toνList (con (x ∷ xs))) = x ∷ toνList xs

unfoldr : (B → ListF A B) → B → νList A
decon (unfoldr f b) with f b
... | []     = []
... | a ∷ b' = a ∷ unfoldr f b'

iterateMaybe : (A → Maybe A) → Maybe A → νList A
iterateMaybe f = unfoldr (maybe (λ a → a ∷ f a) [])

run : B → (A → B → B → B) → νITree A → νList (MachineState A B)
run b f t = iterateMaybe (step b f) (just ⟨ ↓ t , [] ⟩)


--------
-- Simulation

_~_ : Set → Set → Set₁
A ~ B = A → B → Set

data ListSimF (R : A ~ B) (S : X ~ Y) : ListF A X ~ ListF B Y where
  []  : {bs : ListF B Y} → ListSimF R S [] bs
  _∷_ : {a : A} {b : B} → R a b →
        {x : X} {y : Y} → S x y → ListSimF R S (a ∷ x) (b ∷ y)

record νListSim (R : A ~ B) (as : νList A) (bs : νList B) : Set where
  coinductive
  field decon : ListSimF R (νListSim R) (decon as) (decon bs)

open νListSim

toμList : {R : A ~ B} (as : νList A) (bs : μList B)
        → νListSim R as (toνList bs) → μList A
toμList as _           sim with decon as | decon sim
toμList _  _             _ | []     | _       = con []
toμList _ (con (_ ∷ bs)) _ | a ∷ as | _ ∷ sim = con (a ∷ toμList as bs sim)


--------
-- Abstract fold machine

record MachineState' (A : Set) : Set where
  constructor ⟨_,_⟩'
  field
    focus : Maybe (μITree A)
    stack : Stack (Maybe (μITree A))

step' : MachineState' A → Maybe (MachineState' A)
step' ⟨ just (con  leaf       ) , stk ⟩' = just ⟨ nothing ,          stk ⟩'
step' ⟨ just (con (node a t u)) , stk ⟩' = just ⟨ just t ,  just u ∷ stk ⟩'
step' ⟨ nothing ,                 []  ⟩' = nothing
step' ⟨ nothing ,       just u  ∷ stk ⟩' = just ⟨ just u , nothing ∷ stk ⟩'
step' ⟨ nothing ,       nothing ∷ stk ⟩' = just ⟨ nothing ,          stk ⟩'


--------
-- First simulation (termination of the abstract machine)

countTreeSteps : μITree A → ℕ → ℕ
countTreeSteps (con  leaf       ) = suc
countTreeSteps (con (node a t u)) = suc ∘ countTreeSteps t ∘ suc ∘ countTreeSteps u ∘ suc

countStackSteps : Stack (Maybe (μITree A)) → ℕ
countStackSteps            []   = zero
countStackSteps (just t  ∷ stk) = suc (countTreeSteps t (suc (countStackSteps stk)))
countStackSteps (nothing ∷ stk) =                        suc (countStackSteps stk)

countSteps : MachineState' A → ℕ
countSteps ⟨ just t  , stk ⟩' = countTreeSteps t (countStackSteps stk)
countSteps ⟨ nothing , stk ⟩' =                   countStackSteps stk

replicate : ℕ → A → μList A
replicate zero    x = con []
replicate (suc n) x = con (x ∷ replicate n x)

terminationSim : (ms' : MachineState' A)
               → νListSim (λ _ _ → ⊤) (iterateMaybe step' (just ms'))
                                      (toνList (replicate (suc (countSteps ms')) tt))
decon (terminationSim ⟨ just (con leaf        ) , stk ⟩') = tt ∷ terminationSim _
decon (terminationSim ⟨ just (con (node a t u)) , stk ⟩') = tt ∷ terminationSim _
decon (terminationSim ⟨ nothing ,                 []  ⟩') = tt ∷ record { decon = [] }
decon (terminationSim ⟨ nothing ,        just u ∷ stk ⟩') = tt ∷ terminationSim _
decon (terminationSim ⟨ nothing ,       nothing ∷ stk ⟩') = tt ∷ terminationSim _


--------
-- Second simulation (abstraction of the original machine)

funR : (A → B) → (A ~ B)
funR f x y = f x ≡ y

from∂ITree : ∂ITree A B → Maybe (νITree A)
from∂ITree ○-⟨ t , _ ⟩ = just t
from∂ITree ⟨ _ ⟩-○     = nothing

fromFocus : Focus A B → Maybe (νITree A)
fromFocus (↓ t) = just t
fromFocus (↑ _) = nothing

abstractionR : MachineState A B → MachineState' A → Set
abstractionR ⟨ focus , stack ⟩ ⟨ focus' , stack' ⟩' =
  Maybe.Pointwise (funR toνITree) focus' (fromFocus focus) ×
  Stack.Pointwise (Maybe.Pointwise (funR toνITree)) stack' (Stack-map from∂ITree stack)

abstractionSim : (b : B) (f : A → B → B → B)
                 (ms : MachineState A B) (ms' : MachineState' A) → abstractionR ms ms'
               → νListSim abstractionR (iterateMaybe (step b f) (just ms ))
                                       (iterateMaybe  step'     (just ms'))
decon (abstractionSim b f ⟨ ↓ ._ ,              stk ⟩ ⟨ just (con  leaf       ) , stk' ⟩' (just refl ,           p)) = (just refl ,           p) ∷ abstractionSim b f _ _ (nothing ,               p)
decon (abstractionSim b f ⟨ ↓ ._ ,              stk ⟩ ⟨ just (con (node a t u)) , stk' ⟩' (just refl ,           p)) = (just refl ,           p) ∷ abstractionSim b f _ _ (just refl , just refl ∷ p)
decon (abstractionSim b f ⟨ ↑ _ ,               []  ⟩ ⟨ nothing ,                 []   ⟩' (nothing ,            [])) = (nothing ,            []) ∷ record { decon = [] }
decon (abstractionSim b f ⟨ ↑ _ , ○-⟨ _ , _ ⟩ ∷ stk ⟩ ⟨ nothing ,     .(just _) ∷ stk' ⟩' (nothing , just refl ∷ p)) = (nothing , just refl ∷ p) ∷ abstractionSim b f _ _ (just refl ,   nothing ∷ p)
decon (abstractionSim b f ⟨ ↑ _ , ⟨ _ ⟩-○     ∷ stk ⟩ ⟨ nothing ,       nothing ∷ stk' ⟩' (nothing ,   nothing ∷ p)) = (nothing ,   nothing ∷ p) ∷ abstractionSim b f _ _ (nothing ,               p)


--------
-- Termination of the original machine

_◯_ : (A ~ B) → (B ~ C) → (A ~ C)
(R ◯ S) a c = Σ[ b ∈ _ ] (R a b × S b c)

νListSim-trans : {R : A ~ B} {S : B ~ C} {xs : νList A} {ys : νList B} {zs : νList C}
               → νListSim R xs ys → νListSim S ys zs → νListSim (R ◯ S) xs zs
decon (νListSim-trans {xs = xs} {ys} {zs} simR simS)
  with decon xs | decon ys | decon zs | decon simR | decon simS
... | []    | _     | _     | _        | _        = []
... | _ ∷ _ | _ ∷ _ | _ ∷ _ | r ∷ simR | s ∷ simS = (_ , r , s) ∷ νListSim-trans simR simS

νListSim-map : {R S : A ~ B} → (∀ {a b} → R a b → S a b)
             → ∀ {as bs} → νListSim R as bs → νListSim S as bs
decon (νListSim-map R⇒S {as = as} {bs} sim) with decon as | decon bs | decon sim
... | []    | _     | _        = []
... | _ ∷ _ | _ ∷ _ | r ∷ sim' = R⇒S r ∷ νListSim-map R⇒S sim'

fullTerminationSim : (b : B) (f : A → B → B → B) (t : μITree A)
                   → νListSim (λ _ _ → ⊤)
                              (iterateMaybe (step b f) (just ⟨ ↓ (toνITree t) , [] ⟩))
                              (toνList (replicate (suc (countTreeSteps t zero)) tt))
fullTerminationSim b f t =
  νListSim-map (λ _ → tt)
    (νListSim-trans (abstractionSim b f (⟨ ↓ (toνITree t) , [] ⟩ )
                                        (⟨ just        t  , [] ⟩')
                                        (  just      refl , []   ))
                    (terminationSim     (⟨ just        t  , [] ⟩')))
