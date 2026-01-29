-- Checked with Agda 2.8.0 and Standard Library 2.3

{-# OPTIONS --safe --large-indices --no-forced-argument-recursion #-}

module QueueBisimilarity where

open import Function
open import Data.Empty
open import Data.Unit
open import Data.Product
open import Data.Nat
open import Data.Nat.Properties
open import Relation.Binary.PropositionalEquality

module OldDefinitions where

  data ListF (T : Set) (S : T → T → Set) (A : T → Set) (X : T → T → Set) : T → T → Set where
    nil  : ∀ {u}                                     → ListF T S A X u u
    cons : ∀ {l m n u} → S l m → S m n → A m → X n u → ListF T S A X l u

  record Queue (T : Set) (S : T → T → Set) (A : T → Set) : Set₁ where
    field
      State : T → T → Set
      empty : ∀ {u} → State u u
      push  : ∀ {l u m v} → S u m → S m v → A m → State l u → State l v
      pop   : ∀ {l u} → State l u → ListF T S A State l u

module RevisedDefinitions where

  data ListF (I : Set) (R : I → I → Set) (X : I → I → Set) : I → I → Set where
    nil  : ∀ {u}                     → ListF I R X u u
    cons : ∀ {l m u} → R l m → X m u → ListF I R X l u

  record Queue (I : Set) (i₀ : I) (R : I → I → Set) : Set₁ where
    field
      State : I → I → Set
      empty : State i₀ i₀
      push  : ∀ {l u v} → R u v → State l u → State l v
      pop   : ∀ {l u} → State l u → ListF I R State l u

  open Queue public

  SQueue : Set → Set₁
  SQueue A = Queue ⊤ tt (λ _ _ → A)

  PQueue : Set₁
  PQueue = ∀ I i₀ R → Queue I i₀ R

  inst : PQueue → (A : Set) → SQueue A
  inst Q A = Q ⊤ tt (λ _ _ → A)

  variable
    A : Set
    a a' s s' x x' fx fx' : A

open   RevisedDefinitions

module ParametricityTranslation where

  data ListFᴾ {I : Set}         (Iᴾ : I → Set)
              {R : I → I → Set} (Rᴾ : ∀ {i j} → Iᴾ i → Iᴾ j → R i j → Set)
              {X : I → I → Set} (Xᴾ : ∀ {l u} → Iᴾ l → Iᴾ u → X l u → Set)
            : ∀ {l u} → Iᴾ l → Iᴾ u → ListF I R X l u → Set where
    nilᴾ  : ∀ {u fx} → {uᴾ : Iᴾ u}
          → fx ≡ nil → ListFᴾ Iᴾ Rᴾ Xᴾ uᴾ uᴾ fx
    consᴾ : ∀ {l m u r x fx} → {lᴾ : Iᴾ l} {mᴾ : Iᴾ m} {uᴾ : Iᴾ u}
          → Rᴾ lᴾ mᴾ r → Xᴾ mᴾ uᴾ x → fx ≡ cons r x → ListFᴾ Iᴾ Rᴾ Xᴾ lᴾ uᴾ fx

  record Queueᴾ {I  : Set}         (Iᴾ  : I → Set)
                {i₀ : I}           (i₀ᴾ : Iᴾ i₀)
                {R  : I → I → Set} (Rᴾ  : ∀ {i j} → Iᴾ i → Iᴾ j → R i j → Set)
                (q : Queue I i₀ R) : Set₁ where
    field
      Stateᴾ : ∀ {l u} → Iᴾ l → Iᴾ u → q .State l u → Set
      emptyᴾ : Stateᴾ i₀ᴾ i₀ᴾ (q .empty)
      pushᴾ  : ∀ {l u v r s} → {lᴾ : Iᴾ l} {uᴾ : Iᴾ u} {vᴾ : Iᴾ v}
            → Rᴾ uᴾ vᴾ r → Stateᴾ lᴾ uᴾ s → Stateᴾ lᴾ vᴾ (q .push r s)
      popᴾ   : ∀ {l u s} → {lᴾ : Iᴾ l} {uᴾ : Iᴾ u}
            → Stateᴾ lᴾ uᴾ s → ListFᴾ Iᴾ Rᴾ Stateᴾ lᴾ uᴾ (q .pop s)

  open Queueᴾ public

  Param : PQueue → Set₁
  Param Q = {I  : Set}         (Iᴾ  : I → Set)
            {i₀ : I}           (i₀ᴾ : Iᴾ i₀)
            {R  : I → I → Set} (Rᴾ  : ∀ {i j} → Iᴾ i → Iᴾ j → R i j → Set)
                             → Queueᴾ Iᴾ i₀ᴾ Rᴾ (Q I i₀ R)

open   ParametricityTranslation

module BisimilarityProof where

  module _ where

    private variable
      #push #pop : ℕ

    data OpSeq (A : Set) : ℕ → ℕ → Set where
      emptyOp :                          OpSeq A  zero      zero
      pushOp  : A → OpSeq A #pop #push → OpSeq A      #pop (suc #push)
      popOp   :     OpSeq A #pop #push → OpSeq A (suc #pop)     #push

    private variable
      q : SQueue A
      ops ops' : OpSeq A #pop #push
      k : ℕ

    data RunOpSeq : OpSeq A #pop #push → (q : SQueue A) → q .State tt tt → Set where
      emptyOp : q .empty ≡ s
              → RunOpSeq emptyOp q s
      pushOp  : RunOpSeq ops q s' → q .push a s' ≡ s
              → RunOpSeq (pushOp a ops) q s
      popOp   : RunOpSeq ops q s' → q .pop s' ≡ cons a s
              → RunOpSeq (popOp ops) q s

    data PushMap : OpSeq A #pop #push → ℕ → A → Set where
      pushOp-here  : {ops : OpSeq A #pop #push} → PushMap (pushOp a ops) #push a
      pushOp-there : PushMap ops k a'           → PushMap (pushOp a ops) k     a'
      popOp        : PushMap ops k a'           → PushMap (popOp    ops) k     a'

    PushMap-domain-bounded :
      {ops : OpSeq A #pop #push} → PushMap ops k a → k < #push
    PushMap-domain-bounded  pushOp-here     = ≤-refl
    PushMap-domain-bounded (pushOp-there m) = m≤n⇒m≤1+n (PushMap-domain-bounded m)
    PushMap-domain-bounded (popOp        m) = PushMap-domain-bounded m

    PushMap-functional : PushMap ops k a → PushMap ops k a' → a ≡ a'
    PushMap-functional  pushOp-here      pushOp-here      = refl
    PushMap-functional  pushOp-here     (pushOp-there m') = ⊥-elim (1+n≰n (PushMap-domain-bounded m'))
    PushMap-functional (pushOp-there m)  pushOp-here      = ⊥-elim (1+n≰n (PushMap-domain-bounded m ))
    PushMap-functional (pushOp-there m) (pushOp-there m') = PushMap-functional m m'
    PushMap-functional (popOp m)        (popOp        m') = PushMap-functional m m'

    -- Count and Check
    CCQueue : SQueue A → OpSeq A #pop #push → Set₁
    CCQueue q ops = Queueᴾ (λ _ → ℕ)
                           zero
                           (λ l u a → suc l ≡ u × PushMap ops l a)
                           q

    simulate' : {ops : OpSeq A #pop #push}
              → RunOpSeq ops q s
              → (qᴾ : CCQueue q ops')
              → (∀ {k a} → PushMap ops k a → PushMap ops' k a)
              → qᴾ .Stateᴾ #pop #push s
    simulate' (emptyOp    refl) qᴾ inj = qᴾ .emptyᴾ
    simulate' (pushOp run refl) qᴾ inj = qᴾ .pushᴾ (refl , inj pushOp-here) (simulate' run qᴾ (inj ∘ pushOp-there))
    simulate' (popOp  run eq  ) qᴾ inj with qᴾ .popᴾ (simulate' run qᴾ (inj ∘ popOp))
    simulate' (popOp  run eq  ) qᴾ inj | nilᴾ eq' with () ← trans (sym eq) eq'
    simulate' (popOp  run eq  ) qᴾ inj | consᴾ (refl , _) s' eq' with refl ← trans (sym eq) eq' = s'

    simulate : {ops : OpSeq A #pop #push} → RunOpSeq ops q s
             → (qᴾ : CCQueue q ops) → qᴾ .Stateᴾ #pop #push s
    simulate run qᴾ = simulate' run qᴾ id

  record BisimState (q q' : SQueue A)
                    (s : q .State tt tt) (s' : q' .State tt tt) : Set where
    field
      {#pop #push} : ℕ
      ops  : OpSeq A #pop #push
      run  : RunOpSeq ops q  s
      run' : RunOpSeq ops q' s'

  open BisimState public

open   BisimilarityProof

module BisimilarityTheorem where

  data ListFᴿ {A : Set} {X X' : Set} (Xᴿ : X → X' → Set)
          : ListF ⊤ (λ _ _ → A) (λ _ _ → X ) tt tt
          → ListF ⊤ (λ _ _ → A) (λ _ _ → X') tt tt → Set where
    nilᴿ  : fx ≡ nil      → fx' ≡ nil                           → ListFᴿ Xᴿ fx fx'
    consᴿ : fx ≡ cons a x → fx' ≡ cons a' x' → a ≡ a' → Xᴿ x x' → ListFᴿ Xᴿ fx fx'

  record QueueBisim (q q' : SQueue A) : Set₁ where
    field
      Stateᴿ : q .State tt tt → q' .State tt tt → Set
      emptyᴿ : Stateᴿ (q .empty) (q' .empty)
      pushᴿ  : (a : A) → Stateᴿ s s' → Stateᴿ (q .push a s) (q' .push a s')
      popᴿ   : Stateᴿ s s' → ListFᴿ Stateᴿ (q .pop s) (q' .pop s')

  bisim : (Q Q' : PQueue) → Param Q → Param Q'
        → (A : Set) → QueueBisim (inst Q A) (inst Q' A)
  bisim Q Q' param param' A = record
    { Stateᴿ = BisimState q q'
    ; emptyᴿ = record { ops  = emptyOp
                      ; run  = emptyOp refl
                      ; run' = emptyOp refl
                      }
    ; pushᴿ  = λ a sᴿ → record
                      { ops  = pushOp a (sᴿ .ops )
                      ; run  = pushOp   (sᴿ .run ) refl
                      ; run' = pushOp   (sᴿ .run') refl
                      }
    ; popᴿ   = popᴿ-proof
    }
    where

    q q' : SQueue A
    q  = inst Q  A
    q' = inst Q' A

    popᴿ-proof : BisimState q q' s s'
               → ListFᴿ (BisimState q q') (q .pop s) (q' .pop s')
    popᴿ-proof sᴿ with qᴾ  ← CCQueue q  (sᴿ .ops) ∋ param  _ _ _
                     | q'ᴾ ← CCQueue q' (sᴿ .ops) ∋ param' _ _ _
        | qᴾ  .popᴾ (simulate (sᴿ .run ) qᴾ )
        | q'ᴾ .popᴾ (simulate (sᴿ .run') q'ᴾ)
    ... | nilᴾ eq            | nilᴾ eq'             = nilᴿ eq eq'
    ... | nilᴾ _             | consᴾ (_ , m') _ _   with () ← <-irrefl refl (PushMap-domain-bounded m')
    ... | consᴾ (_ , m) _ _  | nilᴾ _               with () ← <-irrefl refl (PushMap-domain-bounded m )
    ... | consᴾ (_ , m) _ eq | consᴾ (_ , m') _ eq' =
      consᴿ eq eq' (PushMap-functional m m') record
        { ops  = popOp (sᴿ .ops )
        ; run  = popOp (sᴿ .run ) eq
        ; run' = popOp (sᴿ .run') eq'
        }

open   BisimilarityTheorem
