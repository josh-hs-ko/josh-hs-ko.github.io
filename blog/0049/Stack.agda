-- Checked with Agda 2.8.0 and Standard Library 2.3

{-# OPTIONS --safe --large-indices --no-forced-argument-recursion #-}

module Stack where

open import Function
open import Data.Product
open import Data.Unit
open import Data.Nat
open import Data.Nat.Properties
open import Data.List
open import Data.List.Relation.Binary.Pointwise renaming (refl to Pointwise-refl)
open import Data.List.Relation.Binary.Suffix.Heterogeneous
open import Data.List.Relation.Binary.Suffix.Heterogeneous.Properties renaming (trans to Suffix-trans)
open import Relation.Binary.PropositionalEquality

module _ (I : Set) (i₀ : I) (R : I → I → Set) where

  data ListF (X : I → Set) : I → Set where
    nil  :                         ListF X i₀
    cons : ∀ {i j} → R i j → X j → ListF X i

  record Stack : Set₁ where
    field
      State : I → Set
      empty : State i₀
      push  : ∀ {i j} → R i j → State j → State i
      pop   : ∀ {i} → State i → ListF State i

open Stack

PStack : Set₁
PStack = ∀ I i₀ R → Stack I i₀ R

SStack : Set → Set₁
SStack A = Stack ⊤ tt (λ _ _ → A)

inst : PStack → (A : Set) → SStack A
inst St A = St ⊤ tt (λ _ _ → A)

Ford : {I : Set} → (I → Set) → I → Set
Ford P i₀ = ∀ {i} → i ≡ i₀ → P i

Ford² : {I J : Set} → (I → J → Set) → I → J → Set
Ford² R i₀ j₀ = Ford (λ i → Ford (R i) j₀) i₀

module _ {I  : Set}         (Iᴾ  : I → Set)
         {i₀ : I}           (i₀ᴾ : Iᴾ i₀)
         {R  : I → I → Set} (Rᴾ  : ∀ {i j} → Iᴾ i → Iᴾ j → R i j → Set) where

  data ListFᴾ {X : I → Set} (Xᴾ : ∀ {i} → Iᴾ i → X i → Set)
          : ∀ {i} (iᴾ : Iᴾ i) → ListF I i₀ R X i → Set where
    nilᴾ  : ListFᴾ Xᴾ i₀ᴾ nil
    consᴾ : ∀ {i j} {r : R i j} {x : X j}
          → {iᴾ : Iᴾ i} {jᴾ : Iᴾ j} → Rᴾ iᴾ jᴾ r → Xᴾ jᴾ x
          → ListFᴾ Xᴾ iᴾ (cons r x)

  record Stackᴾ (st : Stack I i₀ R) : Set₁ where
    field
      Stateᴾ : ∀ {i} → Iᴾ i → st .State i → Set
      emptyᴾ : Stateᴾ i₀ᴾ (st .empty)
      pushᴾ  : ∀ {i j} {r : R i j} {s : st .State j}
             → {iᴾ : Iᴾ i} {jᴾ : Iᴾ j} → Rᴾ iᴾ jᴾ r → Stateᴾ jᴾ s
             → Ford (Stateᴾ iᴾ) (st .push r s)
      popᴾ   : ∀ {i} {s : st .State i}
             → {iᴾ : Iᴾ i} → Stateᴾ iᴾ s
             → Ford (ListFᴾ Stateᴾ iᴾ) (st .pop s)

open Stackᴾ

Param : PStack → Set₁
Param St = {I  : Set}         (Iᴾ  : I → Set)
           {i₀ : I}           (i₀ᴾ : Iᴾ i₀)
           {R  : I → I → Set} (Rᴾ  : ∀ {i j} → Iᴾ i → Iᴾ j → R i j → Set)
                             → Stackᴾ Iᴾ i₀ᴾ Rᴾ (St I i₀ R)

module _ where

  private variable
    k x y : ℕ
    stk res ks xs ys zs : List ℕ

  data OpSeq (A : Set) : List ℕ → List ℕ → Set where
    emptyOp :                                  OpSeq A []            []
    pushOp₀ : A → OpSeq A      stk []        → OpSeq A (zero  ∷ stk) []
    pushOp₁ : A → OpSeq A      stk (k ∷ res) → OpSeq A (suc k ∷ stk)     res
    popOp   :     OpSeq A (k ∷ stk)     res  → OpSeq A          stk (k ∷ res)

  private variable
    A : Set
    a a' s t : A
    st : SStack A
    ops ops' : OpSeq A stk res

  data RunOpSeq : OpSeq A stk res → (st : SStack A) → st .State tt → Set where
    emptyOp : st .empty ≡ s
            → RunOpSeq emptyOp st s
    pushOp₀ : RunOpSeq ops st s → st .push a s ≡ t
            → RunOpSeq (pushOp₀ a ops) st t
    pushOp₁ : RunOpSeq ops st s → st .push a s ≡ t
            → RunOpSeq (pushOp₁ a ops) st t
    popOp   : RunOpSeq ops st s → st .pop s ≡ cons a t
            → RunOpSeq (popOp ops) st t

  data PushMap : OpSeq A stk res → List ℕ → A → Set where
    pushOp₀-here  : {ops : OpSeq A stk []}
                  → PushMap (pushOp₀ a ops) (zero  ∷ stk) a
    pushOp₁-here  : {ops : OpSeq A stk (k ∷ res)}
                  → PushMap (pushOp₁ a ops) (suc k ∷ stk) a
    pushOp₀-there : PushMap ops ks a' → PushMap (pushOp₀ a ops) ks a'
    pushOp₁-there : PushMap ops ks a' → PushMap (pushOp₁ a ops) ks a'
    popOp         : PushMap ops ks a' → PushMap (popOp     ops) ks a'

  _≼_ = Suffix _≤_

  ≼-refl : xs ≼ xs
  ≼-refl = fromPointwise (Pointwise-refl ≤-refl)

  ≼-trans : xs ≼ ys → ys ≼ zs → xs ≼ zs
  ≼-trans ls rs = Suffix-trans ≤-trans ls rs

  revcat : List A → List A → List A
  revcat []       ys = ys
  revcat (x ∷ xs) ys = revcat xs (x ∷ ys)

  ≼-revcat : ∀ xs → ys ≼ zs → ys ≼ revcat xs zs
  ≼-revcat []       ls = ls
  ≼-revcat (x ∷ xs) ls = ≼-revcat xs (there ls)

  Pointwise-revcat :
    ∀ xs → Pointwise _≤_ ys zs → Pointwise _≤_ (revcat xs ys) (revcat xs zs)
  Pointwise-revcat []       ls = ls
  Pointwise-revcat (x ∷ xs) ls = Pointwise-revcat xs (≤-refl ∷ ls)

  PushMap-domain-bounded :
    {ops : OpSeq A stk res} → PushMap ops ks a → ks ≼ revcat res stk
  PushMap-domain-bounded              pushOp₀-here     = ≼-refl
  PushMap-domain-bounded {res = res}  pushOp₁-here     = ≼-revcat res ≼-refl
  PushMap-domain-bounded             (pushOp₀-there m) = there (PushMap-domain-bounded m)
  PushMap-domain-bounded {res = res} (pushOp₁-there m) = ≼-trans (PushMap-domain-bounded m) (here (Pointwise-revcat res (m≤n⇒m≤1+n ≤-refl ∷ Pointwise-refl ≤-refl)))
  PushMap-domain-bounded             (popOp         m) = PushMap-domain-bounded m

  ≼-revcat⁻¹ : ∀ xs → length ys ≤ length zs → ys ≼ revcat xs zs → ys ≼ zs
  ≼-revcat⁻¹           []       l ls = ls
  ≼-revcat⁻¹           (x ∷ xs) l ls with ≼-revcat⁻¹ xs (m≤n⇒m≤1+n l) ls
  ≼-revcat⁻¹ {ys = ys} (x ∷ xs) l ls | here  rs with length ys | Pointwise-length rs
  ≼-revcat⁻¹           (x ∷ xs) l ls | here  rs | ._ | refl with () ← 1+n≰n l
  ≼-revcat⁻¹           (x ∷ xs) l ls | there rs = rs

  PushMap-functional : {ops : OpSeq A stk res} → PushMap ops ks a → PushMap ops ks a' → a ≡ a'
  PushMap-functional              pushOp₀-here      pushOp₀-here      = refl
  PushMap-functional              pushOp₀-here     (pushOp₀-there m') with () ← S[as][bs]⇒∣as∣≢1+∣bs∣ (PushMap-domain-bounded m') refl
  PushMap-functional              pushOp₁-here      pushOp₁-here      = refl
  PushMap-functional {res = res}  pushOp₁-here     (pushOp₁-there m') with ≼-revcat⁻¹ res ≤-refl (PushMap-domain-bounded m')
  PushMap-functional              pushOp₁-here     (pushOp₁-there m') | here (l ∷ ls) with () ← 1+n≰n l
  PushMap-functional              pushOp₁-here     (pushOp₁-there m') | there     ls  with () ← S[as][bs]⇒∣as∣≢1+∣bs∣ ls refl
  PushMap-functional             (pushOp₀-there m)  pushOp₀-here      with () ← S[as][bs]⇒∣as∣≢1+∣bs∣ (PushMap-domain-bounded m) refl
  PushMap-functional             (pushOp₀-there m) (pushOp₀-there m') = PushMap-functional m m'
  PushMap-functional {res = res} (pushOp₁-there m)  pushOp₁-here      with ≼-revcat⁻¹ res ≤-refl (PushMap-domain-bounded m)
  PushMap-functional             (pushOp₁-there m)  pushOp₁-here      | here (l ∷ ls) with () ← 1+n≰n l
  PushMap-functional             (pushOp₁-there m)  pushOp₁-here      | there     ls  with () ← S[as][bs]⇒∣as∣≢1+∣bs∣ ls refl
  PushMap-functional             (pushOp₁-there m) (pushOp₁-there m') = PushMap-functional m m'
  PushMap-functional             (popOp         m) (popOp         m') = PushMap-functional m m'

  CCStack : SStack A → OpSeq A stk res → Set₁
  CCStack st ops = Stackᴾ (λ _ → List ℕ)
                          []
                          (λ ks ks' a → (∃ λ k → ks ≡ k ∷ ks')
                                      × PushMap ops ks a)
                          st

  simulate' : {ops : OpSeq A stk res}
            → RunOpSeq ops st s
            → (stᴾ : CCStack st ops')
            → (∀ {ks a} → PushMap ops ks a → PushMap ops' ks a)
            → stᴾ .Stateᴾ stk s
  simulate' (emptyOp     refl) stᴾ inj = stᴾ .emptyᴾ
  simulate' (pushOp₀ run refl) stᴾ inj = stᴾ .pushᴾ ((_ , refl) , inj pushOp₀-here) (simulate' run stᴾ (inj ∘ pushOp₀-there)) refl
  simulate' (pushOp₁ run refl) stᴾ inj = stᴾ .pushᴾ ((_ , refl) , inj pushOp₁-here) (simulate' run stᴾ (inj ∘ pushOp₁-there)) refl
  simulate' (popOp   run eq  ) stᴾ inj with consᴾ ((_ , refl) , _) t ← stᴾ .popᴾ (simulate' run stᴾ (inj ∘ popOp)) (sym eq) = t

  simulate : {ops : OpSeq A stk res}
           → RunOpSeq ops st s
           → (stᴾ : CCStack st ops)
           → stᴾ .Stateᴾ stk s
  simulate run stᴾ = simulate' run stᴾ id

data ListFᴿ {A : Set} {X X' : Set} (Xᴿ : X → X' → Set)
        : ListF ⊤ tt (λ _ _ → A) (λ _ → X ) tt
        → ListF ⊤ tt (λ _ _ → A) (λ _ → X') tt → Set where
  nilᴿ  :                                    ListFᴿ Xᴿ  nil        nil
  consᴿ : ∀ {a a' x x'} → a ≡ a' → Xᴿ x x' → ListFᴿ Xᴿ (cons a x) (cons a' x')

record Bisim {A : Set} (st st' : SStack A) : Set₁ where
  field
    Stateᴿ : st .State tt → st' .State tt → Set
    emptyᴿ : Stateᴿ (st .empty) (st' .empty)
    pushᴿ  : (a : A) → ∀ {s s'}
           → Stateᴿ s s' → Ford² Stateᴿ (st .push a s) (st' .push a s')
    popᴿ   : ∀ {s s'}
           → Stateᴿ s s' → Ford² (ListFᴿ Stateᴿ) (st .pop s) (st' .pop s')

record BisimState {A : Set} (st st' : SStack A)
                  (s : st .State tt) (s' : st' .State tt) : Set where
  constructor bisimState
  field
    {stk res} : List ℕ
    ops  : OpSeq A stk res
    run  : RunOpSeq ops st  s
    run' : RunOpSeq ops st' s'

bisim : (St St' : PStack) → Param St → Param St'
      → (A : Set) → Bisim (inst St A) (inst St' A)
bisim St St' param param' A = record
  { Stateᴿ = BisimState st st'
  ; emptyᴿ = bisimState emptyOp (emptyOp refl) (emptyOp refl)
  ; pushᴿ  = λ { a (bisimState {_} {[]} ops run run') refl refl →
                    bisimState (pushOp₀ a ops) (pushOp₀ run refl) (pushOp₀ run' refl)
               ; a (bisimState {_} {k ∷ res} ops run run') refl refl →
                    bisimState (pushOp₁ a ops) (pushOp₁ run refl) (pushOp₁ run' refl)
               }
  ; popᴿ   = popᴿ-proof
  }
  where

  st st' : SStack A
  st  = inst St  A
  st' = inst St' A

  popᴿ-proof : ∀ {s s'}
             → BisimState st st' s s'
             → Ford² (ListFᴿ (BisimState st st')) (st .pop s) (st' .pop s')
  popᴿ-proof (bisimState ops run run') eq eq'
    with stᴾ  ← CCStack st  ops ∋ param  _ _ _
       | st'ᴾ ← CCStack st' ops ∋ param' _ _ _
       | stᴾ  .popᴾ (simulate run  stᴾ ) eq
       | st'ᴾ .popᴾ (simulate run' st'ᴾ) eq'
  ...  | nilᴾ                     | nilᴾ                      = nilᴿ
  ...  | nilᴾ                     | consᴾ ((_ , ()  ) , _ ) _
  ...  | consᴾ ((_ , ()  ) , _) _ | nilᴾ
  ...  | consᴾ ((_ , refl) , m) _ | consᴾ ((_ , refl) , m') _ =
    consᴿ (PushMap-functional m m')
          (bisimState (popOp ops) (popOp run (sym eq)) (popOp run' (sym eq')))
