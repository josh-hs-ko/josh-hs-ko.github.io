{-# OPTIONS --cubical --guardedness #-}

-- Agda version: 2.6.2
-- Cubical library version: 0.3

open import Cubical.Foundations.Prelude
open import Cubical.Foundations.Function
open import Cubical.Data.Unit
open import Cubical.Data.Sigma
open import Cubical.Data.Nat


variable A B S : Type

id : A → A
id a = a


--------
-- Traced state monad

mutual

  record Trace (S A : Type) : Type where
    coinductive
    field
      force : TraceStep S A

  data TraceStep (S A : Type) : Type where
    ret : A → S → TraceStep S A
    int : S → Trace S A → TraceStep S A
open Trace

States : Type → Type → Type
States S A = S → Trace S A

fmapTrace : (A → B) → Trace S A → Trace S B
force (fmapTrace f ta) with force ta
... | ret a s   = ret (f a) s
... | int t ta' = int t (fmapTrace f ta')

fmap : (A → B) → States S A → States S B
fmap f ma s = fmapTrace f (ma s)

return : A → States S A
force (return a s) = ret a s

joinTrace : Trace S (States S A) → Trace S A
force (joinTrace tma) with force tma
... | ret ma s    = force (ma s)
... | int t  tma' = int t (joinTrace tma')

join : States S (States S A) → States S A
join mma s = joinTrace (mma s)

_>>=_ : States S A → (A → States S B) → States S B
ma >>= f = join (fmap f ma)

get : States S S
force (get s) = ret s s

put : S → States S Unit
force (put s _) = ret tt s

modify : (S → S) → States S Unit
modify f = get >>= (put ∘ f)

mark : States S Unit
force (mark s) = int s (return tt s)


--------
-- Monad laws

leftUnit : join ∘ return ≡ id {States S A}
force (leftUnit i ma s) = force (ma s)

rightUnit : join ∘ fmap return ≡ id {States S A}
rightUnit i mma s = rightUnitTrace i (mma s)
  where
    rightUnitTrace : joinTrace ∘ fmapTrace return ≡ id {Trace S A}
    force (rightUnitTrace i t) with force t
    ... | ret f s  = ret f s
    ... | int s t' = int s (rightUnitTrace i t')

assoc : join {S} {A} ∘ join ≡ join ∘ fmap join
assoc i mmma s = assocTrace i (mmma s)
  where
    assocTrace : joinTrace ∘ joinTrace ≡ joinTrace ∘ fmapTrace join
    force (assocTrace i tmma) with force tmma
    force (assocTrace i tmma) | ret mma s with force (mma s)
    force (assocTrace i tmma) | ret mma s | ret ma s' = force (ma s')
    force (assocTrace i tmma) | ret mma s | int t tma = int t (joinTrace tma)
    force (assocTrace i tmma) | int t tmma' = int t (assocTrace i tmma')

getput : (get >>= put) ≡ return {S = S} tt
force (getput i s) = ret tt s

putput : (s s' : S)
       → (put s >>= λ _ → put s') ≡ put s'
force (putput s s' i _) = ret tt s'

getget : (k : S → S → States S A)
       → (get >>= λ s → get >>= λ s' → k s s') ≡ (get >>= λ s → k s s)
force (getget k i s) = force (k s s s)

putget : (k : S → S → States S A) (s : S)
       → (put s >>= λ _ → get >>= λ s' → k s s') ≡ (put s >>= λ _ → k s s)
force (putget k s i _) = force (k s s s)