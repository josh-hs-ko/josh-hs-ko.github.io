{-# OPTIONS --cubical --guardedness --termination-depth=2 #-}

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
-- Left scan on colists

data ListF (A X : Type) : Type where
  []  : ListF A X
  _∷_ : A → X → ListF A X

record Colist (A : Type) : Type where
  coinductive
  field
    decon : ListF A (Colist A)

open Colist

module Productivity-check-fails where

  {-# TERMINATING #-}
  scanl : (S → A → S) → Colist A → States S Unit
  scanl f as with decon as
  ... | []      = return tt
  ... | a ∷ as' = mark              >>= λ _ →
                  modify (flip f a) >>= λ _ →
                  scanl f as'

module Definition-expanded where

  scanl : (S → A → S) → Colist A → States S Unit
  force (scanl f as s) with decon as
  ... | []      = ret tt s
  ... | a ∷ as' = int s (scanl f as' (f s a))

module Productivity-check-fails-but-probably-shouldn't where

  modify' : (S → S) → States S A → States S A
  modify' f ma s = ma (f s)

  mark' : States S A → States S A
  force (mark' ma s) = int s (ma s)

  {-# TERMINATING #-}
  scanl : (S → A → S) → Colist A → States S Unit
  force (scanl f as s) with decon as
  ... | []      = force (return tt s)
  ... | a ∷ as' = force (mark' (modify' (flip f a) (scanl f as')) s)

module Danielsson's-embedded-language-technique where

  infix 3 mark>>_
  infix 4 _>==_

  mutual

    record SynStates (S A : Type) : Type₁ where
      coinductive
      field
        force : Productive S A

    data Productive (S : Type) : Type → Type₁ where
      norec   : States S A → Productive S A
      mark>>_ : Marked S A → Productive S A
      _>==_   : States S B → (B → Productive S A) → Productive S A

    data Marked (S : Type) : Type → Type₁ where
      corec : SynStates S A → Marked S A
      _>==_ : States S B → (B → Marked S A) → Marked S A

    -- data Marked (S : Type) : Type → Type₁ where
    --   corec : SynStates S A → Marked S A
    --   embed :    States S A → Marked S A
    --   _>==_ : Marked S B → (B → Marked S A) → Marked S A

  open SynStates

  synScanl : (S → A → S) → Colist A → SynStates S Unit
  force (synScanl f as) with decon as
  ... | []      = norec (return tt)
  ... | a ∷ as' = mark>>
                  modify (flip f a) >== λ _ →
                  corec (synScanl f as')

  mutual

    eval : SynStates S A → States S A
    eval sa = evalP (force sa)

    evalP : Productive S A → States S A
    force (evalP (norec ma)  s) = force (ma s)
    force (evalP (mark>> ka) s) = int s (evalM ka s)
    force (evalP (mb >== f)  s) = force (evalTP (mb s) f)

    evalTP : Trace S B → (B → Productive S A) → Trace S A
    force (evalTP tb f) with force tb
    ... | ret b s   = force (evalP (f b) s)
    ... | int s tb' = int s (evalTP tb' f)

    evalM : Marked S A → States S A
    evalM (corec sa) = eval sa
    evalM (mb >== f) = λ s → evalTM (mb s) f

    evalTM : Trace S B → (B → Marked S A) → Trace S A
    force (evalTM tb f) with force tb
    ... | ret b s   = force (evalM (f b) s)
    ... | int s tb' = int s (evalTM tb' f)

  scanl : (S → A → S) → Colist A → States S Unit
  scanl f as = eval (synScanl f as)