{-# OPTIONS --cubical #-}

-- Agda version: 2.6.2
-- Standard library version: 1.7
-- Cubical library version: 0.3

open import Function using (id; _∘_)
open import Category.Monad.State
open import Data.Unit
open import Data.Product
open import Data.List using (List; []; _∷_)
open import Data.Char
open import Data.String
open import Cubical.Foundations.Prelude using (_≡_; refl; _∙_; subst; cong; cong₂)


infix 2 _∋_

data _∋_ {A : Set} : List A → A → Set where
  zero : {x   : A} {xs : List A} →          x ∷ xs ∋ x
  suc  : {x y : A} {xs : List A} → xs ∋ x → y ∷ xs ∋ x


--------
-- (Most) simply typed λ-calculus

infixr 5 _⇒_

data Ty : Set where
  ι   : Ty
  _⇒_ : Ty → Ty → Ty

variable σ τ : Ty

Cxt : Set
Cxt = List Ty

variable Γ Γ' Δ Δ' Θ Θ' : Cxt

infix 2 _⊢_

infix  7 `_
infixl 6 _·_
infix  5 ƛ_

data _⊢_ : Cxt → Ty → Set where

  `_  : Γ ∋ τ
      → -----
        Γ ⊢ τ

  _·_ : Γ ⊢ σ ⇒ τ
      → Γ ⊢ σ
      → ---------
        Γ ⊢     τ

  ƛ_  : σ ∷ Γ ⊢ τ
      → -------------
            Γ ⊢ σ ⇒ τ


module DirectRenSub where

  ren : Γ ⊢ τ → (∀ {σ} → Γ ∋ σ → Δ ∋ σ) → Δ ⊢ τ
  ren (` i  ) ρ = ` ρ i
  ren (t · u) ρ = ren t ρ · ren u ρ
  ren (ƛ t  ) ρ = ƛ ren t λ { zero    → zero
                            ; (suc i) → suc (ρ i) }

  sub : Γ ⊢ τ → (∀ {σ} → Γ ∋ σ → Δ ⊢ σ) → Δ ⊢ τ
  sub (` i  ) ρ = ρ i
  sub (t · u) ρ = sub t ρ · sub u ρ
  sub (ƛ t  ) ρ = ƛ sub t λ { zero    → ` zero
                            ; (suc i) → ren (ρ i) suc }


--------
-- Polymorphic functions

PolyTy : Ty → Set → Set
PolyTy ι       X = X
PolyTy (σ ⇒ τ) X = PolyTy σ X → PolyTy τ X

PolyCxt : Cxt → Set → Set
PolyCxt []      X = ⊤
PolyCxt (σ ∷ Γ) X = PolyCxt Γ X × PolyTy σ X

PolyJdg : Cxt → Ty → Set → Set
PolyJdg Γ τ X = PolyCxt Γ X → PolyTy τ X

-- ⟦_⟧ᴾ : Γ ⊢ τ → (X : Set) → PolyJdg Γ τ X

PolyTyR : {X Y : Set} (f : X → Y) → PolyTy τ X → PolyTy τ Y → Set
PolyTyR {τ = ι    }                 f x y = f x ≡ y
PolyTyR {τ = σ ⇒ τ} {X = X} {Y = Y} f g h =
  {x : PolyTy σ X} {y : PolyTy σ Y} → PolyTyR f x y → PolyTyR f (g x) (h y)

PolyCxtR : {X Y : Set} (f : X → Y) → PolyCxt Γ X → PolyCxt Γ Y → Set
PolyCxtR {Γ = []   } f tt      tt        = ⊤
PolyCxtR {Γ = σ ∷ Γ} f (p , q) (p' , q') = PolyCxtR f p p' × PolyTyR f q q'

PolyJdgR : {X Y : Set} (f : X → Y) → PolyJdg Γ τ X → PolyJdg Γ τ Y → Set
PolyJdgR {Γ = Γ} {X = X} {Y = Y} f g h =
  {x : PolyCxt Γ X} {y : PolyCxt Γ Y} → PolyCxtR f x y → PolyTyR f (g x) (h y)


--------
module Traversal where

  Model : Set₁
  Model = Cxt → Ty → Set

  record Env (Γ : Cxt) (M : Model) (Δ : Cxt) : Set where
    constructor env
    field
      lookup : Γ ∋ τ → M Δ τ

  open Env

  _▸_ : {M : Model} → M Δ τ → Env Γ M Δ → Env (τ ∷ Γ) M Δ
  lookup (m ▸ ρ) zero    = m
  lookup (m ▸ ρ) (suc x) = lookup ρ x

  record Semantics : Set₁ where
    field
      Val : Model
      thV : Val Δ τ → Val (σ ∷ Δ) τ
         -- Val Δ τ → ∀ Δ' → Δ ⊆ Δ' → Val Δ' τ
      Com : Model
      var : Val Δ τ → Com Δ τ
      app : Com Δ (σ ⇒ τ) → Com Δ σ → Com Δ τ
      lam : (Val (σ ∷ Δ) σ → Com (σ ∷ Δ) τ) → Com Δ (σ ⇒ τ)
         -- (∀ Δ' → Δ ⊆ Δ' → Val Δ' σ → Com Δ' τ) → Com Δ (σ ⇒ τ)

  open Semantics

  sem : (S : Semantics) → Γ ⊢ τ → Env Γ (S .Val) Δ → S .Com Δ τ
  sem S (` i  ) ρ = S .var (lookup ρ i)
  sem S (t · u) ρ = S .app (sem S t ρ) (sem S u ρ)
  sem S (ƛ t  ) ρ = S .lam λ v → sem S t (v ▸ env (S .thV ∘ lookup ρ))


  --------
  -- Renaming and substitution

  ren : Semantics
  ren .Val Δ τ = Δ ∋ τ
  ren .thV     = suc
  ren .Com Δ τ = Δ ⊢ τ
  ren .var i   = ` i
  ren .app t u = t · u
  ren .lam f   = ƛ f zero

  sub : Semantics
  sub .Val Δ τ = Δ ⊢ τ
  sub .thV t   = sem ren t (env suc)
  sub .Com Δ τ = Δ ⊢ τ
  sub .var i   = i
  sub .app t u = t · u
  sub .lam f   = ƛ f (` zero)


  --------
  -- Term printing

  module _ where

    open RawMonadState (StateMonadState String)

    printer : Semantics
    printer .Val Δ τ   = Char
    printer .thV c     = c
    printer .Com Δ τ   = State String String
    printer .var c     = return (fromChar c)
    printer .app ms mt = do
      s ← ms
      t ← mt
      return ("(" ++ s ++ t ++ ")")
    printer .lam f     = do
      fresh ← get
      let (varName , fresh') = split fresh
      put fresh'
      body ← f varName
      -- put fresh
      return ("(λ" ++ fromChar varName ++ "." ++ body ++ ")")
      where
        split : String → Char × String
        split s with toList s
        split s | []     = ' ' , ""
        split s | x ∷ xs = x   , fromList xs

  testTerm : [] ⊢ ((ι ⇒ ι) ⇒ (ι ⇒ ι) ⇒ ι ⇒ ι) ⇒ ι ⇒ ι
  testTerm = ƛ (ƛ ` suc zero · ` zero · ` zero) · (ƛ ` zero)

  testPrinter : String
  testPrinter = proj₁ (sem {Δ = []} printer testTerm (env λ ()) "abcdefg")


  --------
  -- Polymorphic functions

  fun : Set → Semantics
  fun X .Val Δ τ     = PolyJdg Δ τ X
  fun X .thV f       = f ∘ proj₁
  fun X .Com Δ τ     = PolyJdg Δ τ X
  fun X .var         = id
  fun X .app mf mx r = (mf r) (mx r)
  fun X .lam f       = curry (f proj₂)

  testFun : (X : Set) → ((X → X) → (X → X) → X → X) → X → X
  testFun X = sem (fun X) testTerm (env λ ()) tt


  --------
  -- Simulation (proofs with logical relations)

  record Sim (S S' : Semantics) : Set₁ where
    field
      RVal : S .Val Γ τ → S' .Val Γ τ → Set
      thRV : {v : S .Val Γ τ} {v' : S' .Val Γ τ}
           → RVal v v' → RVal {Γ = σ ∷ Γ} (S .thV v) (S' .thV v')
      RCom : S .Com Γ τ → S' .Com Γ τ → Set
      var  : {v : S .Val Γ τ} {v' : S' .Val Γ τ}
           → RVal v v' → RCom (S .var v) (S' .var v')
      app  : {c  : S  .Com Γ (σ ⇒ τ)} {d  : S  .Com Γ σ}
            {c' : S' .Com Γ (σ ⇒ τ)} {d' : S' .Com Γ σ}
           → RCom c c' → RCom d d' → RCom (S .app c d) (S' .app c' d')
      lam  : {f  : S  .Val (σ ∷ Γ) σ → S  .Com (σ ∷ Γ) τ}
             {f' : S' .Val (σ ∷ Γ) σ → S' .Com (σ ∷ Γ) τ}
           → ((v : S .Val (σ ∷ Γ) σ) (v' : S' .Val (σ ∷ Γ) σ)
             → RVal v v' → RCom (f v) (f' v'))
           → RCom (S .lam f) (S' .lam f')

  open Sim

  record REnv {M M' : Model} (R : ∀ {Δ τ} → M Δ τ → M' Δ τ → Set)
              {Γ Δ : Cxt} (ρ : Env Γ M Δ) (ρ' : Env Γ M' Δ) : Set where
    constructor renv
    field
      lookup : (x : Γ ∋ τ) → R (lookup ρ x) (lookup ρ' x)

  open REnv

  _▸ᴿ_ : {M M' : Model} {R : ∀ {Δ τ} → M Δ τ → M' Δ τ → Set}
       → {v : M Δ τ} {v' : M' Δ τ} → R v v'
       → {ρ : Env Γ M Δ} {ρ' : Env Γ M' Δ} → REnv R ρ ρ' → REnv R (v ▸ ρ) (v' ▸ ρ')
  lookup (vr ▸ᴿ ρr) zero    = vr
  lookup (vr ▸ᴿ ρr) (suc i) = lookup ρr i

  sim : (S S' : Semantics) (R : Sim S S') → (t : Γ ⊢ τ)
      → (ρ : Env Γ (S .Val) Δ) (ρ' : Env Γ (S' .Val) Δ)
      → REnv (R .RVal) ρ ρ' → R .RCom (sem S t ρ) (sem S' t ρ')
  sim S S' R (` i  ) ρ ρ' ρr = R .var (lookup ρr i)
  sim S S' R (t · u) ρ ρ' ρr = R .app (sim S S' R t ρ ρ' ρr) (sim S S' R u ρ ρ' ρr)
  sim S S' R (ƛ t  ) ρ ρ' ρr =
    R .lam λ v v' vr → sim S S' R t (v  ▸  env  (S  .thV ∘ lookup ρ ))
                                    (v' ▸  env  (S' .thV ∘ lookup ρ'))
                                    (vr ▸ᴿ renv (R .thRV ∘ lookup ρr))


  --------
  -- Free theorems

  ftSim : {X Y : Set} (f : X → Y) → Sim (fun X) (fun Y)
  ftSim f .RVal = PolyJdgR f
  ftSim f .thRV = λ { x₀ (x₁ , _) → x₀ x₁ }
  ftSim f .RCom = PolyJdgR f
  ftSim f .var  = id
  ftSim f .app  = λ mf mx r → (mf r) (mx r)
  ftSim f .lam  = λ x₀ x₁ x₂ → x₀ proj₂ proj₂ proj₂ (x₁ , x₂)

  testFtSim : (t : [] ⊢ ι ⇒ ι) (X Y : Set) (f : X → Y)
            → let g = sem (fun X) t (env λ ()) tt
                  h = sem (fun Y) t (env λ ()) tt
              in  (x : X) → f (g x) ≡ h (f x)
  testFtSim t X Y f x =
    sim (fun X) (fun Y) (ftSim f) t (env λ ()) (env λ ()) (renv λ ()) tt refl


--------
-- Induction

VarModel : Set₁
VarModel = Cxt → Ty → Set

record Env (Γ : Cxt) (VM : VarModel) (Δ : Cxt) : Set where
  constructor env
  field
    lookup : Γ ∋ τ → VM Δ τ

open Env

_▸_ : {VM : VarModel} → VM Δ σ → Env Γ VM Δ → Env (σ ∷ Γ) VM Δ
lookup (v ▸ ρ) zero    = v
lookup (v ▸ ρ) (suc i) = lookup ρ i

modify : {VM VM' : VarModel} → (∀ {τ} → VM Δ τ → VM' Δ' τ) → Env Γ VM Δ → Env Γ VM' Δ'
lookup (modify f ρ) i = f (lookup ρ i)

envExt : {VM : VarModel} {e e' : Env Γ VM Δ}
        → (∀ {τ} (j : Γ ∋ τ) → lookup e j ≡ lookup e' j) → e ≡ e'
lookup (envExt eqs i) j = eqs j i

record Semantics : Set₁ where
  field
    VM  : Cxt → Ty → Set
    ext : VM Δ τ → VM (σ ∷ Δ) τ
    TM  : Γ ⊢ τ → Env Γ VM Δ → Set
    var : (i : Γ ∋ τ) (ρ : Env Γ VM Δ) → TM (` i) ρ
    app : {t : Γ ⊢ σ ⇒ τ} {u : Γ ⊢ σ} {ρ : Env Γ VM Δ}
        → TM t ρ → TM u ρ → TM (t · u) ρ
    lam : {t : σ ∷ Γ ⊢ τ} {ρ : Env Γ VM Δ}
        → ((v : VM (σ ∷ Δ) σ) → TM t (v ▸ modify ext ρ)) → TM (ƛ t) ρ

open Semantics

sem : (S : Semantics) (t : Γ ⊢ τ) (ρ : Env Γ (S .VM) Δ) → S .TM t ρ
sem S (` i  ) ρ = S .var i ρ
sem S (t · u) ρ = S .app (sem S t ρ) (sem S u ρ)
sem S (ƛ t  ) ρ = S .lam λ v → sem S t (v ▸ modify (S .ext) ρ)


--------
-- Renaming and substitution

ren : Semantics
ren .VM Δ τ  = Δ ∋ τ
ren .ext     = suc
ren .TM {Γ} {τ} {Δ} _ _ = Δ ⊢ τ
ren .var i ρ = ` lookup ρ i
ren .app t u = t · u
ren .lam f   = ƛ f zero

sub : Semantics
sub .VM Δ τ  = Δ ⊢ τ
sub .ext t   = sem ren t (env suc)
sub .TM {Γ} {τ} {Δ} _ _ = Δ ⊢ τ
sub .var i ρ = lookup ρ i
sub .app t u = t · u
sub .lam f   = ƛ f (` zero)


--------
-- Polymorphic functions

fun : Set → Semantics
fun X .VM Δ τ  = PolyJdg Δ τ X
fun X .ext f   = f ∘ proj₁
fun X .TM {Γ} {τ} {Δ} _ _ = PolyJdg Δ τ X
fun X .var i ρ = lookup ρ i
fun X .app mf mx r = (mf r) (mx r)
fun X .lam f   = curry (f proj₂)

lookup-lemma : (VM VM' : VarModel)
               (ext  : ∀ {Δ σ τ} → VM  Δ τ → VM  (σ ∷ Δ) τ)
               (ext' : ∀ {Δ σ τ} → VM' Δ τ → VM' (σ ∷ Δ) τ)
               (h : ∀ {Δ τ} → VM Δ τ → VM' Δ τ)  -- VarModel homomorphism
             → (∀ {Δ σ τ} (v : VM Δ τ) → h (ext {σ = σ} v) ≡ ext' (h v))
             → (v : VM (σ ∷ Δ) σ) (ρ : Env Γ VM Δ) (j : σ ∷ Γ ∋ τ)
             → h (lookup (  v ▸ modify {VM' = VM } ext ρ) j) ≡
                  lookup (h v ▸ modify {VM' = VM'} ext' (modify {VM' = VM'} h ρ)) j
lookup-lemma VM VM' ext ext' h hom v ρ zero    = refl
lookup-lemma VM VM' ext ext' h hom v ρ (suc j) = hom (lookup ρ j)

ftSim : (X Y : Set) → (X → Y) → Semantics
ftSim X Y f .VM Δ τ = Σ[ p ∈ PolyJdg Δ τ X × PolyJdg Δ τ Y ] PolyJdgR f (proj₁ p) (proj₂ p)
ftSim X Y f .ext ((x , y) , r) = (x ∘ proj₁ , y ∘ proj₁) , r ∘ proj₁
ftSim X Y f .TM t ρ = let ρX = modify (proj₁ ∘ proj₁) ρ
                          ρY = modify (proj₂ ∘ proj₁) ρ
                      in  PolyJdgR f (sem (fun X) t ρX) (sem (fun Y) t ρY)
ftSim X Y f .var i ρ r = proj₂ (lookup ρ i) r
ftSim X Y f .app tr ur r = (tr r) (ur r)
ftSim X Y f .lam {σ} {Γ} {τ} {Δ} {t} {ρ} g {ΓX} {ΓY} r {x} {y} r' =
  subst {A = Env (σ ∷ Γ) (fun X .VM) (σ ∷ Δ) × Env (σ ∷ Γ) (fun Y .VM) (σ ∷ Δ)}
        (λ ρs → PolyTyR f (sem (fun X) t (proj₁ ρs) (ΓX , x))
                          (sem (fun Y) t (proj₂ ρs) (ΓY , y)))
        (cong₂ _,_ (envExt (lookup-lemma
                             (ftSim X Y f .VM) (fun X .VM) (ftSim X Y f .ext) (fun X .ext)
                             (proj₁ ∘ proj₁) (λ _ → refl) ((proj₂ , proj₂) , proj₂) ρ))
                   (envExt (lookup-lemma
                             (ftSim X Y f .VM) (fun Y .VM) (ftSim X Y f .ext) (fun Y .ext)
                             (proj₂ ∘ proj₁) (λ _ → refl) ((proj₂ , proj₂) , proj₂) ρ)))
        (g ((proj₂ , proj₂) , proj₂) (r , r'))

testFt : (t : [] ⊢ ι ⇒ ι) (X Y : Set) (f : X → Y)
        → let g = sem (fun X) t (env λ ()) tt
              h = sem (fun Y) t (env λ ()) tt
          in  (x : X) → f (g x) ≡ h (f x)
testFt t X Y f x = sem (ftSim X Y f) t (env λ ()) tt refl


--------
-- Renaming as substitution

renSub : Semantics
renSub .VM Δ τ = Δ ∋ τ
renSub .ext = suc
renSub .TM t ρ = sem ren t ρ ≡ sem sub t (modify `_ ρ)
renSub .var _ _ = refl
renSub .app teq ueq = cong₂ _·_ teq ueq
renSub .lam {t = t} {ρ} f =
  cong ƛ_ (f zero ∙ cong (sem sub t) (envExt
    (lookup-lemma (ren .VM) (sub .VM) (ren .ext) (sub .ext) `_ (λ _ → refl) zero ρ)))


--------
-- Simulation in terms of induction

record Simulation (S S' : Semantics) : Set₁ where
  field
    VR  : S .VM Δ τ → S' .VM Δ τ → Set
    ext : {v : S .VM Δ τ} {v' : S' .VM Δ τ} → VR v v' → VR (S .ext {σ = σ} v) (S' .ext v')
    TR  : {t : Γ ⊢ τ} {ρ : Env Γ (S .VM) Δ} {ρ' : Env Γ (S' .VM) Δ}
        → S .TM t ρ → S' .TM t ρ' → Set

  SimVM : VarModel
  SimVM Δ τ = Σ[ p ∈ S .VM Δ τ × S' .VM Δ τ ] VR (proj₁ p) (proj₂ p)

  fstEnv : Env Γ SimVM Δ → Env Γ (S  .VM) Δ
  fstEnv = modify (proj₁ ∘ proj₁)

  sndEnv : Env Γ SimVM Δ → Env Γ (S' .VM) Δ
  sndEnv = modify (proj₂ ∘ proj₁)

  SimTM : Γ ⊢ τ → Env Γ SimVM Δ → Set
  SimTM t ρ = TR (sem S t (fstEnv ρ)) (sem S' t (sndEnv ρ))

  field
    var : (i : Γ ∋ τ) (ρ : Env Γ SimVM Δ)
        → TR (S .var i (fstEnv ρ)) (S' .var i (sndEnv ρ))
    app : {t : Γ ⊢ σ ⇒ τ} {u : Γ ⊢ σ} {ρ : Env Γ SimVM Δ}
        → SimTM t ρ → SimTM u ρ → SimTM (t · u) ρ
    lam : {t : σ ∷ Γ ⊢ τ} {ρ : Env Γ SimVM Δ}
        → ((v : S .VM (σ ∷ Δ) σ) (v' : S' .VM (σ ∷ Δ) σ) (r : VR v v')
          → TR (sem S  t (v  ▸ modify (Semantics.ext S ) (fstEnv ρ)))
               (sem S' t (v' ▸ modify (Semantics.ext S') (sndEnv ρ))))
        → SimTM (ƛ t) ρ

open Simulation

sim : (S S' : Semantics) (Sim : Simulation S S') → Semantics
sim S S' Sim .VM = SimVM Sim
sim S S' Sim .ext ((v , v') , r) = ((S .ext v , S' .ext v') , Sim .ext r)
sim S S' Sim .TM = SimTM Sim
sim S S' Sim .var = Sim .var
sim S S' Sim .app {ρ = ρ} = Sim .app {ρ = ρ}
sim S S' Sim .lam {t = t} {ρ} f =
  Sim .lam {ρ = ρ} λ v v' r →
    subst (λ ρs → Sim .TR (sem S t (proj₁ ρs)) (sem S' t (proj₂ ρs)))
          (cong₂ _,_ (envExt (lookup-lemma
                               (sim S S' Sim .VM) (S  .VM) (sim S S' Sim .ext) (S  .ext)
                               (proj₁ ∘ proj₁) (λ _ → refl) ((v , v') , r) ρ))
                     (envExt (lookup-lemma
                               (sim S S' Sim .VM) (S' .VM) (sim S S' Sim .ext) (S' .ext)
                               (proj₂ ∘ proj₁) (λ _ → refl) ((v , v') , r) ρ)))
          (f ((v , v') , r))


--------
-- Free theorems and renaming as substitution in terms of simulation

ftSim' : (X Y : Set) → (X → Y) → Simulation (fun X) (fun Y)
ftSim' X Y f .VR = PolyJdgR f
ftSim' X Y f .ext g = g ∘ proj₁
ftSim' X Y f .TR = PolyJdgR f
ftSim' X Y f .var i ρ r = proj₂ (lookup ρ i) r
ftSim' X Y f .app tr ur r = (tr r) (ur r)
ftSim' X Y f .lam g r r' = g proj₂ proj₂ proj₂ (r , r')

testFt' : (t : [] ⊢ ι ⇒ ι) (X Y : Set) (f : X → Y)
        → let g = sem (fun X) t (env λ ()) tt
              h = sem (fun Y) t (env λ ()) tt
          in  (x : X) → f (g x) ≡ h (f x)
testFt' t X Y f x = sem (sim (fun X) (fun Y) (ftSim' X Y f)) t (env λ ()) tt refl

renSub' : Simulation ren sub
renSub' .VR i t = ` i ≡ t
renSub' .ext eq i = sem ren (eq i) (env suc)
renSub' .TR = _≡_
renSub' .var i ρ = proj₂ (lookup ρ i)
renSub' .app teq ueq = cong₂ _·_ teq ueq
renSub' .lam f = cong ƛ_ (f zero (` zero) refl)


--------
-- Fusion of renamings

compose : {VM : VarModel} → Env Γ _∋_ Δ → Env Δ VM Θ → Env Γ VM Θ
lookup (compose ρ ρ') i = lookup ρ' (lookup ρ i)

renren : Semantics
renren .VM Δ τ = Δ ∋ τ
renren .ext = suc
renren .TM {Δ = Δ} t ρ = {Θ : Cxt} (ρ' : Env Δ (ren .VM) Θ)
                       → sem ren (sem ren t ρ) ρ' ≡ sem ren t (compose ρ ρ')
renren .var i ρ ρ' = refl
renren .app mteq mueq ρ' = cong₂ _·_ (mteq ρ') (mueq ρ')
renren .lam {t = t} f ρ' =
  cong ƛ_ (f zero (zero ▸ modify suc ρ') ∙ cong (sem ren t) (envExt lem))
  where
    lem : {ρ : Env Γ _∋_ Δ} {ρ' : Env Δ _∋_ Θ} (j : σ ∷ Γ ∋ τ)
        → lookup (compose (zero ▸ modify {VM' = _∋_} suc ρ )
                          (zero ▸ modify {VM' = _∋_} suc ρ')) j ≡
          lookup (zero ▸ modify {VM' = _∋_} suc (compose ρ ρ')) j
    lem zero    = refl
    lem (suc j) = refl