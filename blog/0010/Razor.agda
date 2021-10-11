-- Agda version: 2.6.2
-- Standard library version: 1.7

open import Function
open import Data.Product
open import Data.Unit
open import Data.Nat hiding (_^_)
open import Data.List
open import Data.Vec hiding (length; _++_; foldr)
open import Relation.Binary.PropositionalEquality


variable
  k m n : ℕ

infixr 5 _∔_

data Expr : Set where
  lit   : ℕ → Expr
  _∔_   : Expr → Expr → Expr
  binOp : (ℕ → ℕ → ℕ) → Expr → Expr → Expr

testExpr : Expr
testExpr = lit 0 ∔ lit 1 ∔ lit 2 ∔ lit 3 ∔ lit 4 ∔ lit 5

eval : Expr → ℕ
eval (lit x) = x
eval (l ∔ r) = eval l + eval r
eval (binOp b l r) = b (eval l) (eval r)

Stack : ℕ → Set
Stack = Vec ℕ

apply : (ℕ → ℕ → ℕ) → Stack (2 + n) → Stack (1 + n)
apply f (x ∷ y ∷ xs) = f x y ∷ xs

infixr 5 _▷_

data Prog : ℕ → ℕ → Set where
  push  : ℕ → Prog n (1 + n)
  add   : Prog (2 + n) (1 + n)
  binOp : (b : ℕ → ℕ → ℕ) → Prog (2 + n) (1 + n)
  _▷_   : Prog k m → Prog m n → Prog k n

⟦_⟧ : Prog m n → Stack m → Stack n
⟦ push x  ⟧ xs           = x ∷ xs
⟦ add     ⟧ (x ∷ y ∷ xs) = (x + y) ∷ xs
⟦ p ▷ q   ⟧ xs           = ⟦ q ⟧ (⟦ p ⟧ xs)
⟦ binOp b ⟧ (x ∷ y ∷ xs) = b x y ∷ xs

compile : Expr → Prog n (1 + n)
compile (lit x)       = push x
compile (l ∔ r)       = compile r ▷ compile l ▷ add  -- order of compilation matters
compile (binOp b l r) = compile r ▷ compile l ▷ binOp b

soundness : (e : Expr) (xs : Stack n) → ⟦ compile e ⟧ xs ≡ eval e ∷ xs
soundness (lit x) xs =
  begin
    ⟦ compile (lit x) ⟧ xs
  ≡⟨ refl ⟩
    ⟦ push x ⟧ xs
  ≡⟨ refl ⟩
    x ∷ xs
  ≡⟨ refl ⟩
    eval (lit x) ∷ xs
  ∎
  where open ≡-Reasoning
soundness (l ∔ r) xs =
  begin
    ⟦ compile (l ∔ r) ⟧ xs
  ≡⟨ refl ⟩
    ⟦ compile r ▷ compile l ▷ add ⟧ xs
  ≡⟨ refl ⟩
    ⟦ add ⟧ (⟦ compile l ⟧ (⟦ compile r ⟧ xs))
  ≡⟨ cong (⟦ add ⟧ ∘ ⟦ compile l ⟧) (soundness r xs) ⟩
    ⟦ add ⟧ (⟦ compile l ⟧ (eval r ∷ xs))
  ≡⟨ cong ⟦ add ⟧ (soundness l (eval r ∷ xs)) ⟩
    ⟦ add ⟧ (eval l ∷ eval r ∷ xs)
  ≡⟨ refl ⟩
    (eval l + eval r) ∷ xs
  ≡⟨ refl ⟩
    eval (l ∔ r) ∷ xs
  ∎
  where open ≡-Reasoning
soundness (binOp b l r) xs =
  begin
    ⟦ compile (binOp b l r) ⟧ xs
  ≡⟨ refl ⟩
    ⟦ compile r ▷ compile l ▷ binOp b ⟧ xs
  ≡⟨ refl ⟩
    ⟦ binOp b ⟧ (⟦ compile l ⟧ (⟦ compile r ⟧ xs))
  ≡⟨ cong (⟦ binOp b ⟧ ∘ ⟦ compile l ⟧) (soundness r xs) ⟩
    ⟦ binOp b ⟧ (⟦ compile l ⟧ (eval r ∷ xs))
  ≡⟨ cong ⟦ binOp b ⟧ (soundness l (eval r ∷ xs)) ⟩
    ⟦ binOp b ⟧ (eval l ∷ eval r ∷ xs)
  ≡⟨ refl ⟩
    b (eval l) (eval r) ∷ xs
  ≡⟨ refl ⟩
    eval (binOp b l r) ∷ xs
  ∎
  where open ≡-Reasoning

data Prog⁺ : (m n : ℕ) → (Stack m → Stack n) → Set where
  push  : (x : ℕ) → Prog⁺ n (1 + n) (x ∷_)
  add   : Prog⁺ (2 + n) (1 + n) (apply _+_)
  binOp : (b : ℕ → ℕ → ℕ) → Prog⁺ (2 + n) (1 + n) (apply b)
  _▷_   : ∀ {f g} → Prog⁺ k m f → Prog⁺ m n g → Prog⁺ k n (g ∘ f)

compile⁺ : (e : Expr) → Prog⁺ n (1 + n) (eval e ∷_)
compile⁺ (lit x) = push x
compile⁺ (l ∔ r) = compile⁺ r ▷ compile⁺ l ▷ add
compile⁺ (binOp b l r) = compile⁺ r ▷ compile⁺ l ▷ binOp b

forget : ∀ {f} → Prog⁺ m n f → Prog m n
forget (push x)  = push x
forget add       = add
forget (p ▷ q)   = forget p ▷ forget q
forget (binOp b) = binOp b

recomputation : ∀ {f} (p : Prog⁺ m n f) xs → ⟦ forget p ⟧ xs ≡ f xs
recomputation (push x) xs           = refl
recomputation add      (x ∷ y ∷ xs) = refl
recomputation (_▷_ {f = f} {g} p q) xs =
  begin
    ⟦ forget q ⟧ (⟦ forget p ⟧ xs)
  ≡⟨ recomputation q (⟦ forget p ⟧ xs) ⟩
   g (⟦ forget p ⟧ xs)
  ≡⟨ cong g (recomputation p xs) ⟩
    g (f xs)
  ∎
  where open ≡-Reasoning
recomputation (binOp b) (x ∷ y ∷ xs) = refl

compile' : Expr → Prog n (1 + n)
compile' = forget ∘ compile⁺

soundness' : (e : Expr) (xs : Stack n) → ⟦ compile' e ⟧ xs ≡ eval e ∷ xs
soundness' e = recomputation (compile⁺ e)

data Instr : (m n : ℕ) → (Stack m → Stack n) → Set where
  push  : (x : ℕ) → Instr n (1 + n) (x ∷_)
  add   : Instr (2 + n) (1 + n) (apply _+_)
  binOp : (b : ℕ → ℕ → ℕ) → Instr (2 + n) (1 + n) (apply b)

infixr 5 _▸_
infixr 4 _▸▸_

data Prog⁺⁺ : (m n : ℕ) → (Stack m → Stack n) → Set where
  end : Prog⁺⁺ n n id
  _▸_ : ∀ {f g} → Instr k m f → Prog⁺⁺ m n g → Prog⁺⁺ k n (g ∘ f)

⟦_⟧⁺⁺ : ∀ {f} → Prog⁺⁺ m n f → Stack m → Stack n
⟦ end         ⟧⁺⁺ xs = xs
⟦ push x  ▸ p ⟧⁺⁺ xs = ⟦ p ⟧⁺⁺ (x ∷ xs)
⟦ add     ▸ p ⟧⁺⁺ (x ∷ y ∷ xs) = ⟦ p ⟧⁺⁺ ((x + y) ∷ xs)
⟦ binOp b ▸ p ⟧⁺⁺ (x ∷ y ∷ xs) = ⟦ p ⟧⁺⁺ (b x y   ∷ xs)

_▸▸_ : ∀ {f g} → Prog⁺⁺ k m f → Prog⁺⁺ m n g → Prog⁺⁺ k n (g ∘ f)
end     ▸▸ q = q
(i ▸ p) ▸▸ q = i ▸ (p ▸▸ q)

compile⁺⁺ : (e : Expr) → Prog⁺⁺ n (1 + n) (eval e ∷_)
compile⁺⁺ (lit x) = push x ▸ end
compile⁺⁺ (l ∔ r) = compile⁺⁺ r ▸▸ compile⁺⁺ l ▸▸ add ▸ end
compile⁺⁺ (binOp b l r) = compile⁺⁺ r ▸▸ compile⁺⁺ l ▸▸ binOp b ▸ end

append : ∀ {f g} → Prog⁺ k m f → Prog⁺ m n g → Prog⁺ k n (g ∘ f)
append (push x)  q = push x ▷ q
append add       q = add ▷ q
append (binOp b) q = binOp b ▷ q
append (p ▷ p')  q = p ▷ append p' q

normalise : ∀ {f} → Prog⁺ m n f → Prog⁺ m n f
normalise (push x)  = push x
normalise add       = add
normalise (binOp b) = binOp b
normalise (p ▷ q)   = append (normalise p) (normalise q)

data AlgList (A {B} : Set) (f : A → B → B) (e : B) : B → Set where
  []  : AlgList A f e e
  _∷_ : (x : A) {y : B} → AlgList A f e y → AlgList A f e (f x y)

forgetAL : {A B : Set} {f : A → B → B} {e : B} {y : B}
         → AlgList A f e y → List A
forgetAL []       = []
forgetAL (x ∷ xs) = x ∷ forgetAL xs

recomputationAL : {A B : Set} {f : A → B → B} {e : B} {y : B}
                → (xs : AlgList A f e y) → foldr f e (forgetAL xs) ≡ y
recomputationAL {f = f} [] = refl
recomputationAL {f = f} (x ∷ xs) = cong (f x) (recomputationAL xs)