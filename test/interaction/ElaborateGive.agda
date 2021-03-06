module ElaborateGive where

open import Agda.Builtin.List
open import Agda.Builtin.Nat
open import Agda.Builtin.Reflection
open import Agda.Builtin.String
open import Agda.Builtin.Unit

five : Nat
five = {!!}

five' : Nat
five' = {!!}

five'' : Nat
five'' = {!!}

vArg : {A : Set} → A → Arg A
vArg = arg (arg-info visible (modality relevant quantity-ω))

macro
  addUnknown : Term → TC ⊤
  addUnknown goal = unify goal
                          (def (quote _+_)
                            (vArg unknown ∷ vArg unknown ∷ []))

unknownPlus : Nat
unknownPlus = {!!} + {!!}

macro
  literalNat : Nat → Term → TC ⊤
  literalNat n hole = unify hole (lit (nat n))

macro
  literalString : String → Term → TC ⊤
  literalString s hole = unify hole (lit (string s))

seven : Nat
seven = {!!}

seven' : Nat
seven' = {!!}

hello : String
hello = {!!}

hello' : String
hello' = {!!}

hi : String
hi = {!!}
