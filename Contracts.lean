
def main : IO Unit :=
  IO.println "Hello, world!"


inductive Currency
 | GBP

structure Obs (T: Type) :=
  value: T

def Date := String

-- inductive Contract
--   | zero: Contract
--   | one: Currency -> Contract
--   | give: Contract -> Contract
--   | and: Contract -> Contract -> Contract
--   | or:  Contract -> Contract -> Contract
--   | truncate: Date -> Contract -> Contract
--   | then_: Contract -> Contract -> Contract
--   | scale: Obs Float -> Contract -> Contract
--   | get: Contract -> Contract
--   | anytime: Contract -> Contract


-- #check Currency
-- #print Contract

-- open Contract
-- open Currency

-- def c5 := one GBP

-- def c5' := give c5

-- def t1 := "t1"

-- def c6 := get (truncate t1 (one GBP))

class Contract (T: Type) where
  zero    : T
  one     : Currency -> T
  give    : T -> T
  and     : T -> T -> T
  or      : T -> T -> T
  truncate: Date -> T -> T
  then_   : T -> T -> T
  scale   : Obs Float -> T -> T
  get     : T -> T
  anytime : T -> T
  -- 
  and_assoc      : (a b c : T) -> (and (and a b) c) = (and a (and b c))
  and_empty_right: (a : T) -> and a zero = a
  and_empty_left : (a : T) -> and zero a = a

class Monoid (A : Type) where
  append : A → A → A
  empty  : A
  assoc  : (a₁ a₂ a₃ : A) -> append (append a₁ a₂) a₃ = append a₁ (append a₂ a₃)
  append_empty_right : (a : A) -> append a empty = a
  append_empty_left  : (a : A) -> append empty a = a

instance contractAndIsMonoid (T: Type) [ct: Contract T]: Monoid T := {
  append       := ct.and,
  empty        := ct.zero,
  assoc        := ct.and_assoc,
  append_empty_left  := ct.and_empty_left
  append_empty_right := ct.and_empty_right
} 

#check contractAndIsMonoid






