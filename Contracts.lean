
def main : IO Unit :=
  IO.println "Hello, world!"


inductive Currency
 | GBP


open Currency


def Date := String


structure Obs (T: Type) :=
  (value: Date -> IO T)
  

def konst {T} (t: T) : Obs T := 
  ⟨λ _ => t⟩ 


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


def scaleK {C} (x: Float) (t: C) [c: Contract C] : C :=
  c.scale (konst x) t

def zcb {C} (t: Date) (x: Float) (k: Currency) [c: Contract C] : C :=
  scaleK x (c.get (c.truncate t (c.one k)))


-- example
def t1: Date := "2022/01/01"
def t2: Date := "2022/02/01"



def c1 {C} [c: Contract C]: C :=
  zcb t1 100 GBP

def c2 {C} [c: Contract C]: C := 
  zcb t2 200 GBP

def c3 {C} [c: Contract C] := 
  c.and c1 c2

def c4 {C} [c: Contract C] := 
  c.and c1 (c.give c2)

-- Combinators

def andGive {C} [c: Contract C] (c1 c2: C) : C :=
  c.and c1 (c.give c2)

def c4' {C} [c: Contract C] (c1 c2: C) := 
  andGive c1 c2


