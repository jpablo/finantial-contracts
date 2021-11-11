

inductive Currency
 | GBP

-- for now
def Date := String


structure Obs (T: Type) :=
  (value: Date -> IO T)
  

def konst {T} (t: T) : Obs T := 
  ⟨fun _ => t⟩ 



def lift {A B} (f: A -> B) (o: Obs A) : Obs B :=
  ⟨fun d => do f (<- o.value d)⟩


#print lift


-- def lift' : {A B : Type} → (A → B) → Obs A → Obs B :=
-- fun {A B} f o => { 
--   value :=
--       fun d =>
--         do 
--           let a ← Obs.value o d 
--           pure (f a) 
-- }

def lift2 {A B C} (f: A -> B -> C) (oa: Obs A) (ob: Obs B): Obs C :=
  ⟨fun d => do f (<- oa.value d) (<- ob.value d)⟩

#print lift2



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

-- #check contractAndIsMonoid

class Group (G: Type) extends Monoid G where
  negate : G -> G
  -- equations



instance obsGroup (T: Type) [g: Group T]: Group (Obs T) := sorry


def scaleK {C} (x: Float) (t: C) [c: Contract C] : C :=
  c.scale (konst x) t

def zcb {C} (t: Date) (x: Float) (k: Currency) [c: Contract C] : C :=
  scaleK x (c.get (c.truncate t (c.one k)))


-- example
def t1: Date := "2022/01/01"
def t2: Date := "2022/02/01"

open Currency

namespace examples

  variable {C: Type} [c: Contract C]

  def c1: C := zcb t1 100 GBP
  def c2: C := zcb t2 200 GBP
  def c3    := c.and c1 c2
  def c4    := c.and c1 (c.give c2)

  -- Combinators

  def andGive (x y: C) : C := c.and x (c.give y)

  def c4' : C := andGive c1 c2


end examples