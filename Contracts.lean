


def main : IO Unit :=
  IO.println "Hello, world!"


inductive Currency
 | GBP

structure Obs (T: Type) :=
  value: T

def Date := String

inductive Contract
  | zero: Contract
  | one: Currency -> Contract
  | give: Contract -> Contract
  | and: Contract -> Contract -> Contract
  | or:  Contract -> Contract -> Contract
  | truncate: Date -> Contract -> Contract
  | then_: Contract -> Contract -> Contract
  | scale: Obs Float -> Contract -> Contract
  | get: Contract -> Contract
  | anytime: Contract -> Contract


#check Currency
#print Contract

open Contract
open Currency

def c5 := one GBP

