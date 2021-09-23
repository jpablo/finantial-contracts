


def main : IO Unit :=
  IO.println "Hello, world!"


inductive Currency
structure Obs (T: Type)

inductive Contract
  | zero: Contract
  | one: Currency -> Contract
  | give: Contract -> Contract
  | and: Contract -> Contract -> Contract
  | or:  Contract -> Contract -> Contract
  | scale: Obs Float -> Contract -> Contract
  | anytime: Contract -> Contract


#check Currency
#print Contract