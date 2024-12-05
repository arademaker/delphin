import Mrs.Basic

namespace PWL.Arguments

open MRS (EP Var)

structure ArgMap where
  name: String
  expected_position: Nat
  deriving Inhabited

abbrev ArgMaps := Array ArgMap

def checkArgPositions (ep: EP) (expected: Array ArgMap) : Option EP := 
  if ep.rargs.length != expected.size then none
  else
    let allMatch := expected.foldl (fun acc map =>
      let foundArg := ep.rargs.find? fun r => r.1 == map.name
      match foundArg with
      | none => false
      | some arg => acc && ep.rargs[map.expected_position]! == arg) true
    if allMatch then some ep else none

def properQArgs : ArgMaps := #[
  ⟨"ARG0", 0⟩,
  ⟨"RSTR", 1⟩,
  ⟨"BODY", 2⟩
]

def compoundArgs : ArgMaps := #[
  ⟨"ARG0", 0⟩,
  ⟨"ARG1", 1⟩,
  ⟨"ARG2", 2⟩
]

def namedArgs : ArgMaps := #[
  ⟨"ARG0", 0⟩
]

def getArg (ep: EP) (name: String) : Option Var :=
  ep.rargs.find? (fun r => r.1 == name)
  |>.map (fun r => r.2)

end PWL.Arguments

export PWL.Arguments (ArgMap ArgMaps properQArgs compoundArgs namedArgs checkArgPositions getArg)
