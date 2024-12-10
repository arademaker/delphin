import Mrs.Basic

namespace PWL.Transform

open MRS (EP Var)

inductive PWLQuantifier where
  | proper_q : Var → String → PWLQuantifier  -- var, name
  | some_q : Var → String → String → PWLQuantifier  -- var, expanded_rstr, expanded_body
  | other_q : String → Var → String → String → PWLQuantifier  -- predname, var, expanded_rstr, expanded_body
  deriving BEq, Inhabited

structure TransformResult where
  quants : List PWLQuantifier 
  eqs : List (Var × Var)      
  vars : List Var              
  deriving Inhabited

partial def buildNested (remaining : List (List Var × String)) (seenVars : List Var) : String :=
  match remaining with
  | [] => ""
  | exprs =>
    let foundPair := List.find? (fun pair => 
      let vars := pair.1
      let newVars := vars.filter (fun v => !seenVars.contains v)
      newVars.length ≤ 1) exprs

    match foundPair with
    | none => ""
    | some expr =>
      let vars := expr.1
      let exprStr := expr.2
      let newVars := vars.filter (fun v => !seenVars.contains v)
      let restExprs := exprs.filter (fun x => x != expr)
      let connector := if restExprs.isEmpty then "" else " &\n  "
      match newVars with
      | [] => exprStr ++ connector ++ buildNested restExprs seenVars
      | [v] => s!"?[{v}]:(" ++ exprStr ++ connector ++ buildNested restExprs (v :: seenVars) ++ ")"
      | _ => ""

end PWL.Transform

export PWL.Transform (PWLQuantifier TransformResult buildNested)
