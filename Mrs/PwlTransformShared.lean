import Mrs.Basic

namespace PWL.Transform

open MRS (Var EP Constraint MRS)

/-- Represents the different types of logical quantifiers and operators in the PWL format -/
inductive PWLQuantifier where
  | proper_q : Var → String → PWLQuantifier            -- var, name
  | indefinite_q : String → Var → String → String → PWLQuantifier  -- predname (some_q/a_q/udef_q/pronoun_q), var, expanded_rstr, expanded_body
  | definite_q : String → Var → String → String → PWLQuantifier   -- predname (the_q/def_explicit_q), var, expanded_rstr, expanded_body
  | other_q : String → Var → String → String → PWLQuantifier  -- predname (no_q/every_q), var, expanded_rstr, expanded_body
  | negation : String → String → PWLQuantifier  -- predname (neg/never_a_1), expanded_body
  deriving BEq, Inhabited

/-- Collects the results of transforming predicates into PWL format -/
structure TransformResult where
  quants : List PWLQuantifier    -- List of transformed quantifiers
  eqs : List (Var × Var)        -- List of variable equalities
  vars : List Var               -- List of variables in scope
  deriving Inhabited

/-- Builds nested quantifier expressions with proper variable scoping -/
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
