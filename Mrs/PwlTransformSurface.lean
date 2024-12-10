import Mrs.Basic
import Mrs.PwlTransformShared

namespace PWL.TransformSurface

open MRS (EP Var)
open PWL.Transform (PWLQuantifier TransformResult buildNested)

def logPrefix := "/* "
def logSuffix := " */"

def logRewrite (input : String) (output : String) : String :=
  if logPrefix != "" then
    s!"{logPrefix}{input}{logSuffix} {output}"
  else
    output

def formatToSurface (result : TransformResult) : String :=
  let formatQuantifier (quantifier : PWLQuantifier) : String := 
    match quantifier with
    | PWLQuantifier.proper_q var name =>
      let input := "proper_q"
      let output := s!"?[n]:(name(n) & arg1(n)={var} & arg2(n)={name})"
      logRewrite input output

    | PWLQuantifier.some_q var rstr body =>
      let input := "some_q"
      let output := s!"({rstr}) & {body}"
      logRewrite input output 

    | PWLQuantifier.other_q "no_q" var rstr body =>
      let input := "no_q"
      let output := s!"![{var}]:({rstr} => ~({body}))"
      logRewrite input output

    | PWLQuantifier.other_q "the_q" var rstr body =>
      let input := "the_q"
      let output := s!"?[S]:(S=^[s]:({rstr}) & size(S)=1 & S({var}) & ({body}))"
      logRewrite input output

    | PWLQuantifier.other_q "every_q" var rstr body =>
      let input := "every_q"
      let output := s!"![{var}]:({rstr} => {body})"
      logRewrite input output

    | PWLQuantifier.other_q name var rstr body =>
      let input := name
      let output := s!"{name}({var}, ({rstr}), {body})"
      logRewrite input output

  let incremental := true

  if !incremental then
    let vars := result.vars.reverse.eraseDups
    let varList := String.intercalate "," (vars.map toString)
    let quantStrs := result.quants.map formatQuantifier
    let eqStrs := result.eqs.map fun (v1, v2) => s!"{v1}={v2}"
    let body := String.intercalate " &\n  " (quantStrs ++ eqStrs)
    s!"?[{varList}]:(\n  {body}\n)"
  else
    let allExpressions : List (List Var × String) := 
      (result.quants.map fun q => 
        match q with
        | PWLQuantifier.proper_q var _ => ([var], formatQuantifier q)
        | PWLQuantifier.some_q var _ _ => ([var], formatQuantifier q)
        | PWLQuantifier.other_q _ var _ _ => ([var], formatQuantifier q)) ++
      (result.eqs.map fun (v1, v2) => ([v1, v2], s!"{v1}={v2}"))

    buildNested allExpressions []

end PWL.TransformSurface

export PWL.TransformSurface (formatToSurface)
