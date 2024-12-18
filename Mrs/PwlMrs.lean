import Mrs.Basic
import Mrs.PwlVarFormat 
import Mrs.PwlTransformCore
import Mrs.PwlTypes
import Mrs.PwlTransformShared
import Mrs.Hof
import Lean.Data.HashMap

namespace PWL.MRS 

open HOF (collectQuantifierVars collectHOExtraVarsForEPs collectExtraVarsForEPs collectEvents collectEPsByHandle)
open PWL (joinComma joinSep)
open MM (Multimap)
open PWL.Transform (transform)

def format (sentenceNumber : Nat) (m : MRS.MRS) : (String × List String × List MRS.Var) :=
  let strings : Multimap String MRS.Var := m.preds.foldl (fun stab pred =>
    match pred.carg with
    | some c => stab.insert c pred.label
    | none => stab) Multimap.empty

  let qm := collectQuantifierVars m.preds
  let em := collectHOExtraVarsForEPs m.preds $
            collectHOExtraVarsForEPs m.preds $
            collectHOExtraVarsForEPs m.preds $
            collectHOExtraVarsForEPs m.preds $ collectExtraVarsForEPs m.preds qm
  let hm := collectEPsByHandle m.preds

  match hm.find? m.top with
  | some _ =>
    -- transform already calls serializeFormula from phase3
    let result := transform m.top m.preds hm
    (result, strings.keys, m.preds.foldl (fun acc ep => 
      ep.rargs.foldl (fun inner (_, v) => 
        if v.sort == 'x' then v :: inner else inner
      ) acc
    ) [] |>.eraseDups)
  | none => ("", strings.keys, [])

end PWL.MRS

export PWL.MRS (format)
