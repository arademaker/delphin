import Mrs.Basic
import Mrs.PwlVarFormat 
import Mrs.PwlTransformCore
import Mrs.Hof
import Mrs.PwlTypes
import Lean.Data.HashMap

namespace PWL.MRS 

open HOF (collectQuantifierVars collectHOExtraVarsForEPs collectExtraVarsForEPs collectEvents collectEPsByHandle)
open PWL (joinComma joinSep)
open MM (Multimap)
open Lean (RBMap HashMap)

structure ConcatenatedNamed where
  canonicalHandle : MRS.Var
  combinedName : String
  derivedFrom : List MRS.Var
deriving Inhabited

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
    let transformed := Transform.transform m.preds hm
    (transformed, strings.keys, m.preds.foldl (fun acc ep => 
      ep.rargs.foldl (fun inner (_, v) => 
        if v.sort == 'x' then v :: inner else inner
      ) acc
    ) [] |>.eraseDups)
  | none => ("", strings.keys, [])

end PWL.MRS

export PWL.MRS (format)
