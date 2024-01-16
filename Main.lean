import Init.System.IO
import Std
import «Mrs»
import Lean.Data.Parsec
import Ace

#eval run_ace "Adam loves Beth."
-- def test := "..."
-- #eval test.drop 2415
-- #eval parseMRS test.mkIterator

def report (r : String × Except String MRS) :=
 match r.2 with
 | Except.ok b => IO.println $ List.length b.preds
 | Except.error e => IO.println (r.1, e)

def main : IO Unit := do
  let ls ← IO.FS.lines "/Users/ar/Temp/ws201.txt"
  let rs := ls.toList.map (λ l => (l, Lean.Parsec.run parseMRS l))
  let _ ← rs.mapM report
  return ()
