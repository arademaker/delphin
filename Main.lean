import Init.System.IO
import Std
import «Mrs»
import Lean.Data.Parsec

open Std
open IO.Process

/-- Pipe input into stdin of the spawned process, then return output upon completion. -/
def cmd_with_stdin (args : SpawnArgs) (input : String) : IO Output := do
  let child ← spawn { args with stdin := .piped, stdout := .piped, stderr := .piped }
  let (stdin, child) ← child.takeStdin
  stdin.putStr input
  stdin.flush
  let stdout ← IO.asTask child.stdout.readToEnd Task.Priority.dedicated
  let stderr ← child.stderr.readToEnd
  let exitCode ← child.wait
  let stdout ← IO.ofExcept stdout.get
  return { exitCode, stdout, stderr }

def run_ace (sentence : String) : IO Unit := do
  let res ← cmd_with_stdin {cmd := "ace", args := #["-g","/Users/ar/r/erg.dat","-T","-n 1"], cwd := "."} sentence
  IO.println res.stdout

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
