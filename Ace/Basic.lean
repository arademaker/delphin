-- import Init.System.IO
-- import Std
import Mrs.Parser
-- open Std
open IO.Process
open MRS
/- util for low-level acess to Ace process -/

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

structure Result where
 derivation : String
 mrs : MRS
deriving Repr

structure Response where
 results : List Result
deriving Repr

def run_ace (sentence : String) : IO (List MRS) := do
  let ret ← cmd_with_stdin {cmd := "ace", args := #["-g","/Users/ar/r/erg.dat","-T"], cwd := "."} sentence
  let res := ret.stdout.splitOn "\n"
  let  ms := res.filter (fun s => s.startsWith "[")
  let   p := Std.Internal.Parsec.String.Parser.run parseMRS
  let  as ← (ms.map p).mapM IO.ofExcept
  return as
