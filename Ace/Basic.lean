import Init.System.IO
import Std

open Std
open IO.Process

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
  let res ← cmd_with_stdin {cmd := "ace", args := #["-g","/Users/ar/r/erg.dat","-T"], cwd := "."} sentence
  IO.println res.stdout
