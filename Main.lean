import Init.System.IO
import Std

open Std
open IO.Process

/-- Pipe input into stdin of the spawned process, then return output upon completion. -/
def cmd_with_stdin (args : SpawnArgs) (input : String) : IO Output := do
  let child <- spawn { args with stdin := .piped, stdout := .piped, stderr := .piped }
  let (stdin, child) <- child.takeStdin
  stdin.putStr input
  stdin.flush
  let stdout <- IO.asTask child.stdout.readToEnd Task.Priority.dedicated
  let stderr <- child.stderr.readToEnd
  let exitCode <- child.wait
  let stdout <- IO.ofExcept stdout.get
  return { exitCode, stdout, stderr }

def run_ace (sentence : String) : IO Unit := do
  let res <- cmd_with_stdin {cmd := "ace", args := #["-g","/Users/ar/hpsg/mrs-logic/erg.dat","-Tf","-n 5"], cwd := "."} sentence
  IO.println res.stdout

#eval run_ace "no cat is happy."

def test := do
 let txt ← (IO.FS.readFile "/Users/ar/work/cpdoc/dhbb-nlp/udp/996.conllu")
 let f := List.filter (fun (l : String) => ¬ l.startsWith "#")
 let g := fun (s : String) => s.splitOn "\n"
 let h := List.map (fun s : String => s.splitOn "\t")
 return (List.map (h ∘ f) (List.map g $ txt.splitOn "\n\n"))

#eval test
