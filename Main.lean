import Init.System.IO
import Std
import «Mrs»

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
  let res <- cmd_with_stdin {cmd := "ace", args := #["-g","/Users/ar/r/erg.dat","-T","-n 1"], cwd := "."} sentence
  IO.println res.stdout

#eval run_ace "The cat, I buy."


def test := "[ LTOP: h1
  INDEX: e2 [ e SF: PROP TENSE: PRES MOOD: INDICATIVE PROG: - PERF: - ]
  RELS: < [ _the_q_rel<0:3> LBL: h3 ARG0: x5 [ x PERS: 3 NUM: SG IND: + ] RSTR: h6 BODY: h4 ]
          [ \"_road_n_1_rel\"<4:8> LBL: h7 ARG0: x5 ]
          [ _rise_v_1<9:14> LBL: h8 ARG0: e2 ARG1: x5 ]
          [ _from_p_dir_rel<15:19> LBL: h8 ARG0: e9 [ e SF: PROP TENSE: UNTENSED MOOD: INDICATIVE PROG: - PERF: - ] ARG1: e2 ARG2: x10 [ x PERS: 3 NUM: SG ] ]
          [ place_n_rel<20:26> LBL: h11 ARG0: x10 ]
          [ def_implicit_q_rel<20:26> LBL: h12 ARG0: x10 RSTR: h13 BODY: h14 ]
          [ _there_a_1_rel<20:26> LBL: h11 ARG0: e15 [ e SF: PROP TENSE: UNTENSED MOOD: INDICATIVE PROG: - PERF: - ] ARG1: x10 ] >
  HCONS: < h6 qeq h7 h13 qeq h11 > ]"


#eval parseMRS test.mkIterator
