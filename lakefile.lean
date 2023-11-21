import Lake
open Lake DSL

package delphin {
  -- add configuration options here
}

require std from git
  "https://github.com/leanprover/std4/"@"v4.2.0"

require Megaparsec from git
  "https://github.com/lurk-lab/Megaparsec.lean"@"main"
