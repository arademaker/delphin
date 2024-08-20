
# Lean DELPH-IN

The long-term goal is to have a complete port of
https://pydelphin.readthedocs.io/ to Lean and even more tools from the
https://github.com/delph-in/docs/wiki. The short-term goal is to port
enough of PyDelphin to allow the port of
http://github.com/ibm/MRS-Logic to Lean.

## Team

- Alexandre Rademaker
- Guilherme Lima

## Setup

You need to have [Ace](http://sweaglesw.org/linguistics/ace/) parser
installed and in the PATH to be executed by the Lean code. 

You need to have the [English Resource
Grammar](https://github.com/delph-in/erg/) compiled with Ace
(e.g. `erg.dat`) available. For now, you will need to manually adjust
the location of this file in the `run_ace` function definition.

You need to obtain the last release of Utool and save the `utool.jar`
file in the root folder of this repo if you want to perform
quantifiers scope resolution. Utool 3.4 can be downloaded from
[here](https://github.com/coli-saar/utool/releases/tag/utool-3.4).

