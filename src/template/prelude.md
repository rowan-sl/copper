# Mlog prelude

the mlog prelude is handwritten mlog inserted at the beinging of the prelude including code used in the program for things

## Current features

requires that `switch1` is connected to a reset switch (starts the program after a reset is triggered)
requires that `message1` is connected (for status messages)
requires that `bank1` is connected (for the stack and other things)
`jump 26 always 0 0` to wait for the reset signal (no status message printed) (end inst after reset is reached)

- this line number is mentioned in: `prelude.md`, `prelude.mlog`, `codegen/mod.rs`, and `cleanup.mlog`. CHANGE IT THERE IF IT CHANGES HERE
