# Pseudocode Interpreter

<!-- Web demo is available at https://users.sussex.ac.uk/~ah2084/pseudocode -->

Interpreter for the pseudocode languages used in Programming Concepts module taught by Philip Saville.

Unlike in slides, commented and empty lines do count towards the line numbers used for `goto line` statements.

The line number used in the `goto line` statement will just be the line number shown by any text editor that supports showing line numbers.

There are three modes for the interpreter. `jumpy` for Jumpy Imp programs, `structured` for Structured Imp programs, and `procedural` for Procedural Imp programs. The `procedural` mode supports Recursive Imp programs. The interpreter should detect based on the input file name extension (.ji, .si, .pi, .ri, or the full extensions used in the examples), but if for some reason that fails, you can specify manually either `--mode=jumpy`, `--mode=structured`, or `--mode=procedural`.

Example of how to use it:

`cargo run -- examples/even.jumpyimp n:10`
`cargo run -- examples/even.structuredimp n:10`
`cargo run -- examples/fibonacci.proceduralimp n:8`
`cargo run -- examples/fibonacci.recursiveimp n:8`

# Alternative operators available for hard-to-type symbols

| Standard | Alternative |
| --- | --- |
| ≥ | >= |
| ≤ | <= |
| ≠ | /= |
| × | * |
