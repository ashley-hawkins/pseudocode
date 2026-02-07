# To Do

### Web UI

#### Misc

- [ ] Move interpreter into a web worker so it doesn't need to yield every 10k instructions
- [ ] Completely program running from the editor component - editor should only be for the input code, the breakpoints, and the current line display
- [ ] Make the frames component better. Make each frame be displayed more distinctly, probably with a border.
- [ ] Fix x-scrolling on the terminal-like output panel, rather than hiding overflow.
- [ ] Save state for the environment editor (will persist)
- [ ] Save state for the output panel (not to persist)
- [ ] Save state for the frame stack display (not to persist)
- [ ] Persistent layout state across reloads (local storage or something)
- [ ] Save current editor state, per language
- [ ] Allow naming and saving a program to local storage, and re-loading it
- [ ] Provide sample programs for each language (maybe incorporated into the same system that is used to load saved user-written programs from local storage)

#### Debugger
- [ ] Figure out how to avoid implicit returns behaving weirdly
- [ ] Figure out why breakpoints set on the first line of the program are not triggered (for procedural, the first line of the program is the first statement in Algorithm: block)

### Interpreter
- [ ] Fully serialize and deserialize interpreter state to JSON... Need to be able to create shared values in the same way as they were shared in the original state somehow (for arrays).
