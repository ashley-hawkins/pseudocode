# To Do

### Web UI

#### Misc

- [ ] Move interpreter into web worker
- [ ] Decouple program running from editor component
- [ ] Make the frames component better. Make each frame be displayed more distinctly.
- [ ] Fix x-scrolling on the terminal-like output panel
- [ ] Persistent state across reloads
- [ ] Save current editor state, per language
- [ ] Allow naming and saving a program to local storage, and re-loading it
- [ ] Provide sample programs for each language (maybe incorporated into the same system that is used to load saved user-written programs from local storage)

#### Debugger
- [ ] Figure out how to avoid implicit returns behaving weirdly
- [ ] Figure out why breakpoints set on the first line of the program are not triggered (for procedural, the first line of the program is the first statement in Algorithm: block)
