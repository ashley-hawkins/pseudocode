import { createEffect, createSignal, onMount } from 'solid-js'

import { Mode, ProgramWrapper } from 'pseudocode_js'
import pseudocodeInit from 'pseudocode_js'
import { EditorView, basicSetup } from 'codemirror'
import { gutter, GutterMarker } from '@codemirror/view'
import { emitUserBroadcast, onUserBroadcast, UserBroadcastType, type IGoldenLayoutProps, type UserBroadcastData } from './types'

await pseudocodeInit();

export default function ProgramEditor(props: IGoldenLayoutProps) {
  const [parserOutput, setParserOutput] = createSignal<string>("")
  let envVars: { key: string, value: string }[] = []

  createEffect(() => {
    emitUserBroadcast(props.glContainer.layoutManager.eventHub, { type: UserBroadcastType.parserOutput, output: parserOutput() });
  })

  const wrapper = new ProgramWrapper()

  onUserBroadcast(props.glContainer.layoutManager.eventHub, (broadcastData: UserBroadcastData) => {
    if (broadcastData.type === UserBroadcastType.envVarsUpdate) {
      envVars = broadcastData.envVars
    }
  })

  let forceStop = false

  let editor: EditorView
  let codeDiv

  onMount(() => {
    class BreakpointMarker extends GutterMarker {
      activated: boolean = false
      toDOM() {
        return document.createTextNode(this.activated ? '●' : '🡆')
      }
    }

    const breakpointGutter = gutter({
      class: 'cm-gutter cm-breakpoints',
      renderEmptyElements: true,
      domEventHandlers: {
        click: (view, line, event) => {
          const lineNumber = view.state.doc.lineAt(line.from).number
          console.log('Gutter clicked on line', lineNumber)
          return true
        }
      },
      lineMarker: (line) => {
        return new BreakpointMarker()
      }
    })

    editor = new EditorView({
      parent: codeDiv!,
      doc: `// Write your pseudocode here\nDEBUGLN "Hello world!"\n`,
      extensions: [
        breakpointGutter,
        basicSetup,
      ],
    })
  })

  let selectedMode: Mode = Mode.Structured

  const enum StepType {
    LineBasedBreakpoint,
    NextLine,
    ToCompletion
  }

  interface StepBreakpointOptions {
    type: StepType.LineBasedBreakpoint,
    breakpoints: Set<number>
  }

  interface StepNextLineOptions {
    type: StepType.NextLine
  }

  type StepOptions = StepBreakpointOptions | StepNextLineOptions

  let stepProgram = async (stepOptions: StepOptions) => {
    let shouldStop = () => {
      const lineQueryResult = wrapper.query_source_lines()

      const last = lineQueryResult.last_line
      const next = lineQueryResult.next_line

      // Program has halted
      if (next === undefined) {
        return true
      }

      // This is not a transition between lines
      if (next === last) {
        return false
      }

      if (stepOptions.type === StepType.LineBasedBreakpoint) {
        // About to hit a breakpoint
        return stepOptions.breakpoints.has(next)
      } else if (stepOptions.type === StepType.NextLine) {
        // About to transition to the next line
        return true
      }
    }

    const updateVisuals = () => {
      setParserOutput(wrapper.output() || 'No runtime output.')
    }

    let counter = 0

    while (true) {
      if (shouldStop()) {
        break
      }
      if (forceStop) {
        forceStop = false
        break
      }

      wrapper.step()

      if (counter > 1000) {
        updateVisuals()
        counter = 0
      }
      // Yield every 1000 steps to make sure the UI isn't blocked.
      await new Promise((resolve) => setTimeout(resolve, 0))
      counter += 1
    }

    updateVisuals()
  }

  let continueWithBreakpointSet = (breakpoints: Set<number>) => {
    stepProgram({ type: StepType.LineBasedBreakpoint, breakpoints })
  }

  let continueProgramToCompletion = () => {
    continueWithBreakpointSet(new Set)
  }

  let stepLine = () => {
    stepProgram({ type: StepType.NextLine })
  }

  let runProgram = () => {
    const src = editor?.state?.doc.toString() ?? ''
    if (wrapper.load_source(src, selectedMode)) {
      wrapper.reset_state_with_environment(envVars.map((ev) => `${ev.key}:${ev.value}`))
      continueProgramToCompletion()
    } else {
      setParserOutput(wrapper.output() || 'No parser output.')
    }
  }

  return (
    <>
      <div class="h-full flex flex-1 flex-row bg-neutral-400 gap-1 p-1">
        <div class="flex flex-1 flex-col gap-1">
          <div class="flex flex-row">
            <select onChange={
              (e) => {
                const value = (e.target as HTMLSelectElement).value
                selectedMode = parseInt(value) as Mode
              }
            } class="select">
              <option value={Mode.Jumpy}>Jumpy Imp</option>
              <option selected value={Mode.Structured}>Structured Imp</option>
              <option value={Mode.Procedural}>Procedural/Recursive Imp</option>
            </select>
            <button
              class="btn flex-1"
              onClick={runProgram}
            >
              Run
            </button>
            <button class="btn flex-1" onClick={stepLine}>Step</button>
            <button class="btn flex-1" onClick={() => { forceStop = true }}>Stop</button>
          </div>
          <div ref={codeDiv} class="flex flex-1 bg-base-200 rounded-md border-base-300"></div>
        </div>
      </div>
    </>
  )
}
