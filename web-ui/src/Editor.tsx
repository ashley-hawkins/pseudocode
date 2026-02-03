import { createEffect, createSignal, onMount } from 'solid-js'

import { Mode, ProgramWrapper } from 'pseudocode_js'
import pseudocodeInit from 'pseudocode_js'
import { EditorView, basicSetup } from 'codemirror'
import { gutter, GutterMarker } from '@codemirror/view'
import { emitUserBroadcast, onUserBroadcast, UserBroadcastType, type IGoldenLayoutProps, type UserBroadcastData } from './types'

await pseudocodeInit();

import { StateField, StateEffect, RangeSet, EditorState, Transaction, Range } from "@codemirror/state"
import type { JsonValue } from 'golden-layout'

const nextExecutionMarker = new class extends GutterMarker {
  elementClass: string = "cm-next-execution-marker"
  toDOM() { return document.createTextNode("▶") }
}
const lastExecutionMarker = new class extends GutterMarker {
  elementClass: string = "cm-last-execution-marker"
  toDOM() { return document.createTextNode("►") }
}

const executionEffect = StateEffect.define<{ lastLinePos?: number; nextLinePos?: number }>({
  map: (val, mapping) => ({
    lastLinePos: val.lastLinePos ? mapping.mapPos(val.lastLinePos) : undefined,
    nextLinePos: val.nextLinePos ? mapping.mapPos(val.nextLinePos) : undefined,
  })
})

const executionState = StateField.define<{
  lastLinePos?: number;
  nextLinePos?: number;
}>({
  create: function (state: EditorState): { lastLinePos?: number; nextLinePos?: number } {
    return {}
  },
  update: function (value: { lastLinePos?: number; nextLinePos?: number }, transaction: Transaction): { lastLinePos?: number; nextLinePos?: number } {
    const mapped = {
      lastLinePos: value.lastLinePos !== undefined ? transaction.changes.mapPos(value.lastLinePos) : undefined,
      nextLinePos: value.nextLinePos !== undefined ? transaction.changes.mapPos(value.nextLinePos) : undefined,
    }

    for (let e of transaction.effects) {
      if (e.is(executionEffect)) {
        return {
          lastLinePos: e.value.lastLinePos,
          nextLinePos: e.value.nextLinePos,
        }
      }
    }

    return mapped
  }
})

function setExecutionState(view: EditorView, lastLine?: number, nextLine?: number) {
  view.dispatch({
    effects: executionEffect.of({
      lastLinePos: lastLine !== undefined ? view.state.doc.line(lastLine + 1).from : undefined,
      nextLinePos: nextLine !== undefined ? view.state.doc.line(nextLine + 1).from : undefined
    })
  })
}

const executionStateGutter = [
  executionState,
  gutter({
    class: "cm-execution-gutter",
    markers: (view) => {
      const execState = view.state.field(executionState)
      const markers: Range<GutterMarker>[] = []
      if (execState.lastLinePos !== undefined) {
        markers.push(lastExecutionMarker.range(execState.lastLinePos))
      }
      if (execState.nextLinePos !== undefined) {
        markers.push(nextExecutionMarker.range(execState.nextLinePos))
      }
      return RangeSet.empty.update({ add: markers })
    },
    initialSpacer: () => nextExecutionMarker,
  }),
  EditorView.baseTheme({
    ".cm-execution-gutter .cm-gutterElement": {
      paddingLeft: "5px",
      cursor: "default"
    },
    ".cm-next-execution-marker": {
      color: "orange",
    },
    ".cm-last-execution-marker": {
      color: "green",
    }
  })
]

const breakpointMarker = new class extends GutterMarker {
  toDOM() { return document.createTextNode("●") }
}

const breakpointEffect = StateEffect.define<{ pos: number, on: boolean }>({
  map: (val, mapping) => ({ pos: mapping.mapPos(val.pos), on: val.on })
})

const breakpointState = StateField.define<RangeSet<GutterMarker>>({
  create() { return RangeSet.empty },
  update(set, transaction) {
    set = set.map(transaction.changes)
    for (let e of transaction.effects) {
      if (e.is(breakpointEffect)) {
        if (e.value.on)
          set = set.update({ add: [breakpointMarker.range(e.value.pos)] })
        else
          set = set.update({ filter: from => from != e.value.pos })
      }
    }
    return set
  }
})

function toggleBreakpoint(view: EditorView, pos: number) {
  let breakpoints = view.state.field(breakpointState)
  let hasBreakpoint = false
  breakpoints.between(pos, pos, () => { hasBreakpoint = true })
  view.dispatch({
    effects: breakpointEffect.of({ pos, on: !hasBreakpoint })
  })
}

const breakpointGutter = [
  breakpointState,
  gutter({
    class: "cm-breakpoint-gutter",
    markers: v => v.state.field(breakpointState),
    initialSpacer: () => breakpointMarker,
    domEventHandlers: {
      mousedown(view, line) {
        toggleBreakpoint(view, line.from)
        return true
      }
    }
  }),
  EditorView.baseTheme({
    ".cm-breakpoint-gutter .cm-gutterElement": {
      color: "red",
      paddingLeft: "5px",
      cursor: "default"
    }
  })
]

export interface ProgramEditorState {
  baseState: JsonValue;
  breakpoints: number[];
}

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

  let newState = undefined;

  const state = (props.glContainer.state as ProgramEditorState | undefined);

  if (state !== undefined && state.baseState) {
    newState = EditorState.fromJSON(state.baseState);
  }

  onMount(() => {
    editor = new EditorView({
      parent: codeDiv!,
      doc: newState?.doc,
      selection: newState?.selection,
      extensions: [
        executionStateGutter,
        breakpointGutter,
        basicSetup,
      ],
    })

    for (let bp of state?.breakpoints || []) {
      const line = editor.state.doc.line(bp + 1)
      toggleBreakpoint(editor, line.from)
    }

    props.glContainer.stateRequestEvent = () => {
      const serializedState = {
        baseState: editor.state.toJSON(),
        breakpoints: (() => {
          let bps: Set<number> = new Set()
          editor.state.field(breakpointState).between(0, editor.state.doc.length, (from, to) => {
            const line = editor.state.doc.lineAt(from)
            bps.add(line.number - 1)
          })
          return Array.from(bps)
        })()
      }
      return serializedState;
    }
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
      const lineQueryResult = wrapper.query_source_lines()
      setExecutionState(
        editor,
        lineQueryResult.last_line,
        lineQueryResult.next_line
      )
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

  let continueProgramToNextBreakpoint = () => {
    forceStop = false
    let breakpoints = new Set<number>()
    editor.state.field(breakpointState).between(0, editor.state.doc.length, (from, to) => {
      const line = editor.state.doc.lineAt(from)
      breakpoints.add(line.number - 1)
    })
    continueWithBreakpointSet(breakpoints)
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
      continueProgramToNextBreakpoint()
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
