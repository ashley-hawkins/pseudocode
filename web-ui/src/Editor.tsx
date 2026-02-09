import { createEffect, createSignal, onMount } from 'solid-js'

import { Mode } from 'pseudocode_js'
import ProgramWorkerClient from './programWorkerClient'
import { EditorView, basicSetup } from 'codemirror'
import { gutter, GutterMarker } from '@codemirror/view'
import { emitUserBroadcast, onUserBroadcast, UserBroadcastType, type IGoldenLayoutProps, type UserBroadcastData } from './types'

import { StateField, StateEffect, RangeSet, EditorState, Transaction, Range } from "@codemirror/state"
import type { JsonValue } from 'golden-layout'
import { RunMode, type RunOptions } from './workerProtocol'

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
  create: function (_state: EditorState): { lastLinePos?: number; nextLinePos?: number } {
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
      return RangeSet.empty.update({ add: markers.sort((a, b) => a.from - b.from) })
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
  const [frameStack, setFrameStack] = createSignal<Map<string, any>[]>([])

  let envVars: { key: string, value: string }[] = []

  createEffect(() => {
    emitUserBroadcast(props.glContainer.layoutManager.eventHub, { type: UserBroadcastType.parserOutput, output: parserOutput() });
  })

  createEffect(() => {
    emitUserBroadcast(props.glContainer.layoutManager.eventHub, { type: UserBroadcastType.frameStackUpdate, frameStack: frameStack() });
  })

  let workerClient: ProgramWorkerClient = new ProgramWorkerClient()

  onUserBroadcast(props.glContainer.layoutManager.eventHub, (broadcastData: UserBroadcastData) => {
    if (broadcastData.type === UserBroadcastType.envVarsUpdate) {
      envVars = broadcastData.envVars
    }
  })

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
          editor.state.field(breakpointState).between(0, editor.state.doc.length, (from, _to) => {
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

  const [currentlyRunning, setCurrentlyRunning] = createSignal<boolean>(false);

  let stepProgram = async (runOptions: RunOptions, firstStep: boolean = false) => {
    if (currentlyRunning()) {
      return
    }
    setCurrentlyRunning(true);
    let previousLine: number | undefined = undefined
    let nextLine: number | undefined = undefined

    const runResult = await workerClient.run(runOptions, firstStep)

    if (runResult.type === 'error') {
      setParserOutput(`Error during execution: ${runResult.message}`)
      setCurrentlyRunning(false)
      return
    }

    previousLine = runResult.lastLine
    nextLine = runResult.nextLine

    setParserOutput(runResult.output || 'No runtime output.')
    setExecutionState(editor, previousLine, nextLine)
    setFrameStack(runResult.frames)
    setCurrentlyRunning(false)
  }

  let continueWithBreakpointSet = (breakpoints: Set<number>, firstStep: boolean = false) => {
    stepProgram({ type: RunMode.LineBasedBreakpoint, breakpoints }, firstStep)
  }

  let continueProgramToNextBreakpoint = (firstStep: boolean = false) => {
    let breakpoints = new Set<number>()
    editor.state.field(breakpointState).between(0, editor.state.doc.length, (from, _to) => {
      const line = editor.state.doc.lineAt(from)
      breakpoints.add(line.number - 1)
    })
    continueWithBreakpointSet(breakpoints, firstStep)
  }

  let stepLine = () => {
    stepProgram({ type: RunMode.NextLine })
  }

  let runProgram = async () => {
    const src = editor?.state?.doc.toString() ?? ''
    const loadRes = await workerClient.loadSource(src, selectedMode)
    if (loadRes.type != 'error' && loadRes.ok) {
      await workerClient.resetWithEnv(envVars.map((ev) => `${ev.key}:${ev.value}`))
      continueProgramToNextBreakpoint(true)
    } else {
      if (loadRes.type === 'error') {
        setParserOutput(`Error loading program: ${loadRes.message}`)
      } else {
        setParserOutput(loadRes.output || 'No parser output.')
      }
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
            {
              currentlyRunning() ? (
                <button class="btn flex-1" onClick={() => workerClient.stop()}>Stop</button>
              ) : (
                <button class="btn flex-1" onClick={runProgram}>Run</button>
              )
            }
            <button class="btn flex-1" onClick={() => continueProgramToNextBreakpoint()}>Continue</button>
            <button class="btn flex-1" onClick={stepLine}>Step</button>
          </div>
          <div ref={codeDiv} class="flex flex-1 bg-base-200 rounded-md border-base-300"></div>
        </div>
      </div>
    </>
  )
}
