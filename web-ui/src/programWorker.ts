import pseudocodeInit, { ProgramJs, Mode, ValueJs, StepResultJs, LineQueryResult } from 'pseudocode_js'
import { RunMode, type CurrentFramesRequestMessage, type CurrentFramesResponseMessage, type LoadRequestMessage, type LoadResponseMessage, type OutputRequestMessage, type OutputResponseMessage, type QueryLinesRequestMessage, type QueryLinesResponseMessage, type RequestMessage, type ResetRequestMessage, type ResetResponseMessage, type ResponseForRequestMessage, type RunRequestMessage, type RunResponseMessage, type StepRequestMessage, type StepResponseMessage } from './workerProtocol'
import { valueToString } from './util';

await pseudocodeInit();

function yieldToEventLoop() {
  const channel = new MessageChannel();
  channel.port2.start();
  return new Promise(resolve => {
    channel.port1.onmessage = resolve;
    channel.port2.postMessage(0);
  });
}

const yieldIntervalMs = 200 as const

const program = new ProgramJs()
class MessageQueue<T> {
  private queue: T[] = []
  private waiting: [(value: T | PromiseLike<T>) => void, (reason?: any) => void][] = []

  push(msg: T) {
    if (this.waiting.length != 0) {
      const [resolve, _reject] = this.waiting.shift()!
      resolve(msg)
    } else {
      this.queue.push(msg)
    }
  }

  async pop(): Promise<T> {
    if (this.queue.length != 0) {
      return Promise.resolve(this.queue.shift()!)
    } else {
      return new Promise<T>((resolve, reject) => {
        this.waiting.push([resolve, reject])
      })
    }
  }

  tryPop(): T | undefined {
    if (this.queue.length != 0) {
      return this.queue.shift()
    } else {
      return undefined
    }
  }

  popAll(): T[] {
    const q = this.queue.slice()
    this.queue.length = 0
    return q
  }

  async yieldPopAll(): Promise<T[]> {
    await yieldToEventLoop()
    return this.popAll()
  }
}

let msgQueue = new MessageQueue<RequestMessage>()
let stopRequested = false
let running = false

onmessage = (e: MessageEvent<RequestMessage>) => {
  const data = e.data
  msgQueue.push(data)
}

async function processExistingMessages() {
  const messages = await msgQueue.yieldPopAll()
  for (const m of messages) {
    await handleMessage(m)
  }
}

function mapFramesToJs(frames: Array<Map<string, ValueJs>>): Array<Map<string, any>> {
  return frames.map(x => (new Map([...x.entries()].map(([k, v]) => [k, v.toJs()]))))
}

function load(m: LoadRequestMessage): LoadResponseMessage {
  const ok = program.loadSource(m.source ?? '', m.mode ?? Mode.Structured)
  const out = program.output() || ''
  return { type: 'loadResult', requestId: m.requestId, ok, output: out }
}

function reset(m: ResetRequestMessage): ResetResponseMessage {
  program.resetStateWithEnvironment(m.env ?? [])
  return { type: 'resetResult', requestId: m.requestId } satisfies ResponseForRequestMessage<typeof m>
}

function queryLines(m: QueryLinesRequestMessage): QueryLinesResponseMessage {
  const q = program.querySourceLines()
  return { type: 'queryResult', requestId: m.requestId, lastLine: q.lastLine, nextLine: q.nextLine, atLineBoundary: q.atLineBoundary() } satisfies ResponseForRequestMessage<typeof m>
}

function step(m: StepRequestMessage): StepResponseMessage {
  const stepResult = program.step()
  const cont = stepResult.shallContinue()
  const q = program.querySourceLines()
  const out = program.output() || ''
  const frames = mapFramesToJs(program.currentFrames())
  return { type: 'stepResult', requestId: m.requestId, cont, return_value: stepResult.returnValue()?.toJs(), lastLine: q.lastLine, nextLine: q.nextLine, atLineBoundary: q.atLineBoundary(), output: out, frames } satisfies ResponseForRequestMessage<typeof m>
}

function currentFrames(m: CurrentFramesRequestMessage): CurrentFramesResponseMessage {
  const frames = mapFramesToJs(program.currentFrames())
  return { type: 'currentFramesResult', requestId: m.requestId, frames } satisfies ResponseForRequestMessage<typeof m>
}

function output(m: OutputRequestMessage): OutputResponseMessage {
  const out = program.output() || ''
  return { type: 'outputResult', requestId: m.requestId, output: out } satisfies ResponseForRequestMessage<typeof m>
}

async function run(m: RunRequestMessage): Promise<RunResponseMessage> {
  if (running) {
    return { type: 'error', requestId: m.requestId, message: 'Program is already running' } satisfies ResponseForRequestMessage<typeof m>
  }
  running = true
  stopRequested = false
  let lastLine: number | undefined = undefined
  let nextLine: number | undefined = undefined

  const runMode = m.options.type
  const firstStep = m.firstStep ?? false

  const finishedStepping = (queryResult: LineQueryResult): boolean => {
    const next = queryResult.nextLine
    if (next === undefined) {
      lastLine = nextLine
      nextLine = undefined
      return true
    }

    if (!queryResult.atLineBoundary()) {
      return false
    }

    if (runMode === RunMode.LineBasedBreakpoint) {
      return m.options.breakpoints.has(next)
    }
    if (runMode === RunMode.NextLine) {
      return true
    }
    return false
  }

  const initialQuery = program.querySourceLines()
  nextLine = initialQuery.nextLine
  lastLine = initialQuery.lastLine

  let shouldContinue = true

  if (firstStep) {
    shouldContinue = !finishedStepping(initialQuery)
  }

  if (initialQuery.nextLine === undefined) {
    shouldContinue = false;
  }

  let lastYieldTime = Date.now()
  let lastStepResult: StepResultJs | undefined;
  while (shouldContinue) {
    lastStepResult = program.step()
    shouldContinue = lastStepResult.shallContinue()

    const q = program.querySourceLines()
    if (q.atLineBoundary()) {
      lastLine = q.lastLine
      nextLine = q.nextLine
    }

    if (finishedStepping(q)) {
      break
    }

    if (Date.now() - lastYieldTime > yieldIntervalMs) {
      await processExistingMessages()
      lastYieldTime = Date.now()
    }

    if (stopRequested) {
      break
    }
    if (!shouldContinue) break
  }

  let out = program.output() || ''

  if (lastStepResult !== undefined) {
    const returnValue = lastStepResult.returnValue()
    if (returnValue !== undefined) {
      if (out) out += '\n'
      out += `Program returned: ${valueToString(returnValue.toJs())}`
    }
  }


  const frames = mapFramesToJs(program.currentFrames())
  running = false
  return { type: 'runResult', requestId: m.requestId, output: out, lastLine, nextLine, frames }
}

function shouldDefer(m: RequestMessage) {
  return (m.type === 'load' || m.type === 'reset' || m.type === 'step' || m.type === 'run') && running
}

async function handleMessage(m: RequestMessage) {
  if (shouldDefer(m)) {
    msgQueue.push(m)
    return;
  }

  switch (m.type) {
    case 'load': {
      postMessage(load(m) satisfies ResponseForRequestMessage<typeof m>)
      break
    }
    case 'reset': {
      postMessage(reset(m) satisfies ResponseForRequestMessage<typeof m>)
      break
    }
    case 'queryLines': {
      postMessage(queryLines(m) satisfies ResponseForRequestMessage<typeof m>)
      break
    }
    case 'step': {
      postMessage(step(m) satisfies ResponseForRequestMessage<typeof m>)
      break
    }
    case 'currentFrames': {
      postMessage(currentFrames(m) satisfies ResponseForRequestMessage<typeof m>)
      break
    }
    case 'output': {
      postMessage(output(m) satisfies ResponseForRequestMessage<typeof m>)
      break
    }
    case 'run': {
      postMessage(await run(m) satisfies ResponseForRequestMessage<typeof m>)
      break
    }
    case 'stop': {
      stopRequested = true
      break
    }
  }
}

async function mainLoop() {
  while (true) {
    const msg = await msgQueue.pop()
    await handleMessage(msg)
  }
}

mainLoop()
