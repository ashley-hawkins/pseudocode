import pseudocodeInit, { ProgramJs, Mode, ValueJs, StepResultJs } from 'pseudocode_js'
import { RunMode, type RequestMessage, type ResponseForRequestMessage, type ResponseMessage } from './workerProtocol'
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

async function run(m: Extract<RequestMessage, { type: 'run' }>) {
  if (running) {
    const res: ResponseMessage = { type: 'error', requestId: m.requestId, message: 'Program is already running' } satisfies ResponseForRequestMessage<typeof m>
    postMessage(res)
    return;
  }
  running = true
  stopRequested = false
  let lastLine: number | undefined = undefined
  let nextLine: number | undefined = undefined

  const runMode = m.options.type
  const firstStep = m.firstStep ?? false

  const finishedStepping = (queryResult: { next_line?: number; last_line?: number; at_line_boundary: () => boolean }): boolean => {
    const next = queryResult.next_line
    if (next === undefined) {
      lastLine = nextLine
      nextLine = undefined
      return true
    }

    if (!queryResult.at_line_boundary()) {
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
  nextLine = initialQuery.next_line
  lastLine = initialQuery.last_line

  let shouldContinue = true

  if (firstStep) {
    shouldContinue = !finishedStepping(initialQuery)
  }

  if (initialQuery.next_line === undefined) {
    shouldContinue = false;
  }

  let lastYieldTime = Date.now()
  let lastStepResult: StepResultJs | undefined;
  while (shouldContinue) {
    lastStepResult = program.step()
    shouldContinue = lastStepResult.shallContinue()

    const q = program.querySourceLines()
    if (q.at_line_boundary()) {
      lastLine = q.last_line
      nextLine = q.next_line
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
  const res: ResponseMessage = { type: 'runResult', requestId: m.requestId, output: out, lastLine, nextLine, frames }
  postMessage(res)
  running = false
}

async function handleMessage(m: RequestMessage) {
  switch (m.type) {
    case 'load': {
      if (running) {
        msgQueue.push(m)
        return;
      }
      const ok = program.loadSource(m.source ?? '', m.mode ?? Mode.Structured)
      const out = program.output() || ''
      const res = { type: 'loadResult', requestId: m.requestId, ok, output: out } satisfies ResponseForRequestMessage<typeof m>
      postMessage(res)
      break
    }
    case 'reset': {
      if (running) {
        msgQueue.push(m)
        return;
      }
      program.resetStateWithEnvironment(m.env ?? [])
      const res: ResponseMessage = { type: 'resetResult', requestId: m.requestId } satisfies ResponseForRequestMessage<typeof m>
      postMessage(res)
      break
    }
    case 'queryLines': {
      const q = program.querySourceLines()
      const res: ResponseMessage = { type: 'queryResult', requestId: m.requestId, last_line: q.last_line, next_line: q.next_line, at_line_boundary: q.at_line_boundary() } satisfies ResponseForRequestMessage<typeof m>
      postMessage(res)
      break
    }
    case 'step': {
      if (running) {
        msgQueue.push(m)
        return;
      }
      const stepResult = program.step()
      const cont = stepResult.shallContinue()
      const q = program.querySourceLines()
      const out = program.output() || ''
      const frames = mapFramesToJs(program.currentFrames())
      const res: ResponseMessage = { type: 'stepResult', requestId: m.requestId, cont, return_value: stepResult.returnValue()?.toJs(), last_line: q.last_line, next_line: q.next_line, at_line_boundary: q.at_line_boundary(), output: out, frames } satisfies ResponseForRequestMessage<typeof m>
      postMessage(res)
      break
    }
    case 'currentFrames': {
      const frames = mapFramesToJs(program.currentFrames())
      const res: ResponseMessage = { type: 'currentFramesResult', requestId: m.requestId, frames } satisfies ResponseForRequestMessage<typeof m>
      postMessage(res)
      break
    }
    case 'output': {
      const out = program.output() || ''
      const res: ResponseMessage = { type: 'outputResult', requestId: m.requestId, output: out } satisfies ResponseForRequestMessage<typeof m>
      postMessage(res)
      break
    }
    case 'run': {
      await run(m)
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
