export type RequestId = number
export function* requestIdGenerator() {
  let id = 1;
  while (true) {
    yield id++
  }
}

export const enum RunMode {
  LineBasedBreakpoint = 'lineBreakpoint',
  NextLine = 'nextLine'
}

export interface RunToBreakpointOptions {
  type: RunMode.LineBasedBreakpoint,
  breakpoints: Set<number>
}

export interface RunToNextLineOptions {
  type: RunMode.NextLine
}

export type RunOptions = RunToBreakpointOptions | RunToNextLineOptions

interface RequestWithIdBase {
  requestId: RequestId
}

export type RequestMessage =
  | { type: 'load', source: string, mode: number } & RequestWithIdBase
  | { type: 'reset', env: string[] } & RequestWithIdBase
  | { type: 'queryLines' } & RequestWithIdBase
  | { type: 'step' } & RequestWithIdBase
  | { type: 'currentFrames' } & RequestWithIdBase
  | { type: 'output' } & RequestWithIdBase
  | { type: 'run', options: RunOptions, firstStep: boolean } & RequestWithIdBase
  | { type: 'stop' }

export type RequestWithId = Extract<RequestMessage, RequestWithIdBase>
export type RequestWithoutId = Exclude<RequestMessage, RequestWithIdBase>

export type RequestPayload = RequestWithId extends infer R
  ? R extends RequestWithId
  ? Omit<R, 'requestId'>
  : never
  : never

export function isRequestWithId(message: RequestMessage): message is RequestWithId {
  return 'requestId' in message
}

export type ResponseMessage = RequestWithIdBase & (
  | { type: 'loadResult', ok: boolean, output: string }
  | { type: 'resetResult' }
  | { type: 'queryResult', last_line?: number, next_line?: number, at_line_boundary: boolean }
  | { type: 'stepResult', cont: boolean, return_value: NonNullable<any> | undefined, last_line?: number, next_line?: number, at_line_boundary: boolean, output: string, frames: any[] }
  | { type: 'currentFramesResult', frames: any[] }
  | { type: 'outputResult', output: string }
  | { type: 'runResult', output: string, lastLine?: number, nextLine?: number, frames: any[] }
  | { type: 'error', message: string })

const requestResponseMap = {
  'load': 'loadResult',
  'reset': 'resetResult',
  'queryLines': 'queryResult',
  'step': 'stepResult',
  'currentFrames': 'currentFramesResult',
  'output': 'outputResult',
  'run': 'runResult'
} as const

export type ResponseForRequestMessage<T extends RequestMessage> = T extends RequestWithId ? Extract<ResponseMessage, { type: typeof requestResponseMap[T['type']] }> | Extract<ResponseMessage, { type: 'error' }> : never
export type ResponseForRequestPayload<T extends RequestPayload> = ResponseForRequestMessage<Extract<RequestMessage, { type: T['type'] }>>

export type LoadRequestMessage = Extract<RequestMessage, { type: 'load' }>
export type LoadResponseMessage = ResponseForRequestMessage<LoadRequestMessage>

export type ResetRequestMessage = Extract<RequestMessage, { type: 'reset' }>
export type ResetResponseMessage = ResponseForRequestMessage<ResetRequestMessage>

export type QueryLinesRequestMessage = Extract<RequestMessage, { type: 'queryLines' }>
export type QueryLinesResponseMessage = ResponseForRequestMessage<QueryLinesRequestMessage>

export type StepRequestMessage = Extract<RequestMessage, { type: 'step' }>
export type StepResponseMessage = ResponseForRequestMessage<StepRequestMessage>

export type CurrentFramesRequestMessage = Extract<RequestMessage, { type: 'currentFrames' }>
export type CurrentFramesResponseMessage = ResponseForRequestMessage<CurrentFramesRequestMessage>

export type OutputRequestMessage = Extract<RequestMessage, { type: 'output' }>
export type OutputResponseMessage = ResponseForRequestMessage<OutputRequestMessage>

export type RunRequestMessage = Extract<RequestMessage, { type: 'run' }>
export type RunResponseMessage = ResponseForRequestMessage<RunRequestMessage>

export type StopRequestMessage = Extract<RequestMessage, { type: 'stop' }>
