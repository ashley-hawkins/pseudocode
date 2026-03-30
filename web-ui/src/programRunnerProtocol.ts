import { requestIdGenerator as createRequestIdGenerator, type RequestId, type RunOptions } from './workerProtocol'

export { type RequestId }
export const requestIdGenerator = createRequestIdGenerator

export interface EnvironmentVariable {
  key: string
  value: string
}

export interface ProgramRunnerState {
  running: boolean
  output: string
  lastLine?: number
  nextLine?: number
  frames: Map<string, any>[]
  envVars: EnvironmentVariable[]
}

interface RequestWithIdBase {
  requestId: RequestId
}

export type ProgramRunnerRequestMessage =
  | ({ type: 'setEnvironment', envVars: EnvironmentVariable[] } & RequestWithIdBase)
  | ({ type: 'loadProgram', source: string, mode: number } & RequestWithIdBase)
  | ({ type: 'stepLine' } & RequestWithIdBase)
  | ({ type: 'continueToNextBreakpoint', breakpoints: number[] } & RequestWithIdBase)
  | ({ type: 'continueWithBreakpointSet', breakpoints: number[] } & RequestWithIdBase)
  | ({ type: 'stepProgram', options: RunOptions, firstStep?: boolean } & RequestWithIdBase)
  | ({ type: 'stop' } & RequestWithIdBase)
  | ({ type: 'queryState' } & RequestWithIdBase)

export type ProgramRunnerRequestPayload = Omit<ProgramRunnerRequestMessage, 'requestId'>

export type ProgramRunnerResponseMessage = RequestWithIdBase & (
  | { type: 'setEnvironmentResult', state: ProgramRunnerState }
  | { type: 'loadProgramResult', state: ProgramRunnerState }
  | { type: 'stepLineResult', state: ProgramRunnerState }
  | { type: 'continueToNextBreakpointResult', state: ProgramRunnerState }
  | { type: 'continueWithBreakpointSetResult', state: ProgramRunnerState }
  | { type: 'stepProgramResult', state: ProgramRunnerState }
  | { type: 'stopResult', state: ProgramRunnerState }
  | { type: 'queryStateResult', state: ProgramRunnerState }
  | { type: 'error', message: string, state: ProgramRunnerState }
)

const requestResponseMap = {
  setEnvironment: 'setEnvironmentResult',
  loadProgram: 'loadProgramResult',
  stepLine: 'stepLineResult',
  continueToNextBreakpoint: 'continueToNextBreakpointResult',
  continueWithBreakpointSet: 'continueWithBreakpointSetResult',
  stepProgram: 'stepProgramResult',
  stop: 'stopResult',
  queryState: 'queryStateResult',
} as const

export type ProgramRunnerResponseForRequest<T extends ProgramRunnerRequestMessage> =
  Extract<ProgramRunnerResponseMessage, { type: (typeof requestResponseMap)[T['type']] }> | Extract<ProgramRunnerResponseMessage, { type: 'error' }>

export type ProgramRunnerResponseForPayload<T extends ProgramRunnerRequestPayload> = ProgramRunnerResponseForRequest<Extract<ProgramRunnerRequestMessage, { type: T['type'] }>>
