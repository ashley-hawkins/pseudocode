import type { EventHub } from 'golden-layout'
import {
  type EnvironmentVariable,
  requestIdGenerator,
  type ProgramRunnerRequestMessage,
  type ProgramRunnerRequestPayload,
  type ProgramRunnerResponseForPayload,
  type ProgramRunnerResponseMessage,
  type RequestId,
  type ProgramRunnerState,
} from './programRunnerProtocol'
import { RunMode, type RunOptions } from './workerProtocol'
import { emitUserBroadcast, onUserBroadcast, UserBroadcastType, type UserBroadcastData } from './types'

export default class ProgramRunnerClient {
  private readonly eventHub: EventHub
  private readonly requestIdGenerator = requestIdGenerator()
  private readonly resolvers = new Map<RequestId, (value: ProgramRunnerResponseMessage) => void>()
  private readonly rejectors = new Map<RequestId, (error: Error) => void>()

  constructor(eventHub: EventHub) {
    this.eventHub = eventHub
    onUserBroadcast(this.eventHub, this.handleBroadcast)
  }

  private readonly handleBroadcast = (broadcastData: UserBroadcastData) => {
    if (broadcastData.type !== UserBroadcastType.programRunnerResponse) {
      return
    }
    this.handleResponse(broadcastData.message)
  }

  private readonly handleResponse = (message: ProgramRunnerResponseMessage) => {
    const id = message.requestId
    if (message.type === 'error') {
      const rejector = this.rejectors.get(id)
      if (rejector) {
        rejector(new Error(message.message))
        this.resolvers.delete(id)
        this.rejectors.delete(id)
      }
      return
    }

    const resolver = this.resolvers.get(id)
    if (resolver) {
      resolver(message)
      this.resolvers.delete(id)
      this.rejectors.delete(id)
    }
  }

  private postRequest<T extends ProgramRunnerRequestPayload>(message: T): Promise<ProgramRunnerResponseForPayload<T>> {
    const requestId = this.requestIdGenerator.next().value!
    const request = { ...message, requestId } as unknown as ProgramRunnerRequestMessage

    return new Promise<ProgramRunnerResponseForPayload<T>>((resolve, reject) => {
      this.resolvers.set(requestId, resolve as (value: ProgramRunnerResponseMessage) => void)
      this.rejectors.set(requestId, reject)
      emitUserBroadcast(this.eventHub, { type: UserBroadcastType.programRunnerRequest, message: request })
    })
  }

  setEnvironment(envVars: EnvironmentVariable[]) {
    return this.postRequest({ type: 'setEnvironment', envVars })
  }

  loadProgram(source: string, mode: number) {
    return this.postRequest({ type: 'loadProgram', source, mode })
  }

  async runProgram(source: string, mode: number, breakpoints: Set<number>, options?: { startPaused?: boolean }): Promise<ProgramRunnerState> {
    const loadResponse = await this.loadProgram(source, mode)
    if (options?.startPaused) {
      return loadResponse.state
    }

    const continueResponse = await this.stepProgram({ type: RunMode.LineBasedBreakpoint, breakpoints }, true)
    return continueResponse.state
  }

  stepLine() {
    return this.postRequest({ type: 'stepLine' })
  }

  continueToNextBreakpoint(breakpoints: Set<number>) {
    return this.postRequest({ type: 'continueToNextBreakpoint', breakpoints: Array.from(breakpoints) })
  }

  continueWithBreakpointSet(breakpoints: Set<number>) {
    return this.postRequest({ type: 'continueWithBreakpointSet', breakpoints: Array.from(breakpoints) })
  }

  stepProgram(options: RunOptions, firstStep = false) {
    return this.postRequest({ type: 'stepProgram', options, firstStep })
  }

  stop() {
    return this.postRequest({ type: 'stop' })
  }

  queryState() {
    return this.postRequest({ type: 'queryState' })
  }

  continueByMode(options: RunOptions) {
    if (options.type === RunMode.NextLine) {
      return this.stepLine()
    }
    return this.continueWithBreakpointSet(options.breakpoints)
  }

  async stateOrThrow(): Promise<ProgramRunnerState> {
    const response = await this.queryState()
    return response.state
  }
}
