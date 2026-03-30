import type { EventHub } from 'golden-layout'
import ProgramWorkerClient from './programWorkerClient'
import { RunMode, type RunOptions } from './workerProtocol'
import {
  type EnvironmentVariable,
  type ProgramRunnerRequestMessage,
  type ProgramRunnerResponseMessage,
  type ProgramRunnerState,
} from './programRunnerProtocol'
import { emitUserBroadcast, onUserBroadcast, UserBroadcastType, type UserBroadcastData } from './types'

export default class ProgramRunner {
  private readonly workerClient: ProgramWorkerClient
  private readonly eventHub: EventHub
  private currentlyRunning = false
  private output = ''
  private lastLine: number | undefined = undefined
  private nextLine: number | undefined = undefined
  private frames: Map<string, any>[] = []
  private envVars: EnvironmentVariable[] = []

  constructor(eventHub: EventHub) {
    this.workerClient = new ProgramWorkerClient()
    this.eventHub = eventHub
    onUserBroadcast(this.eventHub, this.handleBroadcast)
  }

  getState(): ProgramRunnerState {
    return {
      running: this.currentlyRunning,
      output: this.output,
      lastLine: this.lastLine,
      nextLine: this.nextLine,
      frames: this.frames,
      envVars: this.envVars,
    }
  }

  private readonly handleBroadcast = (broadcastData: UserBroadcastData) => {
    if (broadcastData.type !== UserBroadcastType.programRunnerRequest || broadcastData.message === undefined) {
      return
    }
    void this.handleCommand(broadcastData.message)
  }

  private readonly handleCommand = async (command: ProgramRunnerRequestMessage) => {
    const response = await this.processCommand(command)
    emitUserBroadcast(this.eventHub, { type: UserBroadcastType.programRunnerResponse, message: response })
  }

  private async processCommand(command: ProgramRunnerRequestMessage): Promise<ProgramRunnerResponseMessage> {
    try {
      switch (command.type) {
        case 'setEnvironment': {
          this.envVars = command.envVars
          return { type: 'setEnvironmentResult', requestId: command.requestId, state: this.getState() }
        }
        case 'loadProgram': {
          const state = await this.loadProgram(command.source, command.mode)
          return { type: 'loadProgramResult', requestId: command.requestId, state }
        }
        case 'stepLine': {
          const state = await this.stepLine()
          return { type: 'stepLineResult', requestId: command.requestId, state }
        }
        case 'continueToNextBreakpoint': {
          const state = await this.continueToNextBreakpoint(new Set(command.breakpoints))
          return { type: 'continueToNextBreakpointResult', requestId: command.requestId, state }
        }
        case 'continueWithBreakpointSet': {
          const state = await this.continueWithBreakpointSet(new Set(command.breakpoints))
          return { type: 'continueWithBreakpointSetResult', requestId: command.requestId, state }
        }
        case 'stepProgram': {
          const state = await this.stepProgram(command.options, command.firstStep ?? false)
          return { type: 'stepProgramResult', requestId: command.requestId, state }
        }
        case 'stop': {
          this.workerClient.stop()
          return { type: 'stopResult', requestId: command.requestId, state: this.getState() }
        }
        case 'queryState': {
          return { type: 'queryStateResult', requestId: command.requestId, state: this.getState() }
        }
      }
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error)
      return { type: 'error', requestId: command.requestId, message, state: this.getState() }
    }
  }

  private async loadProgram(source: string, mode: number): Promise<ProgramRunnerState> {
    const loadRes = await this.workerClient.loadSource(source, mode)
    if (loadRes.type === 'error') {
      throw new Error(`Error loading program: ${loadRes.message}`)
    }

    if (!loadRes.ok) {
      this.output = loadRes.output || 'No parser output.'
      this.lastLine = undefined
      this.nextLine = undefined
      this.frames = []
      return this.getState()
    }

    const resetRes = await this.workerClient.resetWithEnv(this.envVars.map((ev) => `${ev.key}:${ev.value}`))
    if (resetRes.type === 'error') {
      throw new Error(`Error resetting program state: ${resetRes.message}`)
    }

    const queryRes = await this.workerClient.querySourceLines()
    if (queryRes.type === 'error') {
      throw new Error(`Error querying source lines: ${queryRes.message}`)
    }

    const framesRes = await this.workerClient.currentFrames()
    if (framesRes.type === 'error') {
      throw new Error(`Error querying frames: ${framesRes.message}`)
    }

    const outputRes = await this.workerClient.output()
    if (outputRes.type === 'error') {
      throw new Error(`Error querying output: ${outputRes.message}`)
    }

    this.output = loadRes.output || outputRes.output || 'No parser output.'
    this.lastLine = queryRes.lastLine
    this.nextLine = queryRes.nextLine
    this.frames = framesRes.frames

    return this.getState()
  }
  private async continueWithBreakpointSet(breakpoints: Set<number>, firstStep = false): Promise<ProgramRunnerState> {
    return this.stepProgram({ type: RunMode.LineBasedBreakpoint, breakpoints }, firstStep)
  }

  private async continueToNextBreakpoint(breakpoints: Set<number>, firstStep = false): Promise<ProgramRunnerState> {
    return this.continueWithBreakpointSet(breakpoints, firstStep)
  }

  private async stepLine(): Promise<ProgramRunnerState> {
    return this.stepProgram({ type: RunMode.NextLine })
  }

  private async stepProgram(runOptions: RunOptions, firstStep = false): Promise<ProgramRunnerState> {
    if (this.currentlyRunning) {
      return this.getState()
    }

    this.currentlyRunning = true
    try {
      const runResult = await this.workerClient.run(runOptions, firstStep)
      if (runResult.type === 'error') {
        throw new Error(`Error during execution: ${runResult.message}`)
      }

      this.output = runResult.output || 'No runtime output.'
      this.lastLine = runResult.lastLine
      this.nextLine = runResult.nextLine
      this.frames = runResult.frames
    } finally {
      this.currentlyRunning = false
    }

    return this.getState()
  }
}
