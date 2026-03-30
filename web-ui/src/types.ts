import type { ComponentContainer, EventHub, JsonValue } from "golden-layout";
import type { ProgramRunnerRequestMessage, ProgramRunnerResponseMessage } from "./programRunnerProtocol";

export const enum UserBroadcastType {
  parserOutput,
  envVarsUpdate,
  frameStackUpdate,
  programRunnerUpdate,
  programRunnerRequest,
  programRunnerResponse,
}

interface ParserOutputBroadcastData {
  type: UserBroadcastType.parserOutput,
  output: string
}

interface EnvVarsUpdateBroadcastData {
  type: UserBroadcastType.envVarsUpdate,
  envVars: { key: string, value: string }[]
}

interface FrameStackUpdateBroadcastData {
  type: UserBroadcastType.frameStackUpdate,
  frameStack: Map<string, any>[]
}

interface ProgramRunnerUpdateData {
  type: UserBroadcastType.programRunnerUpdate,
  running: boolean
}

interface ProgramRunnerRequestBroadcastData {
  type: UserBroadcastType.programRunnerRequest,
  message: ProgramRunnerRequestMessage
}

interface ProgramRunnerResponseBroadcastData {
  type: UserBroadcastType.programRunnerResponse,
  message: ProgramRunnerResponseMessage
}

export type UserBroadcastData =
  | ParserOutputBroadcastData
  | EnvVarsUpdateBroadcastData
  | FrameStackUpdateBroadcastData
  | ProgramRunnerUpdateData
  | ProgramRunnerRequestBroadcastData
  | ProgramRunnerResponseBroadcastData;

export function emitUserBroadcast (eventHub: EventHub, data: UserBroadcastData) {
  eventHub.emit('userBroadcast', data);
}

export function onUserBroadcast(eventHub: EventHub, callback: (data: UserBroadcastData) => void) {
  eventHub.on('userBroadcast', <any>(callback));
}

export interface IGoldenLayoutProps {
  glContainer: ComponentContainer,
  glState?: JsonValue
}
