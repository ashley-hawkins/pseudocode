import type { ComponentContainer, EventHub, JsonValue } from "golden-layout";

export const enum UserBroadcastType {
  parserOutput,
  envVarsUpdate,
  frameStackUpdate
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

export type UserBroadcastData = ParserOutputBroadcastData | EnvVarsUpdateBroadcastData | FrameStackUpdateBroadcastData;

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
