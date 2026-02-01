import type { ComponentContainer, EventHub, JsonValue } from "golden-layout";

export const enum UserBroadcastType {
  parserOutput,
  envVarsUpdate
}

interface ParserOutputBroadcastData {
  type: UserBroadcastType.parserOutput,
  output: string
}

interface EnvVarsUpdateBroadcastData {
  type: UserBroadcastType.envVarsUpdate,
  envVars: { key: string, value: string }[]
}

export type UserBroadcastData = ParserOutputBroadcastData | EnvVarsUpdateBroadcastData;

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
