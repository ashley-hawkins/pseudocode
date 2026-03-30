import { requestIdGenerator as createRequestIdGenerator, type RequestId, type RequestPayload, type RequestWithoutId, type ResponseForRequestPayload, type ResponseMessage, type RunOptions } from './workerProtocol'

export default class ProgramWorkerClient {
  worker: Worker
  requestIdGenerator = createRequestIdGenerator()
  resolvers: Map<RequestId, (v: ResponseMessage) => void>
  rejectors: Map<RequestId, (e: Error) => void>

  constructor() {
    this.worker = new Worker(new URL('./programWorker.ts', import.meta.url), { type: 'module' })
    this.resolvers = new Map()
    this.rejectors = new Map()

    this.worker.onmessage = (e: MessageEvent<ResponseMessage>) => {
      const msg = e.data
      const id = msg.requestId
      switch (msg.type) {
        case 'loadResult':
        case 'resetResult':
        case 'queryResult':
        case 'stepResult':
        case 'currentFramesResult':
        case 'outputResult':
        case 'runResult':
          {
            const resolver = this.resolvers.get(id)
            if (resolver) {
              resolver(msg)
              this.resolvers.delete(id)
              this.rejectors.delete(id)
            }
          }
          break
        case 'error':
          {
            const rej = this.rejectors.get(id)
            if (rej) {
              rej(new Error(msg.message))
              this.resolvers.delete(id)
              this.rejectors.delete(id)
            }
          }
          break
      }
    }
  }

  private postRequest<T extends RequestPayload>(msg: T): Promise<ResponseForRequestPayload<T>> {
    const id = this.requestIdGenerator.next().value!
    const req = { ...msg, requestId: id }
    return new Promise<ResponseForRequestPayload<T>>((resolve, reject) => {
      this.resolvers.set(id, resolve as any)
      this.rejectors.set(id, reject)
      this.worker.postMessage(req)
    })
  }

  private postMessage(msg: RequestWithoutId) {
    this.worker.postMessage(msg)
  }

  loadSource(source: string, mode: number) {
    return this.postRequest({ type: 'load', source, mode })
  }

  resetWithEnv(env: string[]) {
    return this.postRequest({ type: 'reset', env })
  }

  querySourceLines() {
    return this.postRequest({ type: 'queryLines' })
  }

  stepOnce() {
    return this.postRequest({ type: 'step' })
  }

  currentFrames() {
    return this.postRequest({ type: 'currentFrames' })
  }

  output() {
    return this.postRequest({ type: 'output' })
  }

  run(options: RunOptions, firstStep: boolean = false) {
    return this.postRequest({ type: 'run', options, firstStep })
  }

  stop() {
    this.postMessage({ type: 'stop' })
  }
}
