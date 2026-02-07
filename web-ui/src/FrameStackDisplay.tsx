import { createSignal, For } from "solid-js";
import { onUserBroadcast, UserBroadcastType, type IGoldenLayoutProps, type UserBroadcastData } from "./types";

export function FrameStackDisplay(props: IGoldenLayoutProps) {
    const [frameStack, setFrameStack] = createSignal<Map<string, any>[]>([])

    onUserBroadcast(props.glContainer.layoutManager.eventHub, (broadcastData: UserBroadcastData) => {
        if (broadcastData.type === UserBroadcastType.frameStackUpdate) {
            setFrameStack(broadcastData.frameStack.reverse())
        }
    })

    return (
        <>
            <For each={frameStack()}>
                {(frame, idx) => (
                    <div data-index={idx()}>
                        <pre>{JSON.stringify(Object.fromEntries(frame), null, 2)}</pre>
                    </div>
                )}
            </For>
        </>
    )
}
