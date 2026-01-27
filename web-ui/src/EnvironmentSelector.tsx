import { createEffect, createSignal } from "solid-js";
import { createStore, unwrap } from "solid-js/store";
import type { IGoldenLayoutProps } from "./types";

export function EnvironmentSelector(props: IGoldenLayoutProps) {
    let [envVars, setEnvVars] = createStore<{ key: string, value: string }[]>([]);

    createEffect(() => {
        props.glContainer.layoutManager.eventHub.emit('envVarsUpdate', unwrap(envVars));
    });

    return (
        <div class="flex flex-col">
            {envVars.map((envVar, index) => (
                <div class="flex flex-row">
                    <input
                        type="text"
                        placeholder="Name"
                        class="input"
                        value={envVar.key}
                        onInput={(e) => {
                            const newKey = (e.target as HTMLInputElement).value;
                            setEnvVars(index, "key", newKey);
                        }}
                    />
                    <input
                        type="text"
                        placeholder="Value"
                        class="input"
                        value={envVar.value}
                        onInput={(e) => {
                            const newValue = (e.target as HTMLInputElement).value;
                            setEnvVars(index, "value", newValue);
                        }}
                    />
                    <button class="btn bg-red-500 text-white" onClick={() => {
                        setEnvVars((vars) => vars.filter((_, i) => i !== index));
                    }}
                    >Delete</button>
                </div>
            ))}
            { /* Button to add new rows */}
            <button
                class="btn bg-green-600 text-white"
                onClick={() => {
                    setEnvVars(vars => [...vars, { key: "", value: "" }]);
                }}
            >
                Add Variable
            </button>
        </div>
    )
}
