import { createEffect, onMount } from "solid-js";
import { createStore, unwrap } from "solid-js/store";
import type { IGoldenLayoutProps } from "./types";
import ProgramRunnerClient from "./ProgramRunnerClient";

export function EnvironmentSelector(props: IGoldenLayoutProps) {
    let [envVars, setEnvVars] = createStore<{ key: string, value: string }[]>([]);
    const runnerClient = new ProgramRunnerClient(props.glContainer.layoutManager.eventHub)
    let envLoaded = false

    onMount(() => {
        void runnerClient.queryState()
            .then((response) => {
                setEnvVars(response.state.envVars)
            })
            .catch(() => {
            })
            .finally(() => {
                envLoaded = true
            })
    })

    createEffect(() => {
        if (!envLoaded) {
            return
        }
        void runnerClient.setEnvironment(unwrap(envVars));
    });

    return (
        <div class="flex flex-col">
            {envVars.map((envVar, index) => (
                <div class="flex flex-row">
                    <input
                        type="text"
                        placeholder="Name"
                        class="input flex-1"
                        value={envVar.key}
                        onInput={(e) => {
                            const newKey = (e.target as HTMLInputElement).value;
                            setEnvVars(index, "key", newKey);
                        }}
                    />
                    <input
                        type="text"
                        placeholder="Value"
                        class="input flex-1"
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
