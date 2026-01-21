import { AnsiUp } from "ansi_up"
import { createSignal } from "solid-js"

export default function OutputPanel(props: any) {
    let ansiUp = new AnsiUp()

    const [parserOutput, setParserOutput] = createSignal<string>("")

    props.glEventHub.on('parserOutput', (output: string) => {
        setParserOutput(output)
    })

    return (
        <>
            <div class="h-full flex flex-1 bg-base-200 rounded-md">
                <div class="flex flex-1 flex-col p-2">
                    <div class="flex-1 bg-black text-white font-mono text-sm p-3 rounded-md overflow-auto">
                        <pre innerHTML={ansiUp.ansi_to_html(parserOutput() || 'No parser output yet.')}></pre>
                    </div>
                </div>
            </div>
        </>
    )
}
