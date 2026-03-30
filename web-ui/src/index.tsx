/* @refresh reload */
import { render } from 'solid-js/web'
import './index.css'
import ProgramEditor from './Editor.tsx'
import { GoldenLayout, LayoutConfig } from 'golden-layout'

import 'golden-layout/dist/css/goldenlayout-base.css';
import 'golden-layout/dist/css/themes/goldenlayout-light-theme.css';
import OutputPanel from './OutputPanel.tsx';
import type { JSXElement } from 'solid-js';
import { EnvironmentSelector } from './EnvironmentSelector.tsx';
import type { IGoldenLayoutProps } from './types.ts';
import { FrameStackDisplay } from './FrameStackDisplay.tsx';
import ProgramRunner from './ProgramRunner.tsx';

const solidFactory = (TheComponent: ((props: IGoldenLayoutProps) => JSXElement)): GoldenLayout.ComponentFactoryFunction => {
    console.log("Creating Solid Component:", TheComponent.name);
    return (container, state) => {
        const dispose = render(() => <TheComponent glContainer={container} glState={state} />, container.element)
        container.on('destroy', () => {
            dispose()
        })

        return true
    }
}

declare module 'golden-layout' {
    interface GoldenLayout {
        registerSolidComponent: (TheComponent: ((props: IGoldenLayoutProps) => JSXElement)) => void;
    }
}

GoldenLayout.prototype.registerSolidComponent = function (TheComponent: ((props: IGoldenLayoutProps) => JSXElement)) {
    this.registerComponentFactoryFunction(TheComponent.name, solidFactory(TheComponent))
}

let container: HTMLElement | undefined;
let layoutConfig: LayoutConfig = { root: undefined }

if (!new URL(window.location.href).searchParams.has("gl-window")) {
    container = document.getElementById('root')!

    layoutConfig = {
        root: {
            type: 'row',
            content: [{
                type: 'component',
                componentType: ProgramEditor.name,
                title: 'Code Editor',
            },
            {
                type: 'component',
                componentType: OutputPanel.name,
                title: 'Output Panel',
            },
            {
                type: 'column',
                content: [
                    {
                        type: 'component',
                        componentType: EnvironmentSelector.name,
                        title: 'Environment Selector',
                    },
                    {
                        type: 'component',
                        componentType: FrameStackDisplay.name,
                        title: 'Frames',
                    }
                ]
            }]
        },
    }
}

const layout = new GoldenLayout(container)
layout.resizeWithContainerAutomatically = true;

if (!new URL(window.location.href).searchParams.has("gl-window")) {
    new ProgramRunner(layout.eventHub)
}

layout.registerSolidComponent(ProgramEditor)
layout.registerSolidComponent(OutputPanel)
layout.registerSolidComponent(EnvironmentSelector)
layout.registerSolidComponent(FrameStackDisplay)
if (layoutConfig.root) {
    layout.loadLayout(layoutConfig);
}
