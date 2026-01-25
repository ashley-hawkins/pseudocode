/* @refresh reload */
import { render } from 'solid-js/web'
import './index.css'
import ProgramEditor from './Editor.tsx'
import { GoldenLayout } from 'golden-layout'

import 'golden-layout/dist/css/goldenlayout-base.css';
import 'golden-layout/dist/css/themes/goldenlayout-light-theme.css';
import OutputPanel from './OutputPanel.tsx';
import type { JSXElement } from 'solid-js';

const root = document.getElementById('root')

const layout = new GoldenLayout(root!)
layout.resizeWithContainerAutomatically = true;

const registerSolid = (TheComponent: ((arg0: any) => JSXElement)): GoldenLayout.ComponentFactoryFunction => {
    return (container, state) => {
        const dispose = render(() => <TheComponent glContainer={container} />, container.element)
        container.on('destroy', () => {
            dispose()
        })

        return true
    }
}

layout.registerComponentFactoryFunction('app', registerSolid(ProgramEditor));
layout.registerComponentFactoryFunction('output-panel', registerSolid(OutputPanel));

layout.loadLayout({
    root: {
        type: 'row',
        content: [{
            type: 'component',
            componentType: 'app',
            title: 'Code Editor',
        }, {
            type: 'component',
            componentType: 'output-panel',
            title: 'Output Panel',
        }]
    },
});
