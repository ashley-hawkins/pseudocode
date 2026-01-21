/* @refresh reload */
import { render } from 'solid-js/web'
import './index.css'
import App from './App.tsx'
import { GoldenLayout } from 'golden-layout'

import 'golden-layout/dist/css/goldenlayout-base.css';
import 'golden-layout/dist/css/themes/goldenlayout-dark-theme.css';
import OutputPanel from './OutputPanel.tsx';
import type { JSXElement } from 'solid-js';

const root = document.getElementById('root')

const layout = new GoldenLayout(root!)

const registerSolid = (TheComponent: ((arg0: any) => JSXElement)): GoldenLayout.ComponentFactoryFunction => {
    return (container, state) => {
        const dispose = render(() => <TheComponent glEventHub={layout.eventHub} />, container.element)
        container.on('destroy', () => {
            dispose()
        })

        return true
    }
}

layout.registerComponentFactoryFunction('app', registerSolid(App));
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
