import { Elm } from './Main.elm';

Elm.Main.init({ node: document.querySelector('main') });

if (module.hot) module.hot.dispose(() => window.location.reload());

