import { Elm } from './Hello.elm';

Elm.Hello.init({ node: document.querySelector('main') });

if (module.hot) module.hot.dispose(() => window.location.reload());

