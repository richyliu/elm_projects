import { Elm } from './Main.elm';

const app = Elm.Main.init({
  node: document.getElementById('main'),
  flags: { api: 'http://localhost:8111' },
});
