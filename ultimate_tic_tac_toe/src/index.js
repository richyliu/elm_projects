import { Elm } from './Main.elm';

const gameId = 'foo';

const firebaseUrl = `https://main-fe047.firebaseio.com/ultimatettt/${gameId}.json`;

const app = Elm.Main.init({
  node: document.getElementById('main'),
  flags: firebaseUrl,
});

let source = new EventSource(firebaseUrl);
source.addEventListener('put', res => {
  const parsed = JSON.parse(res.data);

  if (parsed.path !== '/') {
    // sent new move
    console.log('[js]: Firebase received data:', parsed);
    app__.ports.receiveAddMove.send(parsed.data);
  } else {
    // sent all the current moves so far
    console.log(Object.values(parsed.data));
    Object.values(parsed.data).forEach(app__.ports.receiveAddMove.send);
  }
});

window['app__'] = app;
