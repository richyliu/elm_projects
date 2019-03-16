import { Elm } from './Main.elm';

const search = new URLSearchParams(location.search);

const gameId = search.get('id') || '';
const player = search.get('player') || 'red';

const app = Elm.Main.init({
  node: document.getElementById('main'),
  flags: gameId + ' ' + player,
});

if (gameId.length > 0) {
  const firebaseUrl = `https://main-fe047.firebaseio.com/ultimatettt/${gameId}.json`;
  let source = new EventSource(firebaseUrl);
  source.addEventListener('put', res => {
    const parsed = JSON.parse(res.data);

    if (parsed.path !== '/') {
      // sent new move
      console.log('[js]: Firebase received data:', parsed);
      app.ports.receiveAddMove.send(parsed.data);
    } else {
      // sent all the current moves so far
      console.log(Object.values(parsed.data));
      app.ports.receiveFirstPlayer.send(parsed.data.firstPlayer || '');

      // delay these to ensure first player gets through first
      setTimeout(
        () =>
          Object.values(parsed.data)
            .filter(d => typeof d === 'object')
            .forEach(app.ports.receiveAddMove.send),
        1
      );
    }
  });
}

window['app__'] = app;
