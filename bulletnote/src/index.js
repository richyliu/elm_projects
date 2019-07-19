import { Elm } from './Main.elm';

/* Settings */
const localstorage_key = 'bulletnote_data_v0.0.1';

/***** App initialization *****/
const app = Elm.Main.init();

function save(data) {
  localStorage.setItem(localstorage_key, JSON.stringify(data));
}

function load() {
  return new Promise((resolve, reject) => {
    const item = localStorage.getItem(localstorage_key);
    if (item) resolve(JSON.parse(item));
    else reject();
  });
}

app.ports.saveItemPort.subscribe(save);

app.ports.requestImportItemPort.subscribe(() => {
  load()
    .then(app.ports.importItemPort.send)
    .catch(() => app.ports.importItemPort.send({}));
});
