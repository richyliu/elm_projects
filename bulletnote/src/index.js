import { Elm } from './Main.elm';

/***** App initialization *****/
const app = Elm.Main.init();

app.ports.saveItemPort.subscribe(console.log);

app.ports.requestImportItemPort.subscribe(() => {
  // simulate request time
  setTimeout(() => {
    app.ports.importItemPort.send({
      id: 'todo',
      content: { name: 'todo', time: '2018-03-02T17:30' },
      children: [
        {
          id: 'eat pie 1',
          content: { name: 'eat pie 1', time: '2018-03-02T17:30' },
          children: [],
        },
        {
          id: 'wash face',
          content: { name: 'wash face', time: '2018-03-02T17:30' },
          children: [
            {
              id: 'wash ears',
              content: { name: 'wash ears', time: '2018-03-02T17:30' },
              children: [],
            },
            {
              id: 'wash nose',
              content: { name: 'wash nose', time: '2018-03-02T17:30' },
              children: [],
            },
          ],
        },
        {
          id: 'eat pie',
          content: { name: 'eat pie', time: '2018-03-02T17:30' },
          children: [],
        },
      ],
    });
  }, 1000);
});
