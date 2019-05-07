const express = require('express');
const bodyParser = require('body-parser');
const cors = require('cors');
const helmet = require('helmet');
const morgan = require('morgan');
const jwt = require('express-jwt');
const jwksRsa = require('jwks-rsa');
const mongoose = require('mongoose');
mongoose.connect('mongodb://localhost:27017/default', {
  useNewUrlParser: true,
});

/****** SETUP AND MIDDLEWARE ******/

const port = 8111;
const app = express();
app.use(helmet());
app.use(bodyParser.json());
app.use(cors());
app.use(morgan('combined'));

app.get('/', (req, res) => res.send({ msg: 'hello world' }));

// anything after this requires authentication
const checkJwt = jwt({
  secret: jwksRsa.expressJwtSecret({
    cache: true,
    rateLimit: true,
    jwksRequestsPerMinute: 5,
    jwksUri: `https://t485.auth0.com/.well-known/jwks.json`,
  }),

  // Validate the audience and the issuer.
  audience: 'https://db-api',
  issuer: `https://t485.auth0.com/`,
  algorithms: ['RS256'],
});
app.use(checkJwt);
app.use((err, req, res, next) => {
  if (err.name === 'UnauthorizedError')
    res.status(err.status).send({ message: err.message });
  else next();
});

// connect to mongoose
const db = mongoose.connection;
db.on('error', console.error.bind(console, 'connection error:'));

/****** ROUTE DEFINITIONS ******/

db.once('open', () => {
  // define schema for creating items
  const itemSchemaObj = {
    name: String,
    owner: String,
    img: String,
    description: String,
  };
  const itemSchema = new mongoose.Schema(makeAllRequired(itemSchemaObj));
  // item constructor
  const Item = mongoose.model('Item', itemSchema);

  app.get('/items', (req, res) =>
    Item.find((error, items) => {
      if (error) res.status(500).send(stringify({ error }));
      else res.send(stringify(items));
    })
  );

  app.post('/items', (req, res) => {
    const item = new Item(req.body);
    item.save((error, savedItem) => {
      if (error) res.status(400).send(stringify({ error }));
      else res.send({ id: savedItem._id });
    });
  });

  app.get('/items/:id', (req, res) => {
    Item.findById(req.params.id, (error, item) => {
      if (error) res.status(500).send(stringify({ error }));
      else {
        if (item === null) res.status(404).send({ msg: 'not found' });
        else res.send(item);
      }
    });
  });

  app.delete('/items/:id', (req, res) => {
    Item.findByIdAndRemove(req.params.id, (error, item) => {
      if (error) res.status(500).send({ error });
      else {
        if (item === null) res.status(404).send({ msg: 'not found' });
        else res.send({ msg: 'successfully deleted' });
      }
    });
  });

  app.listen(port, () => console.log(`Listening on port ${port}...`));
});


/****** HELPER FUNCTIONS ******/

function makeAllRequired(schemaObj) {
  let newObj = {};
  for (const key in schemaObj) {
    newObj[key] = {
      type: schemaObj[key],
      required: true,
    };
  }
  return newObj;
}

function stringify(obj) {
  return JSON.stringify(obj, null, 2);
}
