import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

const LocalStorage = require('@the-sett/elm-localstorage').ElmLocalStoragePorts;

const app = Elm.Main.init({
  node: document.getElementById('root')
});

const localStorage = new LocalStorage();
localStorage.subscribe(app);

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
