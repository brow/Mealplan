import './main.css';
import { Elm } from './Main.elm';

var app = Elm.Main.init({
  node: document.getElementById('root')
});
app.ports.error.subscribe(function(error) {
    console.error(error);
});
