import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

var app = Elm.Main.init();

app.ports.error.subscribe(function(error) {
    alert(error);
});

registerServiceWorker();
