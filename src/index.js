import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

window.addEventListener("beforeunload", function (e) {
  let warning = "Your ingredients and shopping list will be lost when you leave this page.";
  (e || window.event).returnValue = warning;
  return warning;
});

var app = Elm.Main.init();

app.ports.error.subscribe(function(error) {
    alert(error);
});
app.ports.urlToOpen.subscribe(function(url) {
    window.open(url, "_blank");
});

registerServiceWorker();
