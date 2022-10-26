const { Elm } = require("./Main.elm");

const app = Elm.Main.init({
  node: document.getElementById("app"),
  flags: {},
});

app.ports.openLink.subscribe((x) => window.xnft.openWindow(x));
