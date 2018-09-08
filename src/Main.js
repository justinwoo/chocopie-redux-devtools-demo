var ReactDOM = require("react-dom");

exports.render = function(jsx) {
  return function() {
    ReactDOM.render(jsx, document.getElementById("app"));
  };
};
