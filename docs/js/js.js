(function () {
  var input = document.getElementById('input');
  var output = document.getElementById('output');

  input.focus();

  input.addEventListener('keyup', function (e) {
    try {
      var css = e.target.value;
      output.value = css2garden.core.convert_pretty(css);
    } catch { };

  }, false);

})();