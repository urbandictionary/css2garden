(function () {
  var input = document.getElementById('input');
  var output = document.getElementById('output');
  var $alert = $('<div id="error-alert" class="alert alert-danger"></div>');

  input.focus();

  input.addEventListener('keyup', function (e) {
    try {
      var css = e.target.value;
      output.value = css2garden.core.convert_pretty(css);

      $alert.alert('close');
    } catch(err) {
      $alert.text(err);
      $('body').prepend($alert)
      $alert.alert();
    };

  }, false);

})();