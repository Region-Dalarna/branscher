// Sidopanelen följer med när man scrollar (position: sticky, se app.css).
// Är panelen högre än fönstret sätts top negativt, så att dess nederkant
// (med t.ex. kön/ålder) ligger kvar i bild i stället för att hamna under
// fönstrets underkant. Räknas om när panelens höjd ändras (flikbyte,
// villkorade fält) och när fönstret ändrar storlek.
(function () {
  var MARGINAL = 16;

  function justera(el) {
    var h = el.offsetHeight;
    if (!h) return;
    el.style.top = Math.min(MARGINAL, window.innerHeight - h - MARGINAL) + 'px';
  }

  function alla() {
    document.querySelectorAll('.rd-sidebar').forEach(justera);
  }

  document.addEventListener('DOMContentLoaded', function () {
    if ('ResizeObserver' in window) {
      var ro = new ResizeObserver(function (poster) {
        poster.forEach(function (p) { justera(p.target); });
      });
      document.querySelectorAll('.rd-sidebar').forEach(function (el) { ro.observe(el); });
    }
    alla();
  });
  window.addEventListener('resize', alla);
})();
