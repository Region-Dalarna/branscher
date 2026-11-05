// tooltips.js
// Hover-tooltips på knappar (t.ex. indikatorknapparna i N3), via tippy.js
// (självhostat, se www/tippy-bundle.umd.min.js – ingen extern CDN).
//
// shinyWidgets::radioGroupButtons() bygger sina <button>-taggar lat
// (htmltools::tagFunction) först när HTML:en renderas i webbläsaren, så
// R-koden kan inte skapa tooltips direkt på knapparna. I stället skickar
// R-sidan med en liten JSON-blob i attributet data-tooltips (<script
// type="application/json" class="rd-tooltip-data" data-input-id="..."
// data-tooltips='{"varde":"text", ...}'>), och den här filen läser den
// och kopplar en tippy-instans till rätt knapp i den färdiga DOM:en.
// (JSON:en ligger medvetet i ett attribut och inte som text-innehåll i
// <script>-taggen – webbläsaren HTML-avkodar inte text-innehåll i
// <script>-element, så en HTML-kodad JSON-sträng där skulle inte gå att
// parsa. Attributvärden avkodas alltid korrekt.)

function satt_tooltips_for(scriptEl) {
  // Redan hanterad? Slipp jobba om samma script-tagg varje gång
  // MutationObserver triggar (den kan trigga väldigt ofta).
  if (scriptEl.dataset.tooltipsSatta === 'true') return;

  var inputId = scriptEl.getAttribute('data-input-id');
  var container = document.getElementById(inputId);
  if (!container) return;

  var karta;
  try {
    karta = JSON.parse(scriptEl.getAttribute('data-tooltips'));
  } catch (e) {
    return;
  }

  container.querySelectorAll('input[type="radio"], input[type="checkbox"]').forEach(function (inp) {
    var text = karta[inp.value];
    if (!text) return;
    // Knappen kan vara antingen <button> (Bootstrap 3, standard i appen)
    // eller <label> (Bootstrap 5-läge) – closest() hittar rätt oavsett.
    var knapp = inp.closest('button, label') || inp.parentElement;
    if (knapp && window.tippy) {
      tippy(knapp, {
        content: text,
        theme: 'rd',
        placement: 'top',
        delay: [150, 0],
        maxWidth: 260
      });
    }
  });

  scriptEl.dataset.tooltipsSatta = 'true';
}

function satt_alla_tooltips() {
  document.querySelectorAll('script.rd-tooltip-data').forEach(satt_tooltips_for);
}

// Skriptet ligger i <head> och körs alltså INNAN <body> finns i DOM:en.
// document.body är null vid det laget, så vi får inte anropa
// observer.observe(document.body, ...) direkt – det kraschar tyst. Vi
// väntar tills dokumentet är klart (eller kör direkt om det redan är det,
// t.ex. om skriptet av någon anledning laddas sent).
function mula_igang() {
  satt_alla_tooltips();

  // Shiny renderar/uppdaterar delar av sidan asynkront på sätt som inte
  // pålitligt triggar ett "shiny:value"-event vår lyssnare fångar (testat
  // – fungerar inte för den här typen av output). I stället bevakar vi
  // DOM:en direkt: närhelst något ändras (Shiny lägger till/byter ut
  // HTML), kollar vi om det finns tooltip-data att applicera.
  // satt_tooltips_for() hoppar över redan hanterade element så detta är
  // billigt även om det triggar ofta.
  var observer = new MutationObserver(function () {
    satt_alla_tooltips();
  });
  observer.observe(document.body, { childList: true, subtree: true });
}

if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', mula_igang);
} else {
  mula_igang();
}
