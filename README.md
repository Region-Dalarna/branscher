# branscher

Detta repository innehåller en Shinyapplikation (`branscher`) för Samhällsanalys, Region Dalarna.

## Struktur

Appfilerna ligger **direkt i repo-roten** (så att Shiny Server kör appen utan omväg):

- `ui.R`, `server.R`, `global.R`
- `www/` för favicon, logotyp, CSS (`regiondalarna_ruf.css` + `app.css`) och `fonts/`
- `R/` för hjälpfunktioner

- `_dependencies.R` i root listar alla paket appen använder (läses av `renv::dependencies()`, körs aldrig)
- `_publicering_till_server.yml` i root styr vilken Shiny-server som är default för `shinyapp_publicera()`
- `renv.lock` + `renv/` + `.Rprofile` styr paketversioner. Kör `renv::restore()` efter klon för att få samma paket som senast snapshot:ades. Vid nya paket: `renv::install(...)` följt av `renv::snapshot()`.

- Deployment sker via GitHub Actions:
  - `.github/workflows/deploy.yml` – publicerar vid push till `publicera-publik` eller `publicera-intern`
  - `.github/workflows/avpublicera.yml` – tar bort appen från vald server (manuell trigger)

  Appmapp på servern: `/srv/shiny-server/branscher`.

