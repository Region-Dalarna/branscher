# _dependencies.R – läses av renv::dependencies(), körs aldrig
# Lägg till alla paket appen använder, även de som laddas via source().
library(DBI)
library(RPostgres)
library(sf)
library(dbplyr)
library(shiny.telemetry)
library(ggtext)
# ... lägg till fler paket vid behov
