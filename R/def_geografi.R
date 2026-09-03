# ============================================================
#  def_geografi.R – geografiska definitioner för branschappen
#  (kommuner i Dalarna, plus länskod för uppslag mot riksdata)
# ============================================================

# OBS: statisk PLACEHOLDER-lista, i linje med hur def_geografi.R används
# i utbildningsappen. Ersätts/kompletteras av func_data.R:s uppslag mot
# mikro_db.dim_kommun när databaskopplingen är på plats.

DALARNA_KOMMUNER <- tibble::tribble(
  ~kommun_kod, ~kommun_namn,
  '2021', 'Vansbro',
  '2023', 'Malung-Sälen',
  '2026', 'Gagnef',
  '2029', 'Leksand',
  '2031', 'Rättvik',
  '2034', 'Orsa',
  '2039', 'Älvdalen',
  '2061', 'Smedjebacken',
  '2062', 'Mora',
  '2080', 'Falun',
  '2081', 'Borlänge',
  '2082', 'Säter',
  '2083', 'Hedemora',
  '2084', 'Avesta',
  '2085', 'Ludvika'
)

LAN_KOD_DALARNA <- '20'
