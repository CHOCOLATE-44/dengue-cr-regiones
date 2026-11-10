# PARTE A: descargas espaciales (GADM 4.1, WorldPop 2020) y listado de ARS del ASIS 2023.
# Se ejecuta desde la raíz del proyecto (lo llama modelo_dengue.R). Salta lo ya descargado.

library(httr2)

dir_esp <- "datos/fuentes/espacial"
dir.create(dir_esp, showWarnings = FALSE, recursive = TRUE)

bajar <- function(url, destino) {
  if (file.exists(destino) && file.size(destino) > 0) return(invisible(destino))
  cat("Descargando", url, "\n")
  request(url) |> req_retry(max_tries = 5, backoff = function(i) 10 * i) |>
    req_timeout(1800) |> req_perform(path = destino)
  invisible(destino)
}

# A1. GADM 4.1 Costa Rica (gpkg con niveles 0-3)
bajar("https://geodata.ucdavis.edu/gadm/gadm4.1/gpkg/gadm41_CRI.gpkg", file.path(dir_esp, "gadm41_CRI.gpkg"))

# A3. WorldPop 2020, Costa Rica, ~100 m, conteos por píxel sin restricción (listado id = 29)
bajar("https://data.worldpop.org/GIS/Population/Global_2000_2020/2020/CRI/cri_ppp_2020.tif",
      file.path(dir_esp, "cri_ppp_2020.tif"))

# A2. Listado de ARS por región rectora en "ASIS de niveles locales 2023" (Ministerio de Salud)
base_asis <- paste0("https://www.ministeriodesalud.go.cr/index.php/biblioteca-de-archivos-left/",
                    "documentos-ministerio-de-salud/vigilancia-de-la-salud/analisis-de-situacion-salud/",
                    "analisis-de-situacion-de-salud-de-niveles-locales-2023")
carpetas <- c(Brunca = "region-brunca", C.Este = "region-central-este", C.Norte = "region-central-norte",
              C.Sur = "region-central-sur", Chorotega = "region-chorotega", H.Caribe = "region-huetar-caribe",
              H.Norte = "region-huetar-norte", Occidente = "region-occidente", P.Central = "region-pacifico")
f_ars <- file.path(dir_esp, "ars_asis_2023.csv")
if (!file.exists(f_ars)) {
  ars <- do.call(rbind, lapply(names(carpetas), function(r) {
    url <- paste0(base_asis, "/", carpetas[[r]])
    Sys.sleep(2)
    h <- request(url) |> req_retry(max_tries = 5) |> req_perform() |> resp_body_string()
    m <- regmatches(h, gregexpr("<a[^>]+href=\"[^\"]*/file\"[^>]*>[^<]*(<[^>]+>[^<]*)*?</a>", h))[[1]]
    href <- sub(".*href=\"([^\"]*)\".*", "\\1", m)
    txt  <- trimws(gsub("\\s+", " ", gsub("<[^>]+>", " ", m)))
    nom  <- trimws(sub("^pdf\\s*", "", sub("\\(.*$", "", txt)))
    keep <- grepl("ARS", nom)
    data.frame(region = r, documento = nom[keep],
               url = ifelse(startsWith(href[keep], "http"), href[keep],
                            paste0("https://www.ministeriodesalud.go.cr", href[keep])))
  }))
  ars <- unique(ars)
  write.csv(ars, f_ars, row.names = FALSE, fileEncoding = "UTF-8")
}
ars <- read.csv(f_ars, fileEncoding = "UTF-8")
cat("ARS listados por región (ASIS 2023):\n"); print(table(ars$region))
