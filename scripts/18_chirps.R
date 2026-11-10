# NO SE USA COMO FUENTE (servidor sin respuesta): reemplazado por scripts/22_chirps_iri.R. Su caché de 6 meses sirve para comparar.
# PARTE B3 (lluvia): CHIRPS v3.0 mensual, 1991-01 a 2026-08, recortado a Costa Rica y agregado a 0,1°.
# Lee cada GeoTIFF global por /vsicurl/ (solo la ventana de Costa Rica) y guarda una caché local por mes.

library(terra)
Sys.setenv(GDAL_HTTP_TIMEOUT = "90", GDAL_HTTP_CONNECTTIMEOUT = "30", GDAL_HTTP_MAX_RETRY = "3", GDAL_HTTP_RETRY_DELAY = "10")

dir_c <- "datos/fuentes/chirps_v3_cr"
dir.create(dir_c, showWarnings = FALSE, recursive = TRUE)
url_base <- "https://data.chc.ucsb.edu/products/CHIRPS/v3.0/monthly/global/tifs/"
plantilla <- rast("datos/procesados/poblacion_grilla_0p1.tif")
meses <- format(seq(as.Date("1991-01-01"), as.Date("2026-08-01"), by = "month"), "%Y.%m")

fallidos <- character()
for (m in meses) {
  destino <- file.path(dir_c, paste0("chirps_cr_", m, ".tif"))
  if (file.exists(destino)) next
  ok <- FALSE
  for (intento in 1:5) {
    r <- try({
      g <- rast(paste0("/vsicurl/", url_base, "chirps-v3.0.", m, ".tif"))
      x <- crop(g, ext(plantilla))
      NAflag(x) <- -9999
      x[x < 0] <- NA
      writeRaster(x, destino, overwrite = TRUE)
    }, silent = TRUE)
    if (!inherits(r, "try-error")) { ok <- TRUE; break }
    Sys.sleep(10 * intento)
  }
  if (!ok) fallidos <- c(fallidos, m)
}
if (length(fallidos)) stop("CHIRPS v3: no se pudieron leer los meses ", paste(fallidos, collapse = ", "))

# Pila mensual 0,05° -> 0,1° (media de las 4 celdas; mm/mes)
pila <- rast(file.path(dir_c, paste0("chirps_cr_", meses, ".tif")))
names(pila) <- meses
lluvia01 <- resample(pila, plantilla, method = "average")
writeRaster(lluvia01, "datos/procesados/chirps_v3_0p1_mensual.tif", overwrite = TRUE)
cat("CHIRPS v3 a 0,1°:", nlyr(lluvia01), "meses,", meses[1], "a", meses[length(meses)], "\n")
print(global(lluvia01[[c(1, 4, 7, 10)]], "mean", na.rm = TRUE))
