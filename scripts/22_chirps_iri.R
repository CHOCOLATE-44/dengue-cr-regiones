# PARTE B3 (lluvia): CHIRPS v3.0 mensual desde el espejo de la IRI Data Library, recortado a Costa Rica,
# 1991-01 a 2026-08. Verifica unidades, compara con los 6 meses en caché del servidor de CHC (UCSB)
# y agrega a la grilla de 0,1°.

library(httr2)
library(terra)
library(ncdf4)

dir_i <- "datos/fuentes/chirps_v3_iri"
dir.create(dir_i, showWarnings = FALSE, recursive = TRUE)
f_nc <- file.path(dir_i, "chirps_v3_monthly_CR_1991-2026.nc")
url <- paste0("https://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v3p0/.monthly/.global/.precipitation/",
              "X/-86/-82.5/RANGEEDGES/Y/8/11.3/RANGEEDGES/T/(Jan%201991)/(Aug%202026)/RANGEEDGES/data.nc")
if (!file.exists(f_nc) || file.size(f_nc) == 0)
  request(url) |> req_retry(max_tries = 5, backoff = function(i) 30 * i) |> req_timeout(3600) |>
    req_perform(path = f_nc)

# Unidades y eje temporal, tal como los declara el NetCDF
nc <- nc_open(f_nc)
v  <- names(nc$var)[1]
unidad <- ncatt_get(nc, v, "units")$value
t_u    <- nc$dim$T$units; t_cal <- ncatt_get(nc, "T", "calendar")$value
t_val  <- nc$dim$T$vals
cat("Variable:", v, "| unidades:", unidad, "| T:", t_u, "| calendario:", t_cal, "| n meses:", length(t_val), "\n")
nc_close(nc)
stopifnot(grepl("months since", t_u))
origen <- as.Date(sub(".*months since ([0-9-]+).*", "\\1", t_u))
fechas <- seq(origen, by = "month", length.out = floor(max(t_val)) + 1)[floor(t_val) + 1]
cat("Primer y último mes:", format(range(fechas), "%Y-%m"), "\n")
stopifnot(identical(format(fechas, "%Y-%m"),
                    format(seq(as.Date("1991-01-01"), as.Date("2026-08-01"), by = "month"), "%Y-%m")))

lluvia <- rast(f_nc)
names(lluvia) <- format(fechas, "%Y-%m")
# La IRI rotula la longitud media celda (0,025°) al oeste del centro real de CHIRPS (X = -86, -85.95, ...;
# los centros de CHIRPS son -85.975, -85.925, ...). Se comprobó que con este corrimiento los valores
# coinciden exactamente con los archivos de CHC (tabla de comparación abajo). La latitud no está corrida.
lluvia <- shift(lluvia, dx = 0.025)
# Pasar a mm/mes si viene como tasa diaria
dias_mes <- as.numeric(diff(c(fechas, seq(fechas[length(fechas)], by = "month", length.out = 2)[2])))
if (grepl("day", unidad)) {
  lluvia <- lluvia * dias_mes
  cat("Unidad diaria: se multiplicó por los días de cada mes -> mm/mes\n")
} else if (!grepl("month", unidad)) stop("Unidad de CHIRPS no reconocida: ", unidad)

# Comparación con los 6 meses en caché de data.chc.ucsb.edu (mm/mes)
cache <- file.path("datos/fuentes/chirps_v3_cr", paste0("chirps_cr_1991.0", 1:6, ".tif"))
comp <- do.call(rbind, lapply(1:6, function(k) {
  ch <- rast(cache[k]); ir <- crop(lluvia[[k]], ch, snap = "near"); ir <- resample(ir, ch, method = "near")
  d <- values(ir - ch, mat = FALSE); ok <- !is.na(d)
  data.frame(mes = sprintf("1991-%02d", k), celdas = sum(ok),
             media_chc = mean(values(ch)[ok]), media_iri = mean(values(ir)[ok]),
             dif_abs_max = max(abs(d[ok])), dif_abs_media = mean(abs(d[ok])),
             correlacion = cor(values(ch)[ok], values(ir)[ok]))
}))
print(comp, digits = 4)
write.csv(comp, "modelo/resultados/chirps_iri_vs_chc_1991.csv", row.names = FALSE)
if (any(comp$dif_abs_max > 0.5)) stop("CHIRPS de la IRI y de CHC no coinciden en 1991-01 a 1991-06 (diferencia > 0,5 mm/mes). Revisar antes de seguir.")

plantilla <- rast("datos/procesados/poblacion_grilla_0p1.tif")
lluvia01 <- resample(lluvia, plantilla, method = "average")
writeRaster(lluvia01, "datos/procesados/chirps_v3_0p1_mensual.tif", overwrite = TRUE)
cat("CHIRPS v3 (IRI) a 0,1°:", nlyr(lluvia01), "meses en mm/mes\n")
