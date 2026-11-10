# NO SE USA (decisión del usuario, etapa 9): reemplazado por scripts/21_era5land_cds.R.
# PARTE B1: temperatura diaria ERA5-Land (media, máxima, mínima) vía Open-Meteo, 1991-01-01 a 2026-08-31,
# en los centros de las celdas de 0,1° con población > 0. Caché local por celda; pausas y reintentos ante 429.
# Datos: Open-Meteo (https://open-meteo.com/, CC BY 4.0) y ERA5-Land (Muñoz Sabater J. ERA5-Land hourly data
# from 1950 to present. Copernicus C3S CDS. doi:10.24381/cds.e2161bac). Ver auditoria.md.
#
# Uso: Rscript scripts/20_era5_openmeteo.R            -> solo planifica (cuenta celdas y peso de la API)
#      DESCARGAR_ERA5=1 Rscript scripts/20_era5_openmeteo.R -> descarga lo que falte hasta topar el límite

library(terra)
library(httr2)
library(jsonlite)

dir_t <- "datos/fuentes/era5land_openmeteo"
dir.create(dir_t, showWarnings = FALSE, recursive = TRUE)
pob <- rast("datos/procesados/poblacion_grilla_0p1.tif")
celdas <- as.data.frame(pob, xy = TRUE, na.rm = TRUE)
celdas <- celdas[celdas$poblacion_wp2020 > 0, ]
celdas$id <- sprintf("lat%.2f_lon%.2f", celdas$y, celdas$x)
write.csv(celdas, file.path(dir_t, "celdas.csv"), row.names = FALSE)

inicio <- as.Date("1991-01-01"); fin <- as.Date("2026-08-31")
variables <- c("temperature_2m_mean", "temperature_2m_max", "temperature_2m_min")

# Peso por pedido según Open-Meteo: n_lugares × (n_días / 14) × (n_variables / 10)
n_dias <- as.numeric(fin - inicio) + 1
peso_celda <- (n_dias / 14) * (length(variables) / 10)
hechas <- file.exists(file.path(dir_t, paste0(celdas$id, ".csv")))
plan <- data.frame(celdas = nrow(celdas), celdas_en_cache = sum(hechas), dias = n_dias,
                   peso_por_celda = peso_celda, peso_total_pendiente = peso_celda * sum(!hechas),
                   limite_diario_gratuito = 10000, limite_horario_gratuito = 5000,
                   dias_minimos_de_descarga = ceiling(peso_celda * sum(!hechas) / 10000))
print(plan)
write.csv(plan, "modelo/resultados/plan_descarga_era5.csv", row.names = FALSE)

if (Sys.getenv("DESCARGAR_ERA5") != "1") {
  cat("Modo planificación: no se descarga nada. Defina DESCARGAR_ERA5=1 para descargar.\n")
} else {
  for (i in which(!hechas)) {
    destino <- file.path(dir_t, paste0(celdas$id[i], ".csv"))
    req <- request("https://archive-api.open-meteo.com/v1/archive") |>
      req_url_query(latitude = celdas$y[i], longitude = celdas$x[i],
                    start_date = format(inicio), end_date = format(fin),
                    daily = paste(variables, collapse = ","), models = "era5_land", timezone = "GMT") |>
      req_retry(max_tries = 6, is_transient = function(r) resp_status(r) %in% c(429, 500, 502, 503),
                backoff = function(k) 60 * k) |>
      req_error(is_error = function(r) FALSE)
    r <- req_perform(req)
    if (resp_status(r) == 429) { cat("Límite de la API alcanzado en la celda", i, "; se sigue en la próxima corrida.\n"); break }
    if (resp_status(r) != 200) stop("Open-Meteo respondió ", resp_status(r), " en ", celdas$id[i], ": ", resp_body_string(r))
    j <- fromJSON(resp_body_string(r))
    d <- data.frame(fecha = j$daily$time, tmean = j$daily$temperature_2m_mean,
                    tmax = j$daily$temperature_2m_max, tmin = j$daily$temperature_2m_min,
                    lat_celda = celdas$y[i], lon_celda = celdas$x[i],
                    lat_era5 = j$latitude, lon_era5 = j$longitude, elevacion = j$elevation)
    write.csv(d, destino, row.names = FALSE)
    Sys.sleep(15)                                 # pausa entre pedidos
  }
  cat("Celdas en caché:", sum(file.exists(file.path(dir_t, paste0(celdas$id, ".csv")))), "de", nrow(celdas), "\n")
}
