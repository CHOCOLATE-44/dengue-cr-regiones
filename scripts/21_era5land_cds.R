# PARTE B1: temperatura media mensual ERA5-Land desde Copernicus CDS (reemplaza a Open-Meteo, que no se usa).
# Dataset reanalysis-era5-land-monthly-means, monthly_averaged_reanalysis, 2m_temperature, 1991-01 a 2026-08,
# área N 11.3, W -86, S 8, E -82.5. Un pedido por año. La clave se lee del llavero con ecmwfr::wf_get_key();
# no se escribe en ningún archivo. Cita: Muñoz Sabater J (2019), ERA5-Land monthly averaged data from 1950
# to present, C3S CDS (ver auditoria.md).

library(ecmwfr)
library(terra)

dir_e <- "datos/fuentes/era5land_cds"
dir.create(dir_e, showWarnings = FALSE, recursive = TRUE)

clave <- try(wf_get_key(), silent = TRUE)
if (inherits(clave, "try-error") || !nzchar(clave))
  stop("No hay clave del CDS en el llavero. Guardarla una vez con ecmwfr::wf_set_key() (sin escribirla en archivos).")
rm(clave)

anios <- 1991:2026
for (a in anios) {
  destino <- file.path(dir_e, paste0("era5land_t2m_", a, ".nc"))
  if (file.exists(destino) && file.size(destino) > 0) next
  meses <- if (a == 2026) sprintf("%02d", 1:8) else sprintf("%02d", 1:12)
  pedido <- list(dataset_short_name = "reanalysis-era5-land-monthly-means",
                 product_type = "monthly_averaged_reanalysis", variable = "2m_temperature",
                 year = as.character(a), month = meses, time = "00:00",
                 area = c(11.3, -86, 8, -82.5), data_format = "netcdf", download_format = "unarchived",
                 target = basename(destino))
  wf_request(pedido, path = dir_e, transfer = TRUE, verbose = FALSE)
  cat("ERA5-Land", a, "->", destino, "\n")
}

# Pila mensual, Kelvin -> °C, bilineal a la grilla de 0,1° de la población
plantilla <- rast("datos/procesados/poblacion_grilla_0p1.tif")
capas <- lapply(anios, function(a) rast(file.path(dir_e, paste0("era5land_t2m_", a, ".nc"))))
t2m <- rast(capas)
fechas <- as.Date(time(t2m))
stopifnot(!anyNA(fechas), !anyDuplicated(fechas))
orden <- order(fechas); t2m <- t2m[[orden]]; fechas <- fechas[orden]
esperadas <- seq(as.Date("1991-01-01"), as.Date("2026-08-01"), by = "month")
faltan <- setdiff(format(esperadas, "%Y-%m"), format(fechas, "%Y-%m"))
if (length(faltan)) stop("ERA5-Land: faltan meses ", paste(faltan, collapse = ", "))
cat("Unidad declarada en el NetCDF:", paste(unique(units(t2m)), collapse = ", "), "\n")
t2m_c <- resample(t2m, plantilla, method = "bilinear") - 273.15
names(t2m_c) <- format(fechas, "%Y-%m")
writeRaster(t2m_c, "datos/procesados/era5land_t2m_0p1_mensual.tif", overwrite = TRUE)
cat("ERA5-Land a 0,1°:", nlyr(t2m_c), "meses; rango °C:", paste(round(range(values(t2m_c), na.rm = TRUE), 2), collapse = " a "), "\n")
