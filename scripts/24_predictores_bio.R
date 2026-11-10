# PARTE B: predictores biológicos por región y año, en dos versiones: ponderada por población (principal,
# WorldPop 2020 en la grilla de 0,1°) y ponderada por área (comparación).
#   B1 temp_ea         temperatura media ene–ago (ERA5-Land, °C)
#   B2 aptitud_termica R0 relativo de Mordecai 2017 aplicado a cada celda-mes ene–ago y luego promediado
#      aptitud_termica_ea4  igual, pero solo ene–abr (información disponible en la SE 18, para el pronóstico)
#   B3 sequia          −SPEI-4 de abril (lluvia CHIRPS v3 − ETP Thornthwaite), referencia 1991–2020
#   B4 reemergencia    nacional (17_reemergencia.R), igual para todas las regiones
# Se calcula 2014–2026; los modelos usan 2015–2025 y 2026 no se predice.
# Thornthwaite y no Hargreaves: el ERA5-Land mensual solo trae la temperatura media mensual (no Tmax/Tmin
# mensuales), que es lo único que Thornthwaite necesita además de la latitud.

library(terra)
library(sf)
library(exactextractr)
library(SPEI)

f_t2m <- Sys.getenv("ERA5_T2M", "datos/procesados/era5land_t2m_0p1_mensual.tif")
salida <- Sys.getenv("SALIDA_BIO", "datos/procesados/predictores_bio.csv")
anios <- 2014:2026

pob    <- rast("datos/procesados/poblacion_grilla_0p1.tif")
area   <- cellSize(pob, unit = "km")
reg    <- st_read("datos/regiones_rectoras.gpkg", quiet = TRUE)
t2m    <- rast(f_t2m)
lluvia <- rast("datos/procesados/chirps_v3_0p1_mensual.tif")
stopifnot(compareGeom(t2m, pob), compareGeom(lluvia, pob))
meses <- format(seq(as.Date("1991-01-01"), as.Date("2026-08-01"), by = "month"), "%Y-%m")
stopifnot(identical(names(t2m), meses), identical(names(lluvia), meses))

curva <- readRDS("modelo/resultados/mordecai_curva_r0.rds")
par <- curva$par; R0_max <- curva$R0_max
source("scripts/funciones_r0.R")

# Agregación por región: media ponderada por población y por área de celda (exactextractr, fracción cubierta)
por_region <- function(r, nombre) {
  wp <- exact_extract(r, reg, "weighted_mean", weights = pob, progress = FALSE)
  wa <- exact_extract(r, reg, "weighted_mean", weights = area, progress = FALSE)
  arm <- function(m, ver) {
    m <- as.matrix(m); colnames(m) <- names(r)
    data.frame(region = rep(reg$region, ncol(m)), anio = rep(as.integer(names(r)), each = nrow(m)),
               valor = as.vector(m), version = ver)
  }
  d <- rbind(arm(wp, "pob"), arm(wa, "area"))
  d$variable <- nombre
  d
}

# B1 y B2: meses ene–ago de cada año; R0 relativo por celda-mes antes de promediar
temp_ea <- apt_ea <- apt_ea4 <- list()
for (a in anios) {
  capas <- sprintf("%d-%02d", a, 1:8)
  x <- t2m[[capas]]
  temp_ea[[as.character(a)]] <- mean(x)
  r0x <- app(x, r0_relativo)
  apt_ea[[as.character(a)]]  <- mean(r0x)
  apt_ea4[[as.character(a)]] <- mean(r0x[[1:4]])
}
temp_ea <- rast(temp_ea); apt_ea <- rast(apt_ea); apt_ea4 <- rast(apt_ea4)

# B3: ETP Thornthwaite por celda con su latitud, balance = lluvia − ETP, SPEI-4, abril de cada año
lat <- init(pob, "y")
v_t <- values(t2m); v_p <- values(lluvia); v_lat <- values(lat)[, 1]
celdas <- which(rowSums(is.na(v_t)) == 0 & rowSums(is.na(v_p)) == 0)
abril <- match(sprintf("%d-04", anios), meses)
spei_abr <- matrix(NA_real_, ncell(pob), length(anios))
for (i in celdas) {
  tt <- ts(v_t[i, ], start = c(1991, 1), frequency = 12)
  etp <- as.numeric(thornthwaite(tt, lat = v_lat[i], verbose = FALSE))
  bal <- ts(v_p[i, ] - etp, start = c(1991, 1), frequency = 12)
  s <- spei(bal, scale = 4, ref.start = c(1991, 1), ref.end = c(2020, 12), verbose = FALSE)
  spei_abr[i, ] <- as.numeric(s$fitted)[abril]
}
sequia <- rast(pob, nlyrs = length(anios)); values(sequia) <- -spei_abr

# Cobertura: población de cada región en celdas sin temperatura o sin lluvia (costa: la interpolación
# bilineal de ERA5-Land da NA si algún punto vecino es mar). Esas celdas quedan fuera de las medias.
sin_dato <- pob; values(sin_dato) <- ifelse(seq_len(ncell(pob)) %in% celdas, 0, 1)
cob <- data.frame(region = reg$region,
                  pob_total = exact_extract(pob, reg, "sum", progress = FALSE),
                  pob_sin_dato = exact_extract(pob * sin_dato, reg, "sum", progress = FALSE))
cob$prop_pob_sin_dato <- cob$pob_sin_dato / cob$pob_total
print(cob, digits = 3)
write.csv(cob, "modelo/resultados/cobertura_clima_regiones.csv", row.names = FALSE)
names(temp_ea) <- names(apt_ea) <- names(apt_ea4) <- names(sequia) <- anios
cat("Celdas con temperatura y lluvia completas:", length(celdas), "\n")

largo <- rbind(por_region(temp_ea, "temp_ea"), por_region(apt_ea, "aptitud_termica"),
               por_region(apt_ea4, "aptitud_termica_ea4"),
               por_region(sequia, "sequia"))
largo$columna <- paste0(largo$variable, "_", largo$version)
bio <- reshape(largo[, c("region", "anio", "columna", "valor")], idvar = c("region", "anio"),
               timevar = "columna", direction = "wide")
names(bio) <- sub("^valor\\.", "", names(bio))

reem <- read.csv("modelo/resultados/reemergencia_anual.csv")
bio <- merge(bio, reem[, c("anio", "reemergencia")], by = "anio", all.x = TRUE)
bio <- bio[order(bio$region, bio$anio), c("region", "anio",
           "temp_ea_pob", "temp_ea_area", "aptitud_termica_pob", "aptitud_termica_area",
           "aptitud_termica_ea4_pob", "aptitud_termica_ea4_area",
           "sequia_pob", "sequia_area", "reemergencia")]
bio$fuente <- paste("ERA5-Land mensual (CDS); CHIRPS v3 mensual (IRI); Mordecai 2017 R0(T);",
                    "SPEI-4 abril Thornthwaite ref. 1991-2020; WorldPop 2020; reemergencia OPS/INCIENSA/MS")
stopifnot(!anyNA(bio[bio$anio %in% 2015:2025, 3:11]), nrow(bio[bio$anio %in% 2015:2025, ]) == 99)
write.csv(bio, salida, row.names = FALSE, fileEncoding = "UTF-8")
cat("predictores_bio:", nrow(bio), "filas ->", salida, "\n")
print(aggregate(cbind(temp_ea_pob, aptitud_termica_pob, sequia_pob) ~ region, bio, range), digits = 3)
