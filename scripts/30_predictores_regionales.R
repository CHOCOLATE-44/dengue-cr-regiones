# Hipótesis regionales H1–H4: predictores nuevos (definiciones fijas, decididas antes de ver resultados).
#   H1 inmunidad3       log(1 + tasa acumulada por 100 000 de la región en los años completos a−3, a−2 y a−1)
#   H2 vecinos_arrastre log(1 + media ponderada por población de la tasa SE 41–cierre de a−1 de las regiones vecinas)
#      vecinos_seco     log(1 + media ponderada por población de la tasa SE 1–18 de a de las vecinas; bloque seco escalado)
#   H3 caribe, H4 chorotega: indicadoras de región (las interacciones se arman en 31)
# Requiere scripts/12_cv_loyo.R cargado (preparar_datos). Salidas: datos/procesados/predictores_regionales.csv,
# datos/vecindad_regiones.csv, datos/procesados/casos_anuales_region_2012-2024.csv.

library(sf)

REG <- c("Brunca", "C.Este", "Central Norte", "Central Sur", "Chorotega", "H. Caribe", "H.Norte", "Occidente", "Pacífico Central")
nombres <- c("Pacífico Central" = "Pacífico Central", "Chorotega" = "Chorotega", "Central Norte" = "Central Norte",
             "H. Caribe" = "H. Caribe", "Huetar Caribe" = "H. Caribe", "Central Sur" = "Central Sur", "Brunca" = "Brunca",
             "C.Este" = "C.Este", "Central Este" = "C.Este", "Occidente" = "Occidente", "H.Norte" = "H.Norte",
             "Huetar Norte" = "H.Norte")

# -----------------------------------------------------------------------------
# H1. Casos anuales (crudos) por región, años completos 2012–2024
# -----------------------------------------------------------------------------
# 2012–2018: Excel ASIS, hoja "Region" (casos por semana y región). Se verifica la suma regional contra la fila
# "Total" de la hoja, la columna "Total" y el total del año en la hoja "Resumen".
# readxl (libxls) desalinea la hoja Region de 2013 (las columnas 1–9 quedan corridas 5 filas respecto de las
# 10–11); LibreOffice la lee bien. Por eso todas las hojas del ASIS se convierten a CSV con LibreOffice
# (soffice --headless) en una carpeta temporal y se leen de ahí, igual para todos los años.
hoja_asis <- function(a, hoja) {
  dir_t <- file.path(tempdir(), paste0("asis_", a))
  if (!dir.exists(dir_t)) {
    dir.create(dir_t)
    file.copy(sprintf("datos/fuentes/asis/dengue_%d.xls", a), dir_t)
    system2("env", c("-u", "LD_LIBRARY_PATH", "soffice", "--headless", "--convert-to",   # R fija LD_LIBRARY_PATH y rompe soffice
            shQuote("csv:Text - txt - csv (StarCalc):44,34,76,1,,0,false,true,false,false,false,-1"),
            "--outdir", shQuote(dir_t), shQuote(file.path(dir_t, sprintf("dengue_%d.xls", a)))), stdout = FALSE, stderr = FALSE)
  }
  f <- file.path(dir_t, sprintf("dengue_%d-%s.csv", a, hoja))
  stopifnot(file.exists(f))
  read.csv(f, header = FALSE, colClasses = "character", fileEncoding = "UTF-8", check.names = FALSE)
}
num_lo <- function(v) as.numeric(gsub(",", ".", trimws(v)))   # LibreOffice exporta con coma decimal
asis_region <- function(a) {
  x <- hoja_asis(a, "Region")
  h <- which(trimws(x[[1]]) == "Semana")[1]
  enc <- trimws(as.character(unlist(x[h, ])))
  sem <- x[(h + 1):nrow(x), ]; sem <- sem[grepl("^[0-9]+$", trimws(sem[[1]])), ]
  num <- function(v) { v <- num_lo(v); v[is.na(v)] <- 0; v }   # celda vacía = 0 casos
  cols <- which(enc %in% names(nombres))
  casos <- sapply(cols, function(j) sum(num(sem[[j]])))
  names(casos) <- nombres[enc[cols]]
  stopifnot(setequal(names(casos), REG))
  fila_total <- x[which(trimws(x[[1]]) == "Total")[1], ]
  total_fila <- sapply(cols, function(j) num(fila_total[[j]]))
  total_col  <- sum(num(sem[[which(enc == "Total")]]))
  res <- hoja_asis(a, "Resumen")
  # fila del año en "Resumen" (la columna del año no es la misma en todos los archivos): el valor siguiente es el total
  pos <- which(matrix(grepl(paste0("^", a, "\\*?$"), trimws(as.matrix(res))), nrow(res)), arr.ind = TRUE)[1, ]
  total_resumen <- num_lo(res[pos[1], pos[2] + 1])
  list(casos = casos[REG], chequeo = data.frame(anio = a, fuente = paste0("Excel ASIS ", a, ", hoja Region"),
       semanas = nrow(sem), suma_regiones = sum(casos), total_fila_hoja = sum(total_fila),
       regiones_coinciden_fila_total = all(casos == total_fila), total_columna = total_col, total_resumen = total_resumen))
}
asis <- lapply(2012:2018, asis_region)
chequeo <- do.call(rbind, lapply(asis, `[[`, "chequeo"))

# 2019: boletín N°1 de 2020, Cuadro 1 (cierre a la SE 52)
t19 <- system2("pdftotext", c("-layout", "datos/fuentes/extras/2020_semanal_1.pdf", "-"), stdout = TRUE)
t19 <- paste(t19, collapse = "\n"); t19 <- substr(t19, regexpr("Cuadro 1", t19), nchar(t19))
n2020 <- c("Central Norte" = "Central Norte", "Huetar Caribe" = "H. Caribe", "Pacífico Central" = "Pacífico Central",
           "Chorotega" = "Chorotega", "Central Este" = "C.Este", "Central Sur" = "Central Sur", "Huetar Norte" = "H.Norte",
           "Brunca" = "Brunca", "Occidente" = "Occidente")
c19 <- sapply(names(n2020), function(n) as.numeric(regmatches(t19, regexec(paste0(n, "\\s+(\\d+)\\s+(\\d+)\\s"), t19))[[1]][3]))
names(c19) <- n2020[names(n2020)]
tot19 <- as.numeric(regmatches(t19, regexec("TOTAL\\s+\\d+\\s+(\\d+)", t19))[[1]][2])
chequeo <- rbind(chequeo, data.frame(anio = 2019, fuente = "Boletín N°1 de 2020, Cuadro 1 (SE 52)", semanas = 52,
  suma_regiones = sum(c19), total_fila_hoja = tot19, regiones_coinciden_fila_total = sum(c19) == tot19,
  total_columna = NA, total_resumen = NA))

# 2020–2024: último boletín con las 9 regiones y corte en la SE 52 o 53 del año (como 06c_construir_predictores.py)
bol <- read.csv("datos/procesados/boletines_regiones_2021-2026.csv", fileEncoding = "UTF-8-BOM")
bol <- subset(bol, nivel == "region" & usar_en_analisis & !is.na(se_corte) & se_corte >= 52)
cierre <- function(a) {
  x <- subset(bol, anio_datos == a)
  g <- split(x, paste(x$url, x$numero, x$tipo_boletin))
  g <- g[sapply(g, function(q) setequal(q$unidad, REG) && nrow(q) == 9)]
  orden <- order(sapply(g, function(q) q$fecha_publicacion[1]), sapply(g, function(q) as.integer(q$numero[1])))
  q <- g[[orden[length(orden)]]]
  list(casos = setNames(q$casos_acum, q$unidad)[REG],
       chequeo = data.frame(anio = a, fuente = sprintf("Boletín %s N°%s de %d (SE %d), p. %d", q$tipo_boletin[1], q$numero[1],
                                                       q$anio[1], q$se_corte[1], q$pagina_pdf[1]),
                            semanas = q$se_corte[1], suma_regiones = sum(q$casos_acum), total_fila_hoja = q$total_tabla[1],
                            regiones_coinciden_fila_total = sum(q$casos_acum) == q$total_tabla[1],
                            total_columna = NA, total_resumen = NA))
}
bols <- lapply(2020:2024, cierre)
chequeo <- rbind(chequeo, do.call(rbind, lapply(bols, `[[`, "chequeo")))
print(chequeo)
write.csv(chequeo, "modelo/resultados/h1_chequeo_totales_anuales.csv", row.names = FALSE)

casos_anuales <- rbind(
  do.call(rbind, lapply(seq_along(asis), function(i) data.frame(anio = 2011 + i, region = REG, casos = asis[[i]]$casos))),
  data.frame(anio = 2019, region = REG, casos = c19[REG]),
  do.call(rbind, lapply(seq_along(bols), function(i) data.frame(anio = 2019 + i, region = REG, casos = bols[[i]]$casos))))
casos_anuales$fuente <- chequeo$fuente[match(casos_anuales$anio, chequeo$anio)]

# Población: 2015–2024 la del modelo (bloques; Occidente 2018 interpolado); 2012 implícita en las tasas del ASIS
# 2012 (hoja "Regiones": casos / tasa × 100 000); 2013 y 2014 sin tasas regionales en el ASIS: interpolación
# lineal entre 2012 y 2015, marcada.
d_mod <- preparar_datos()
pob <- unique(d_mod[, c("anio", "region", "poblacion")]); pob$poblacion_fuente <- "modelo (bloques_region_anio)"
r12 <- hoja_asis(2012, "Regiones")
r12 <- r12[trimws(r12[[1]]) %in% names(nombres), 1:3]
p12 <- data.frame(anio = 2012, region = unname(nombres[trimws(r12[[1]])]),
                  poblacion = num_lo(r12[[2]]) / num_lo(r12[[3]]) * 1e5,
                  poblacion_fuente = "ASIS 2012, hoja Regiones (casos / tasa)")
stopifnot(setequal(p12$region, REG))
stopifnot(all(p12$region == p12$region), all(abs(num_lo(r12[[2]]) - asis[[1]]$casos[p12$region]) == 0))
p15 <- subset(pob, anio == 2015)
p1314 <- do.call(rbind, lapply(REG, function(r) {
  y <- approx(c(2012, 2015), c(p12$poblacion[p12$region == r], p15$poblacion[p15$region == r]), xout = 2013:2014)$y
  data.frame(anio = 2013:2014, region = r, poblacion = y, poblacion_fuente = "interpolada 2012–2015 (sin tasas en ASIS)")
}))
pob <- rbind(p12, p1314, pob)
casos_anuales <- merge(casos_anuales, pob, by = c("anio", "region"))
casos_anuales$tasa <- 1e5 * casos_anuales$casos / casos_anuales$poblacion
write.csv(casos_anuales[order(casos_anuales$anio, casos_anuales$region), ],
          "datos/procesados/casos_anuales_region_2012-2024.csv", row.names = FALSE, fileEncoding = "UTF-8")

# inmunidad3: suma de las tasas anuales de a−3, a−2 y a−1 (tasa acumulada por 100 000)
inm <- do.call(rbind, lapply(2015:2025, function(a) {
  x <- subset(casos_anuales, anio %in% (a - 3):(a - 1))
  stopifnot(all(table(x$region) == 3))
  s <- tapply(x$tasa, x$region, sum)
  data.frame(anio = a, region = names(s), inmunidad3 = log1p(as.numeric(s)),
             inmunidad3_fuente = paste0("años ", a - 3, "–", a - 1, "; población: ",
                                        paste(unique(x$poblacion_fuente), collapse = " / ")))
}))

# -----------------------------------------------------------------------------
# H2. Vecindad (polígonos que comparten borde) y presión de vecinas
# -----------------------------------------------------------------------------
reg_sf <- st_read("datos/regiones_rectoras.gpkg", quiet = TRUE)
reg_sf <- reg_sf[match(REG, reg_sf$region), ]
toca <- st_touches(reg_sf, sparse = FALSE)
comparte <- st_relate(reg_sf, reg_sf, pattern = "F***1****", sparse = FALSE)   # frontera de dimensión 1 (no solo un punto)
dimnames(toca) <- dimnames(comparte) <- list(REG, REG)
vec <- toca * 1
write.csv(data.frame(region = REG, vec, check.names = FALSE), "datos/vecindad_regiones.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("\nVecindad (st_touches):\n"); print(vec)
cat("Pares que se tocan solo en un punto:", sum(toca & !comparte) / 2, "\n")

# Tasa SE 41–cierre de a−1 = expm1(arrastre) (arrastre = log(1 + tasa), población de a−1; 2015 usa la de 2015)
pr <- subset(d_mod, anio <= 2025)[, c("anio", "region", "arrastre", "poblacion")]
pr$pob_arrastre <- pob$poblacion[match(paste(pr$anio - 1, pr$region), paste(pob$anio, pob$region))]
pr$pob_arrastre[pr$anio == 2015] <- pr$poblacion[pr$anio == 2015]
b <- read.csv("datos/procesados/bloques_region_anio.csv", fileEncoding = "UTF-8-BOM")
seco <- subset(b, bloque == "seco")[, c("anio", "region", "casos_escalados")]
pr <- merge(pr, seco, by = c("anio", "region"))
vecinos <- do.call(rbind, lapply(split(pr, pr$anio), function(x) {
  x <- x[match(REG, x$region), ]
  do.call(rbind, lapply(REG, function(r) {
    v <- REG[vec[r, ] == 1]; xv <- x[x$region %in% v, ]
    data.frame(anio = x$anio[1], region = r, n_vecinas = length(v),
               vecinos_arrastre = log1p(sum(expm1(xv$arrastre) * xv$pob_arrastre) / sum(xv$pob_arrastre)),
               vecinos_seco = log1p(1e5 * sum(xv$casos_escalados) / sum(xv$poblacion)))
  }))
}))

out <- merge(inm, vecinos, by = c("anio", "region"))
out$caribe <- as.integer(out$region == "H. Caribe"); out$chorotega <- as.integer(out$region == "Chorotega")
out <- out[order(out$anio, out$region), ]
stopifnot(nrow(out) == 99, !anyNA(out))
write.csv(out, "datos/procesados/predictores_regionales.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("predictores_regionales.csv:", nrow(out), "filas\n")
