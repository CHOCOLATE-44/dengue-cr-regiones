# Sección 16, fase A: casos por región rectora y período de 4 semanas epidemiológicas, 2015–2026.
# Período k = SE 4k−3 a 4k (k = 1…13); la SE 53 se suma al período 13.
# 2015–2020: semanas regionales del ASIS (hoja "Regiones MinSalud" del Excel protegido, solo lectura).
# 2021–2026: acumulado regional del boletín de cierre del período menos el del cierre anterior.
# Salidas en semanal/resultados/: s45_calidad.csv (nombre pedido en el preregistro), s49_cortes.csv,
# s49_series_periodo.csv y s49_parada.csv.
suppressMessages(library(readxl))
dir.create("semanal/resultados", recursive = TRUE, showWarnings = FALSE)
src <- "datos/fuentes/dengue_semanal_CR_2015-2026.xlsx"
reg <- c("Brunca", "C.Este", "Central Norte", "Central Sur", "Chorotega", "H. Caribe", "H.Norte", "Occidente", "Pacífico Central")
periodo_de <- function(se) pmin(ceiling(se / 4), 13)

# ---- Serie nacional de la OPS por semana (para cierres y escalamiento) ----
s <- read_excel(src, sheet = "Serie semanal")[, 1:3]; names(s) <- c("anio", "se", "ops")
s <- as.data.frame(s); s$periodo <- periodo_de(s$se)
ultima_se <- tapply(s$se, s$anio, max)
ops_per <- aggregate(ops ~ anio + periodo, s, sum)
ops_nse <- aggregate(se ~ anio + periodo, s, length); names(ops_nse)[3] <- "semanas_ops"
ops_per <- merge(ops_per, ops_nse)

# ---- 2015–2020: ASIS ----
g <- as.data.frame(read_excel(src, sheet = "Regiones MinSalud"))[, 1:4]; names(g) <- c("anio", "se", "region", "casos")
stopifnot(setequal(unique(g$region), reg))
g$periodo <- periodo_de(g$se)
asis <- aggregate(casos ~ anio + periodo + region, g, sum)
asis_nse <- aggregate(se ~ anio + periodo, unique(g[, c("anio", "periodo", "se")]), length); names(asis_nse)[3] <- "semanas_fuente"
asis <- merge(asis, asis_nse)
asis$semanas_esperadas <- ifelse(asis$periodo == 13, ultima_se[as.character(asis$anio)] - 48, 4)
asis$tipo_fuente <- "ASIS"; asis$se_cierre_objetivo <- NA; asis$se_cierre_usada <- NA; asis$aprox_cierre <- FALSE; asis$aprox_inicio <- FALSE
asis$fuente_cierre <- paste("Excel ASIS", asis$anio)
asis$faltante <- asis$semanas_fuente < asis$semanas_esperadas
# períodos sin ninguna semana en el ASIS (2019: SE 47–52; 2020: SE 45–53)
falt <- do.call(rbind, lapply(2015:2020, function(a) {
  k <- setdiff(1:13, asis$periodo[asis$anio == a]); if (!length(k)) return(NULL)
  expand.grid(anio = a, periodo = k, region = reg, stringsAsFactors = FALSE)
}))
if (!is.null(falt)) {
  falt$casos <- NA; falt$semanas_fuente <- 0; falt$semanas_esperadas <- ifelse(falt$periodo == 13, ultima_se[as.character(falt$anio)] - 48, 4)
  falt$tipo_fuente <- "ASIS"; falt$se_cierre_objetivo <- NA; falt$se_cierre_usada <- NA; falt$aprox_cierre <- FALSE; falt$aprox_inicio <- FALSE
  falt$fuente_cierre <- "sin dato"; falt$faltante <- TRUE
  asis <- rbind(asis, falt[, names(asis)])
}
asis$casos_crudos <- asis$casos; asis$casos <- NULL

# ---- 2021–2026: boletines ----
b <- read.csv("datos/procesados/boletines_regiones_2021-2026.csv")
b <- b[b$nivel == "region" & b$usar_en_analisis & !is.na(b$se_corte), ]
b$id <- paste(b$url, b$se_corte)
completos <- tapply(b$unidad, b$id, function(u) setequal(u, reg) && length(u) == 9)
b <- b[b$id %in% names(completos)[completos], ]
cab <- unique(b[, c("id", "anio_datos", "se_corte", "numero", "tipo_boletin", "fecha_publicacion")])
# boletín de cierre: el de la semana objetivo; si falta, el más cercano (en empate la semana posterior,
# y con la misma semana el boletín publicado después), igual que en la serie anual (06_construir_bloques.py)
cierre <- function(a, objetivo) {
  x <- cab[cab$anio_datos == a, ]
  if (!nrow(x)) return(NULL)
  o <- order(abs(x$se_corte - objetivo), -x$se_corte, rev(rank(x$fecha_publicacion, ties.method = "first")), -suppressWarnings(as.integer(x$numero)))
  x[o[1], ]
}
cortes <- do.call(rbind, lapply(2021:2026, function(a) {
  ult <- if (a == 2026) max(cab$se_corte[cab$anio_datos == 2026]) else ultima_se[as.character(a)]
  obj <- c(4 * (1:12), ult); obj <- obj[obj <= ult]
  do.call(rbind, lapply(seq_along(obj), function(k) {
    x <- cierre(a, obj[k])
    data.frame(anio = a, periodo = k, se_cierre_objetivo = obj[k], se_cierre_usada = x$se_corte, id = x$id,
               fuente_cierre = paste0("Boletín N.º ", x$numero, " (", x$tipo_boletin, ", SE ", x$se_corte, ")"))
  }))
}))
# en 2026 solo hay períodos completos hasta el último boletín
cortes <- cortes[!(cortes$anio == 2026 & cortes$se_cierre_objetivo %% 4 != 0), ]
cortes$aprox_cierre <- cortes$se_cierre_usada != cortes$se_cierre_objetivo
cortes$repetido <- duplicated(cortes[, c("anio", "id")])   # el mismo boletín cierra dos períodos: el segundo queda sin dato
write.csv(cortes[, setdiff(names(cortes), "id")], "semanal/resultados/s49_cortes.csv", row.names = FALSE)

acum <- b[, c("id", "unidad", "casos_acum")]; names(acum)[2] <- "region"
bol <- merge(cortes, acum, by = "id")
bol <- bol[order(bol$anio, bol$region, bol$periodo), ]
bol$acum_prev <- ave(bol$casos_acum, bol$anio, bol$region, FUN = function(v) c(0, head(v, -1)))
bol$casos_crudos <- bol$casos_acum - bol$acum_prev
bol$aprox_inicio <- ave(bol$aprox_cierre, bol$anio, bol$region, FUN = function(v) c(FALSE, head(v, -1)))
bol$faltante <- bol$repetido
bol$casos_crudos[bol$faltante] <- NA
bol$tipo_fuente <- "boletín"; bol$semanas_fuente <- NA
bol$semanas_esperadas <- ifelse(bol$periodo == 13, bol$se_cierre_objetivo - 48, 4)
cols <- c("anio", "periodo", "region", "tipo_fuente", "fuente_cierre", "se_cierre_objetivo", "se_cierre_usada", "aprox_cierre",
          "aprox_inicio", "faltante", "semanas_fuente", "semanas_esperadas", "casos_crudos")
p <- rbind(asis[, cols], bol[, cols])
p$negativo <- !is.na(p$casos_crudos) & p$casos_crudos < 0

# ---- Escalamiento a la OPS (negativos a cero ANTES de las proporciones) ----
p$casos_cero <- pmax(p$casos_crudos, 0)
tot <- aggregate(casos_cero ~ anio + periodo, p, sum); names(tot)[3] <- "suma_regiones"
p <- merge(p, tot, all.x = TRUE); p <- merge(p, ops_per[, c("anio", "periodo", "ops")], all.x = TRUE)
p$casos <- ifelse(p$suma_regiones > 0, p$casos_cero / p$suma_regiones * p$ops, 0)
p$casos[is.na(p$casos_cero)] <- NA
p <- p[order(p$anio, p$periodo, p$region), ]
write.csv(p, "semanal/resultados/s49_series_periodo.csv", row.names = FALSE)

# ---- Calidad por región y año ----
cal <- aggregate(cbind(periodos = 1, negativos = negativo, aprox_cierre = aprox_cierre, aprox_inicio = aprox_inicio,
                       faltantes = faltante) ~ anio + region, p, sum)
cal <- cal[order(cal$anio, cal$region), ]
write.csv(cal, "semanal/resultados/s45_calidad.csv", row.names = FALSE)

# ---- Criterio de parada (preregistro, fase A.6): región × período de 2021–2025 ----
q <- p[p$anio %in% 2021:2025, ]
par <- data.frame(
  criterio = c("preregistrado: negativo o boletín de cierre aproximado", "estricto: negativo o cierre o inicio aproximado",
               "solo negativos", "solo cierre aproximado", "faltantes"),
  celdas = nrow(q),
  marcadas = c(sum(q$negativo | q$aprox_cierre), sum(q$negativo | q$aprox_cierre | q$aprox_inicio), sum(q$negativo),
               sum(q$aprox_cierre), sum(q$faltante)))
par$proporcion <- round(par$marcadas / par$celdas, 4)
par$umbral <- 0.10; par$supera <- par$proporcion > par$umbral
write.csv(par, "semanal/resultados/s49_parada.csv", row.names = FALSE)
print(par)
cat(if (par$supera[1]) "PARADA: se supera el 10 % preregistrado; no se ajustan modelos.\n" else "Pasa la fase A.\n")
