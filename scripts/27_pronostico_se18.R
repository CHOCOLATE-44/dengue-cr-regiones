# Pronóstico desde la SE 18 (lista cerrada, evaluada una sola vez). Respuesta: casos de SE 19–40
# (bloque lluvioso) por región y año, con offset log(población). Solo información disponible en la SE 18:
#   tasa_seco            log(1 + tasa SE 1–18) de la misma región y año (bloque seco)
#   arrastre, oni        arrastre y RONI del año previo (sin cambios)
#   aptitud_termica_ea4  R0 relativo promedio ENE–ABR, ponderado por población (24_predictores_bio.R)
#   sequia               −SPEI-4 de abril, ponderado por población
# region + offset + (1 | anio_f), NB2, 2015–2025 (99 filas), estandarización dentro de cada pliegue.
# Sensibilidad: P1 con casos CRUDOS (respuesta y tasa_seco). 2026 no se usa ni se predice.
# Requiere scripts/12_cv_loyo.R y scripts/25_parte_c_predictores.R cargados.

library(glmmTMB)

set.seed(2026)
b <- read.csv("datos/procesados/bloques_region_anio.csv", fileEncoding = "UTF-8-BOM")
stopifnot(all(b$se_inicio[b$bloque == "seco"] == 1), all(b$se_fin[b$bloque == "seco"] == 18),
          all(b$se_inicio[b$bloque == "lluvioso"] == 19), all(b$se_fin[b$bloque == "lluvioso"] == 40))
bloque <- function(nombre, sufijo) {
  x <- b[b$bloque == nombre, c("anio", "region", "casos_escalados", "casos_crudos")]
  names(x)[3:4] <- paste0(c("esc_", "cru_"), sufijo); x
}
p <- datos_bio()[, c("anio", "region", "anio_f", "poblacion", "arrastre", "oni",
                     "aptitud_termica_ea4_pob", "aptitud_termica_ea4_area", "sequia_pob")]
p <- merge(merge(p, bloque("seco", "seco"), by = c("anio", "region")), bloque("lluvioso", "lluv"), by = c("anio", "region"))
p$region <- factor(p$region); p$anio_f <- factor(p$anio)
p <- p[order(p$anio, p$region), ]
stopifnot(nrow(p) == 99, !anyNA(p))

# Versión escalada (principal) o cruda (sensibilidad) de respuesta y tasa_seco
version_datos <- function(tipo) {
  x <- p
  x$casos  <- x[[paste0(tipo, "_lluv")]]
  x$tasa_seco <- log1p(1e5 * x[[paste0(tipo, "_seco")]] / x$poblacion)
  x$y_log  <- log1p(1e5 * x$casos / x$poblacion)
  x
}
dif <- subset(p, esc_seco != cru_seco | esc_lluv != cru_lluv)
cat("Filas donde escalados y crudos difieren:\n"); print(dif[, c("anio", "region", "esc_seco", "cru_seco", "esc_lluv", "cru_lluv")])

comun <- "region + (1 | anio_f) + offset(log(poblacion))"
modelos <- list(
  P00   = character(),
  P0    = c("arrastre", "oni", "aptitud_termica_ea4_pob", "sequia_pob"),
  P1    = c("arrastre", "oni", "aptitud_termica_ea4_pob", "sequia_pob", "tasa_seco"),
  P1min = c("arrastre", "tasa_seco"))
formula_p <- function(v) as.formula(paste("casos ~", paste(c(if (length(v)) paste0(v, "_z"), comun), collapse = " + ")))
estandarizar <- function(ent, pru, vars) {
  for (v in vars) { m <- mean(ent[[v]]); s <- sd(ent[[v]])
    ent[[paste0(v, "_z")]] <- (ent[[v]] - m) / s; pru[[paste0(v, "_z")]] <- (pru[[v]] - m) / s }
  list(ent = ent, pru = pru)
}
cv_p <- function(datos, vars, B = 5000) {
  res <- lapply(2015:2025, function(a) {
    z <- estandarizar(subset(datos, anio != a), subset(datos, anio == a), vars)
    ent <- droplevels(z$ent); ent$region <- factor(ent$region, levels = levels(datos$region))
    mod <- ajustar(formula_p(vars), ent)
    sl  <- log1p(1e5 * simular(mod, z$pru, B) / z$pru$poblacion)
    list(pred = data.frame(anio = a, region = z$pru$region, y_log = z$pru$y_log, media_pred = rowMeans(sl),
                           ip_inf = apply(sl, 1, quantile, 0.025), ip_sup = apply(sl, 1, quantile, 0.975),
                           crps = scoringRules::crps_sample(z$pru$y_log, sl)),
         coef = data.frame(anio_fuera = a, termino = names(fixef(mod)$cond), estimado = fixef(mod)$cond),
         diag = data.frame(anio_fuera = a, sigma_a = sigma_a(mod), pdHess = isTRUE(mod$sdr$pdHess)))
  })
  lapply(c(pred = "pred", coef = "coef", diag = "diag"), function(k) do.call(rbind, lapply(res, `[[`, k)))
}

evaluar <- function(datos, lista, etiqueta) {
  completo <- estandarizar(datos, datos, unique(unlist(lista)))$ent
  aj  <- lapply(lista, function(v) ajustar(formula_p(v), completo))
  cvs <- lapply(lista, function(v) cv_p(datos, v))
  m0  <- ajustar(formula_p(character()), completo)            # referencia del pseudo R² (P00)
  t <- do.call(rbind, lapply(names(lista), function(k) {
    m <- aj[[k]]; dg <- cvs[[k]]$diag
    cbind(data.frame(version = etiqueta, modelo = k, evaluacion = "LOYO", pseudo_r2_anio = pseudo_r2_anio(m, m0)),
          metricas_loyo(cvs[[k]]$pred),
          data.frame(sigma_a = sigma_a(m), AIC = AIC(m), pdHess = isTRUE(m$sdr$pdHess),
                     E1_pliegues_sigma_a_menor_005 = sum(dg$sigma_a < 0.05), E1_pliegues_sin_pdHess = sum(!dg$pdHess)))
  }))
  e2 <- do.call(rbind, lapply(names(lista)[lengths(lista) > 0], function(k) {
    cf <- subset(cvs[[k]]$coef, grepl("_z$", termino))
    do.call(rbind, lapply(split(cf, cf$termino), function(x)
      data.frame(version = etiqueta, modelo = k, termino = x$termino[1], completo = fixef(aj[[k]])$cond[[x$termino[1]]],
                 se_completo = summary(aj[[k]])$coefficients$cond[x$termino[1], "Std. Error"],
                 min = min(x$estimado), max = max(x$estimado), cambia_signo = min(x$estimado) < 0 && max(x$estimado) > 0)))
  }))
  t$E1_aparecio <- !t$pdHess | t$sigma_a < 0.05 | t$E1_pliegues_sigma_a_menor_005 > 0 | t$E1_pliegues_sin_pdHess > 0
  t$E2_aparecio <- sapply(t$modelo, function(k) any(e2$cambia_signo[e2$modelo == k]))
  t$E2_aparecio[t$modelo == "P00"] <- NA
  pred <- do.call(rbind, lapply(names(cvs), function(k) cbind(version = etiqueta, modelo = k, cvs[[k]]$pred)))
  list(tabla = t, e2 = e2, pred = pred)
}

esc <- version_datos("esc")
r_esc <- evaluar(esc, modelos, "escalados")
techo <- ajustar(casos ~ region + anio_f + offset(log(poblacion)), esc)
yh <- log1p(1e5 * fitted(techo) / esc$poblacion)
yc <- esc$y_log - ave(esc$y_log, esc$region); yhc <- yh - ave(yh, esc$region)
fila_techo <- data.frame(version = "escalados", modelo = "TechoP", evaluacion = "en la muestra", pseudo_r2_anio = NA,
  r2_loyo_total = 1 - sum((esc$y_log - yh)^2) / sum((esc$y_log - mean(esc$y_log))^2),
  r2_loyo_dentro_region = 1 - sum((yc - yhc)^2) / sum(yc^2), crps_medio = NA, cobertura_ip95 = NA,
  filas_evaluadas = nrow(esc), sigma_a = NA, AIC = AIC(techo), pdHess = isTRUE(techo$sdr$pdHess),
  E1_pliegues_sigma_a_menor_005 = NA, E1_pliegues_sin_pdHess = NA, E1_aparecio = NA, E2_aparecio = NA)

# Sensibilidad: P1 con casos crudos (pseudo R² respecto de P00 crudo). Decisión del usuario: SOLO en esta
# sensibilidad se quitan las filas con bloque crudo negativo (C.Este 2025: el acumulado crudo de la SE 40 es menor
# que el de la SE 18 por depuración). El análisis principal sigue con 99 filas.
cru <- version_datos("cru")
negativos <- subset(cru, cru_seco < 0 | cru_lluv < 0)[, c("anio", "region", "cru_seco", "cru_lluv")]
write.csv(negativos, "modelo/resultados/p_crudos_negativos.csv", row.names = FALSE)
cat("\nSensibilidad con crudos: se quitan", nrow(negativos), "fila(s) con bloque crudo negativo\n"); print(negativos)
cru <- subset(cru, cru_seco >= 0 & cru_lluv >= 0)
stopifnot(nrow(cru) == 98)
r_cru <- evaluar(cru, modelos[c("P00", "P1")], "crudos (98 filas, sin C.Este 2025)")
r_cru$tabla <- subset(r_cru$tabla, modelo == "P1")

tabla_p <- rbind(r_esc$tabla, fila_techo, r_cru$tabla)
for (u in c("060", "070")) {
  tabla_p[[paste0("pseudo_r2_ge_", u)]] <- tabla_p$pseudo_r2_anio >= as.numeric(u) / 100
  tabla_p[[paste0("r2_dentro_ge_", u)]] <- tabla_p$r2_loyo_dentro_region >= as.numeric(u) / 100
}
cat("\nTabla P (pronóstico desde la SE 18; TechoP en la muestra)\n"); print(tabla_p, digits = 3)
e2_p <- rbind(r_esc$e2, subset(r_cru$e2, modelo == "P1"))
print(e2_p, digits = 3)
write.csv(tabla_p, "modelo/resultados/tabla_p_pronostico_se18.csv", row.names = FALSE)
write.csv(e2_p, "modelo/resultados/p_estabilidad_coeficientes.csv", row.names = FALSE)
write.csv(rbind(r_esc$pred, subset(r_cru$pred, modelo == "P1")), "modelo/resultados/p_cv_loyo_predicciones.csv", row.names = FALSE)
write.csv(dif[, c("anio", "region", "esc_seco", "cru_seco", "esc_lluv", "cru_lluv")],
          "modelo/resultados/p_escalados_vs_crudos.csv", row.names = FALSE)
