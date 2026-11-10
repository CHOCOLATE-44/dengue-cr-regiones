# Sección 14, PARTE 2: evaluación de 2026 (una sola vez). GLMM NB2 (glmmTMB), sin nada bayesiano.
#   Ajuste con 2015–2025 (99 filas) y la versión elegida en 43_calibracion_reml.R (`version_2026`: REML o ML).
#   Principal: S = arrastre + aptitud_termica_pob (ene–ago). Sensibilidad: M1 = arrastre + oni (RONI previo) y M0.
#   Predictores 2026: arrastre (SE 41–cierre de 2025, predictores_region_anio.csv), aptitud térmica ene–ago 2026
#   (predictores_bio.csv) y RONI previo (estaciones JAS a NDJ de 2025, centradas de agosto a diciembre).
#   Estandarización con la media y la DE de 2015–2025. Extrapolación: valor 2026 fuera del rango 2015–2025 de su región.
#   Respuesta 2026: casos escalados SE 1–40 (bloques_region_anio.csv; Boletín N°39 con corte en la SE 40).
#   Predictiva: 20 000 sorteos, β ~ N(β̂, V̂), u ~ N(0, σ̂²_a) común a las regiones, y ~ BN2; el total nacional es la
#   suma por sorteo. Probabilidad a dos colas: min(1, 2·min(P(Y ≤ obs), P(Y ≥ obs))); Holm sobre las 9 regiones.
# Requiere 12_cv_loyo.R, 25_parte_c_predictores.R, 36_validacion_v2.R (formula_v2, continuas) y 43 (version_2026).
library(glmmTMB)
library(MASS)
library(ggplot2)

B26 <- 20000
indicadoras <- c("caribe", "chorotega")
modelos <- list(S = c("arrastre", "aptitud_termica_pob"), M1 = c("arrastre", "oni"), M0 = character())
reml <- identical(version_2026, "REML")

ent <- datos_bio(); ent$casos <- ent$casos_escalados
bio <- read.csv("datos/procesados/predictores_bio.csv", fileEncoding = "UTF-8")
n26 <- merge(subset(preparar_datos(), anio == 2026), subset(bio, anio == 2026)[, c("region", "aptitud_termica_pob")], by = "region")
n26$casos <- n26$casos_escalados
stopifnot(nrow(ent) == 99, nrow(n26) == 9, !anyNA(n26[, c("arrastre", "oni", "aptitud_termica_pob", "casos", "poblacion")]))
n26 <- n26[order(n26$region), ]

# 2b. Fuente de la respuesta 2026 y diferencia con la OPS a la SE 40
b <- read.csv("datos/procesados/bloques_region_anio.csv", fileEncoding = "UTF-8-BOM")
b26 <- subset(b, anio == 2026 & bloque == "lluvioso")
ops <- setNames(as.data.frame(readxl::read_excel("datos/fuentes/dengue_semanal_CR_2015-2026.xlsx", "Serie semanal"))[, 1:3], c("a", "w", "c"))
bol <- read.csv("datos/procesados/boletines_regiones_2021-2026.csv", fileEncoding = "UTF-8-BOM")
bol40 <- subset(bol, anio_datos == 2026 & se_corte == 40 & nivel == "region" & usar_en_analisis)
fuente26 <- data.frame(fuente_corte_fin = unique(b26$fuente_corte_fin), boletin_numero = unique(bol40$numero),
                       fecha_publicacion = unique(bol40$fecha_publicacion), pagina_pdf = unique(bol40$pagina_pdf),
                       url = unique(bol40$url), suma_regiones_boletin_se40 = sum(bol40$casos_acum),
                       ops_acumulado_se40 = sum(ops$c[ops$a == 2026 & ops$w <= 40]),
                       ops_ultima_semana_disponible = max(ops$w[ops$a == 2026]),
                       dif_rel_boletin_vs_ops = unique(b26$dif_ops_rel_corte_fin),
                       total_escalado_se1_40 = sum(n26$casos))
print(fuente26)

# 2a. Extrapolación: valor 2026 fuera del rango 2015–2025 de su región
extra <- do.call(rbind, lapply(c("arrastre", "aptitud_termica_pob", "oni"), function(v) {
  r <- aggregate(ent[[v]], list(region = ent$region), range)
  x <- merge(n26[, c("region", v)], data.frame(region = r$region, min = r$x[, 1], max = r$x[, 2]), by = "region")
  data.frame(variable = v, region = x$region, valor_2026 = x[[v]], min_2015_2025 = x$min, max_2015_2025 = x$max,
             extrapolacion = x[[v]] < x$min | x[[v]] > x$max)
}))
print(subset(extra, extrapolacion))

# 2a/2c. Ajuste, predictiva y comparación con lo observado
evaluar26 <- function(k) {
  t <- modelos[[k]]; e <- ent; p <- n26
  for (v in continuas(t)) { m <- mean(e[[v]]); s <- sd(e[[v]])
    e[[paste0(v, "_z")]] <- (e[[v]] - m) / s; p[[paste0(v, "_z")]] <- (p[[v]] - m) / s }
  w <- character()
  mod <- withCallingHandlers(glmmTMB(formula_v2(t), family = nbinom2, data = e, REML = reml),
                             warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
  p$anio_f <- factor(NA, levels = levels(e$anio_f))
  bh <- fixef(mod)$cond; V <- vcov(mod)$cond
  X <- model.matrix(delete.response(terms(formula(mod, fixed.only = TRUE))), p)[, names(bh), drop = FALSE]
  set.seed(2026)
  beta <- mvrnorm(B26, bh, V); u <- rnorm(B26, 0, sigma_a(mod))
  eta <- X %*% t(beta) + matrix(u, nrow(X), B26, byrow = TRUE) + log(p$poblacion)
  y <- matrix(rnbinom(length(eta), mu = exp(eta), size = sigma(mod)), nrow(X), B26)
  y <- rbind(y, colSums(y)); obs <- c(p$casos, sum(p$casos))
  q <- t(apply(y, 1, quantile, c(0.5, 0.025, 0.975, 0.10, 0.90)))
  p_two <- pmin(1, 2 * pmin(rowMeans(y <= obs), rowMeans(y >= obs)))
  out <- data.frame(modelo = k, ajuste = if (reml) "REML" else "ML", region = c(as.character(p$region), "Nacional"),
                    observado = obs, mediana = q[, 1], ip95_inf = q[, 2], ip95_sup = q[, 3], ip80_inf = q[, 4], ip80_sup = q[, 5],
                    razon_obs_mediana = obs / q[, 1], fuera_ip95 = obs < q[, 2] | obs > q[, 3],
                    direccion = ifelse(obs > q[, 3], "por encima", ifelse(obs < q[, 2], "por debajo", "dentro")),
                    p_dos_colas = p_two, sigma_a = sigma_a(mod), pdHess = isTRUE(mod$sdr$pdHess),
                    avisos = paste(unique(w), collapse = " | "))
  reg <- out$region != "Nacional"
  out$p_holm <- NA; out$p_holm[reg] <- p.adjust(out$p_dos_colas[reg], "holm")
  out
}
res <- do.call(rbind, lapply(names(modelos), evaluar26))
res$modelo <- factor(res$modelo, levels = names(modelos))

# 2d. Sensibilidad: razón y fuera del IP95 en S, M1 y M0
sens <- reshape(res[, c("modelo", "region", "razon_obs_mediana", "fuera_ip95", "direccion", "p_holm")],
                idvar = "region", timevar = "modelo", direction = "wide")
sens$misma_conclusion <- with(sens, fuera_ip95.S == fuera_ip95.M1 & fuera_ip95.S == fuera_ip95.M0 &
                                    direccion.S == direccion.M1 & direccion.S == direccion.M0)

options(width = 220)
print(res[, c("modelo", "region", "observado", "mediana", "ip95_inf", "ip95_sup", "ip80_inf", "ip80_sup", "razon_obs_mediana",
              "direccion", "p_dos_colas", "p_holm")], digits = 3)
print(sens, digits = 3)
print(subset(res, region == "Occidente"), digits = 3)
write.csv(fuente26, "modelo/resultados/s14_2026_fuente_respuesta.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(extra, "modelo/resultados/s14_2026_extrapolacion.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(res, "modelo/resultados/s14_2026_evaluacion.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(sens, "modelo/resultados/s14_2026_sensibilidad.csv", row.names = FALSE, fileEncoding = "UTF-8")

# 2e. Figura: observado frente al IP95 (e IP80) por región y modelo; tasa por 100 000 en escala log
pob <- setNames(c(n26$poblacion, sum(n26$poblacion)), c(as.character(n26$region), "Nacional"))
fig <- transform(res, tasa_obs = 1e5 * observado / pob[region], t95i = 1e5 * ip95_inf / pob[region],
                 t95s = 1e5 * ip95_sup / pob[region], t80i = 1e5 * ip80_inf / pob[region], t80s = 1e5 * ip80_sup / pob[region],
                 tmed = 1e5 * mediana / pob[region])
fig$region <- factor(fig$region, levels = c(sort(as.character(n26$region)), "Nacional"))
etiq <- c(S = "S: arrastre + aptitud térmica (principal)", M1 = "M1: arrastre + RONI previo", M0 = "M0: sin predictores")
g <- ggplot(fig, aes(y = modelo, colour = modelo, shape = modelo)) +
  geom_linerange(aes(xmin = t95i, xmax = t95s), linewidth = 0.6) +
  geom_linerange(aes(xmin = t80i, xmax = t80s), linewidth = 1.6) +
  geom_point(aes(x = tmed), size = 2.4, fill = "#fcfcfb") +
  geom_vline(aes(xintercept = tasa_obs), data = unique(fig[, c("region", "tasa_obs")]), colour = "#1f1f1e", linewidth = 0.6) +
  facet_wrap(~ region, ncol = 2, scales = "free_x") +
  scale_x_log10() + scale_y_discrete(limits = rev(names(modelos)), labels = NULL) +
  scale_colour_manual(values = c(S = "#2a78d6", M1 = "#eb6834", M0 = "#1baf7a"), labels = etiq, name = NULL) +
  scale_shape_manual(values = c(S = 21, M1 = 24, M0 = 22), labels = etiq, name = NULL) +
  labs(x = "Casos por 100 000 habitantes, SE 1–40 de 2026 (escala log)", y = NULL,
       title = "Dengue 2026: observado frente a la predictiva por región",
       subtitle = paste0("Línea vertical: observado. Barra fina: IP95; barra gruesa: IP80; punto: mediana. Ajuste ",
                         if (reml) "REML" else "ML", " con 2015–2025.")) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "top", legend.direction = "vertical", panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(), plot.background = element_rect(fill = "#fcfcfb", colour = NA),
        strip.text = element_text(face = "bold", hjust = 0), plot.margin = margin(8, 18, 8, 8))
ggsave("figuras/s14_prediccion_2026.png", g, width = 7.5, height = 9, dpi = 200)
