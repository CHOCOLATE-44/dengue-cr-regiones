# PARTE C: variación, colinealidad y coherencia de los predictores biológicos (2015–2025, 99 filas).
# Requiere scripts/12_cv_loyo.R cargado (preparar_datos). Define datos_bio(), que usa también 26.

library(ggplot2)

datos_bio <- function() {
  d <- subset(preparar_datos(), anio <= 2025)
  bio <- read.csv("datos/procesados/predictores_bio.csv", fileEncoding = "UTF-8")
  d <- merge(d, bio[, setdiff(names(bio), "fuente")], by = c("region", "anio"), all.x = TRUE)
  d$region <- factor(d$region); d$anio_f <- factor(d$anio)
  stopifnot(nrow(d) == 99, !anyNA(d[, c("arrastre", "oni", "reemergencia", "aptitud_termica_pob",
                                         "sequia_pob", "aptitud_termica_area", "sequia_area")]))
  d[order(d$anio, d$region), ]
}
d <- datos_bio()
etiquetas <- c(arrastre = "arrastre", oni = "RONI año previo", reemergencia = "reemergencia",
               aptitud_termica_pob = "aptitud térmica (pob.)", sequia_pob = "sequía (pob.)",
               temp_ea_pob = "temperatura ene–ago (pob.)",
               aptitud_termica_area = "aptitud térmica (área)", sequia_area = "sequía (área)",
               temp_ea_area = "temperatura ene–ago (área)")

# C1: variación. DE entre regiones = DE de las medias regionales; DE dentro de región = DE de los desvíos
# respecto de la media de cada región. Aviso si la varianza dentro de región es < 10 % de la total
# (umbral elegido aquí y documentado en auditoria.md).
c1 <- do.call(rbind, lapply(names(etiquetas), function(v) {
  x <- d[[v]]; dentro <- x - ave(x, d$region)
  data.frame(variable = v,
             valores_distintos_por_anio_min = min(tapply(x, d$anio, function(z) length(unique(round(z, 10))))),
             valores_distintos_por_anio_max = max(tapply(x, d$anio, function(z) length(unique(round(z, 10))))),
             de_total = sd(x), de_entre_regiones = sd(tapply(x, d$region, mean)), de_dentro_region = sd(dentro),
             prop_var_dentro = sum(dentro^2) / sum((x - mean(x))^2))
}))
c1$aviso_poca_variacion_dentro <- c1$prop_var_dentro < 0.10
cat("\nC1. Variación\n"); print(c1, digits = 3)
write.csv(c1, "modelo/resultados/c1_variacion_predictores.csv", row.names = FALSE)

# C2: correlaciones y VIF (VIF_j = 1 / (1 − R²_j), R²_j de regresar cada predictor sobre los demás)
c2v <- c("arrastre", "oni", "reemergencia", "aptitud_termica_pob", "sequia_pob")
r <- cor(d[, c2v])
vif <- sapply(c2v, function(v) 1 / (1 - summary(lm(reformulate(setdiff(c2v, v), v), data = d))$r.squared))
pares <- which(upper.tri(r), arr.ind = TRUE)
c2 <- data.frame(var1 = c2v[pares[, 1]], var2 = c2v[pares[, 2]], r = r[pares])
c2$r_abs_mayor_070 <- abs(c2$r) > 0.70
c2_vif <- data.frame(variable = c2v, VIF = vif, VIF_mayor_5 = vif > 5)
cat("\nC2. Correlaciones\n"); print(round(r, 3)); print(c2_vif, digits = 3)
write.csv(c2, "modelo/resultados/c2_correlaciones.csv", row.names = FALSE)
write.csv(c2_vif, "modelo/resultados/c2_vif.csv", row.names = FALSE)

# C3: versión por población vs por área, correlación entre años dentro de cada región
c3 <- do.call(rbind, lapply(c("temp_ea", "aptitud_termica", "sequia"), function(v)
  do.call(rbind, lapply(levels(d$region), function(g) {
    s <- d[d$region == g, ]
    data.frame(variable = v, region = g, r_pob_area = cor(s[[paste0(v, "_pob")]], s[[paste0(v, "_area")]]),
               dif_media_pob_menos_area = mean(s[[paste0(v, "_pob")]] - s[[paste0(v, "_area")]]))
  }))))
cat("\nC3. Población vs área\n"); print(c3, digits = 3)
write.csv(c3, "modelo/resultados/c3_pob_vs_area.csv", row.names = FALSE)

# C4: cada predictor vs log(1 + tasa) centrado por región; H. Caribe resaltado
d$y_c <- d$y_log - ave(d$y_log, d$region)
c4v <- c("arrastre", "oni", "reemergencia", "aptitud_termica_pob", "sequia_pob")
largo <- do.call(rbind, lapply(c4v, function(v)
  data.frame(variable = etiquetas[[v]], x = d[[v]], y_c = d$y_c, region = d$region, anio = d$anio)))
largo$variable <- factor(largo$variable, levels = unname(etiquetas[c4v]))
largo$hc <- largo$region == "H. Caribe"
g <- ggplot(largo, aes(x, y_c, colour = region)) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
  geom_point(data = subset(largo, !hc), size = 1.8, alpha = 0.75) +
  geom_point(data = subset(largo, hc), size = 3, shape = 21, fill = "black", colour = "white", stroke = 0.8) +
  facet_wrap(~ variable, scales = "free_x", ncol = 3) +
  labs(x = NULL, y = "log(1 + tasa) centrado por región", colour = "Región",
       caption = "Puntos negros con borde blanco: H. Caribe. 2015–2025, 99 filas.") +
  theme_minimal(base_size = 10) + theme(legend.position = "bottom")
ggsave("figuras/fig_c4_predictores_vs_tasa.png", g, width = 10, height = 7, dpi = 200, bg = "white")
cat("Figura: figuras/fig_c4_predictores_vs_tasa.png\n")
