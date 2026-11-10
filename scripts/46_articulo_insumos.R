# Sección 15: insumos numéricos del artículo. No ajusta modelos nuevos ni cambia decisiones.
#   1. Descripción: casos escalados SE 1–40 y tasa por 100 000 por región y año (2015–2026) -> Cuadro 1 y Figura 1.
#   2. Modelo final S (arrastre + aptitud térmica), el mismo de 41_seleccion_explicacion.R (ML) y de 44 (REML):
#      se vuelve a ajustar igual solo para leer θ y para los residuos DHARMa. Se comprueba que los coeficientes
#      coinciden con s13_explicacion_coeficientes.csv y σ_a con s14_2026_evaluacion.csv.
#   3. Versiones de R y de los paquetes (sessionInfo()).
#   4. Tabla de números del texto (clave, valor con formato del texto, archivo de origen): s15_numeros_texto.csv.
# Formato del texto: coma decimal y espacio indivisible entre grupos de tres cifras.
# Requiere 12_cv_loyo.R (preparar_datos), 25_parte_c_predictores.R (datos_bio) y 36_validacion_v2.R (formula_v2).
library(glmmTMB)
library(DHARMa)
library(ggplot2)

if (!exists("formula_v2")) {   # uso fuera de modelo_dengue.R: carga solo las funciones de los scripts previos
  solo_funciones <- function(f) for (e in parse(f, encoding = "UTF-8"))
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function")))
      eval(e, globalenv())
  for (f in c("scripts/12_cv_loyo.R", "scripts/25_parte_c_predictores.R", "scripts/36_validacion_v2.R")) solo_funciones(f)
}
indicadoras <- c("caribe", "chorotega")
r <- function(f) read.csv(file.path("modelo/resultados", f), fileEncoding = "UTF-8")

# Formato numérico del texto
nb <- " "
num <- function(x, d = 0) {
  s <- formatC(round(x, d), format = "f", digits = d, big.mark = nb, decimal.mark = ",")
  gsub("^-", "−", s)
}
pval <- function(p) ifelse(p < 0.001, "< 0,001", num(p, 3))

# -----------------------------------------------------------------------------------------------------------
# 1. Descripción por región y año
# -----------------------------------------------------------------------------------------------------------
d <- preparar_datos()
d <- d[, c("anio", "region", "casos_escalados", "poblacion", "tasa")]; d$region <- as.character(d$region)
write.csv(d, "modelo/resultados/s15_tasas_region_anio.csv", row.names = FALSE, fileEncoding = "UTF-8")
h <- subset(d, anio <= 2025); a26 <- subset(d, anio == 2026)
stopifnot(nrow(h) == 99, nrow(a26) == 9)

cuadro1 <- do.call(rbind, lapply(split(h, h$region), function(x) data.frame(
  region = x$region[1],
  casos_total_2015_2025 = sum(x$casos_escalados),
  tasa_mediana_2015_2025 = median(x$tasa), tasa_min_2015_2025 = min(x$tasa), tasa_max_2015_2025 = max(x$tasa),
  anio_tasa_max = x$anio[which.max(x$tasa)])))
cuadro1 <- merge(cuadro1, data.frame(region = a26$region, casos_2026 = a26$casos_escalados, tasa_2026 = a26$tasa,
                                     poblacion_2026 = a26$poblacion), by = "region")
nac <- aggregate(cbind(casos_escalados, poblacion) ~ anio, data = d, FUN = sum)
nac$tasa <- 1e5 * nac$casos_escalados / nac$poblacion
nh <- subset(nac, anio <= 2025)
cuadro1 <- rbind(cuadro1, data.frame(
  region = "Costa Rica", casos_total_2015_2025 = sum(nh$casos_escalados), tasa_mediana_2015_2025 = median(nh$tasa),
  tasa_min_2015_2025 = min(nh$tasa), tasa_max_2015_2025 = max(nh$tasa), anio_tasa_max = nh$anio[which.max(nh$tasa)],
  casos_2026 = nac$casos_escalados[nac$anio == 2026], tasa_2026 = nac$tasa[nac$anio == 2026],
  poblacion_2026 = nac$poblacion[nac$anio == 2026]))
# Posición de 2026 entre las tasas 2015–2025 de cada región (cuántos años de 2015–2025 tuvieron una tasa menor)
cuadro1$anios_previos_con_tasa_menor <- c(sapply(cuadro1$region[cuadro1$region != "Costa Rica"], function(g)
  sum(h$tasa[h$region == g] < a26$tasa[a26$region == g])), sum(nh$tasa < nac$tasa[nac$anio == 2026]))
print(cuadro1, digits = 4)
write.csv(cuadro1, "modelo/resultados/s15_cuadro1_descriptivo.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(nac, "modelo/resultados/s15_nacional_por_anio.csv", row.names = FALSE, fileEncoding = "UTF-8")

# Figura 1: tasa por región y año, un panel por región; 2026 resaltado (corte SE 40 en todos los años)
fig <- rbind(d[, c("anio", "region", "tasa")], data.frame(anio = nac$anio, region = "Costa Rica", tasa = nac$tasa))
fig$region <- factor(fig$region, levels = c(sort(unique(as.character(d$region))), "Costa Rica"))
fig$periodo <- ifelse(fig$anio == 2026, "2026", "2015–2025")
g1 <- ggplot(fig, aes(anio, tasa)) +
  geom_line(data = subset(fig, anio <= 2025), colour = "#2a78d6", linewidth = 0.6) +
  geom_point(aes(shape = periodo, fill = periodo), colour = "#1f1f1e", size = 1.8) +
  scale_shape_manual(values = c("2015–2025" = 21, "2026" = 24), name = NULL) +
  scale_fill_manual(values = c("2015–2025" = "#2a78d6", "2026" = "#fcfcfb"), name = NULL) +
  facet_wrap(~ region, ncol = 2, scales = "free_y") + scale_y_continuous(limits = c(0, NA)) +
  scale_x_continuous(breaks = c(2015, 2018, 2021, 2024, 2026)) +
  labs(x = NULL, y = "Casos por 100 000 habitantes, SE 1–40 (escala propia de cada panel)",
       title = "Dengue por región rectora, Costa Rica, 2015–2026",
       subtitle = "Casos notificados acumulados de la SE 1 a la 40, armonizados con la serie de la OPS") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "top", panel.grid.minor = element_blank(), strip.text = element_text(face = "bold", hjust = 0),
        plot.background = element_rect(fill = "#fcfcfb", colour = NA), plot.margin = margin(8, 14, 8, 8))
ggsave("figuras/s15_tasas_region_anio.png", g1, width = 7.5, height = 9, dpi = 200)

# -----------------------------------------------------------------------------------------------------------
# 2. Modelo final S: θ y residuos DHARMa (mismo modelo; se comprueba que coincide con lo guardado)
# -----------------------------------------------------------------------------------------------------------
e <- datos_bio(); e$casos <- e$casos_escalados
for (v in c("arrastre", "aptitud_termica_pob")) e[[paste0(v, "_z")]] <- (e[[v]] - mean(e[[v]])) / sd(e[[v]])
fS <- formula_v2(c("arrastre", "aptitud_termica_pob"))
mS_ml <- glmmTMB(fS, family = nbinom2, data = e)
mS_reml <- glmmTMB(fS, family = nbinom2, data = e, REML = TRUE)
co13 <- r("s13_explicacion_coeficientes.csv")
dif_coef <- max(abs(fixef(mS_ml)$cond[co13$termino] - co13$estimado))
ev <- r("s14_2026_evaluacion.csv")
dif_sigma_reml <- abs(sigma_a(mS_reml) - unique(ev$sigma_a[ev$modelo == "S"]))
cat("Control: diferencia máxima de coeficientes ML con s13:", dif_coef, "; σ_a REML con s14:", dif_sigma_reml, "\n")
stopifnot(dif_coef < 1e-6, dif_sigma_reml < 1e-6)

res <- simulateResiduals(mS_ml, n = 1000, seed = 2026)
dharma <- data.frame(
  prueba = c("uniformidad (KS)", "dispersión", "valores atípicos (bootstrap)", "cuantiles vs arrastre",
             "cuantiles vs aptitud térmica"),
  p = c(testUniformity(res, plot = FALSE)$p.value, testDispersion(res, plot = FALSE)$p.value,
        testOutliers(res, type = "bootstrap", plot = FALSE)$p.value,
        testQuantiles(res, predictor = e$arrastre, plot = FALSE)$p.value,
        testQuantiles(res, predictor = e$aptitud_termica_pob, plot = FALSE)$p.value))
dharma$estadistico <- c(testUniformity(res, plot = FALSE)$statistic, testDispersion(res, plot = FALSE)$statistic, NA, NA, NA)
dharma$simulacion <- "simulateResiduals(n = 1000, seed = 2026), simulación condicional a los efectos de año (DHARMa 0.5)"
print(dharma)
write.csv(dharma, "modelo/resultados/s15_supuestos_S_dharma.csv", row.names = FALSE, fileEncoding = "UTF-8")

ajusteS <- data.frame(ajuste = c("ML", "REML"), theta = c(sigma(mS_ml), sigma(mS_reml)),
                      sigma_a = c(sigma_a(mS_ml), sigma_a(mS_reml)), logLik = c(logLik(mS_ml), NA),
                      pdHess = c(mS_ml$sdr$pdHess, mS_reml$sdr$pdHess))
print(ajusteS)
write.csv(ajusteS, "modelo/resultados/s15_modelo_S_theta.csv", row.names = FALSE, fileEncoding = "UTF-8")

# -----------------------------------------------------------------------------------------------------------
# 3. Software
# -----------------------------------------------------------------------------------------------------------
pq <- c("glmmTMB", "DHARMa", "MASS", "scoringRules", "SPEI", "terra", "sf", "exactextractr", "ecmwfr", "ggplot2", "httr2")
soft <- data.frame(software = c("R", pq), version = c(paste(R.version$major, R.version$minor, sep = "."),
                   sapply(pq, function(p) as.character(packageVersion(p)))))
print(soft)
write.csv(soft, "modelo/resultados/s15_software.csv", row.names = FALSE, fileEncoding = "UTF-8")

# -----------------------------------------------------------------------------------------------------------
# 4. Números del texto: clave, valor con formato y archivo de origen
# -----------------------------------------------------------------------------------------------------------
N <- list()
pon <- function(clave, valor, archivo) N[[length(N) + 1]] <<- data.frame(clave = clave, valor = valor, archivo = archivo)

# Descripción
c1 <- cuadro1; cr <- c1[c1$region == "Costa Rica", ]; rg <- c1[c1$region != "Costa Rica", ]
f1 <- "s15_cuadro1_descriptivo.csv"; fn <- "s15_nacional_por_anio.csv"
pon("casos_total_2015_2025", num(cr$casos_total_2015_2025), f1)
pon("tasa_nac_min", num(cr$tasa_min_2015_2025, 1), f1)
pon("anio_tasa_nac_min", nh$anio[which.min(nh$tasa)], fn)
pon("tasa_nac_max", num(cr$tasa_max_2015_2025, 1), f1)
pon("anio_tasa_nac_max", cr$anio_tasa_max, f1)
pon("casos_nac_max", num(max(nh$casos_escalados)), fn)
pon("anio_casos_nac_max", nh$anio[which.max(nh$casos_escalados)], fn)
pon("casos_nac_min", num(min(nh$casos_escalados)), fn)
pon("anio_casos_nac_min", nh$anio[which.min(nh$casos_escalados)], fn)
pon("razon_max_min_nac", num(max(nh$tasa) / min(nh$tasa), 0), fn)
pon("casos_2026", num(cr$casos_2026), f1)
pon("tasa_nac_2026", num(cr$tasa_2026, 1), f1)
pon("anios_previos_menor_2026_nac", cr$anios_previos_con_tasa_menor, f1)
pon("tasa_nac_mediana", num(cr$tasa_mediana_2015_2025, 1), f1)
pon("occ_anios_previos_menor", rg$anios_previos_con_tasa_menor[rg$region == "Occidente"], f1)
pon("occ_tasa_2026", num(rg$tasa_2026[rg$region == "Occidente"], 1), f1)
pon("occ_tasa_max", num(rg$tasa_max_2015_2025[rg$region == "Occidente"], 1), f1)
pon("occ_anio_tasa_max", rg$anio_tasa_max[rg$region == "Occidente"], f1)
pon("n_anios_regiones_con_cero", sum(h$casos_escalados == 0), "s15_tasas_region_anio.csv")
o <- rg[order(-rg$tasa_mediana_2015_2025), ]
pon("region_tasa_mediana_max", o$region[1], f1); pon("tasa_mediana_max", num(o$tasa_mediana_2015_2025[1], 1), f1)
pon("region_tasa_mediana_2", o$region[2], f1); pon("tasa_mediana_2", num(o$tasa_mediana_2015_2025[2], 1), f1)
pon("region_tasa_mediana_min", o$region[9], f1); pon("tasa_mediana_min", num(o$tasa_mediana_2015_2025[9], 1), f1)
pon("razon_mediana_max_min", num(o$tasa_mediana_2015_2025[1] / o$tasa_mediana_2015_2025[9], 0), f1)
n_max_2023 <- sum(rg$anio_tasa_max == 2023 | rg$anio_tasa_max == 2024)
pon("regiones_max_2023_2024", n_max_2023, f1)

# Selección (s13)
ps <- r("s13_explicacion_pasos.csv"); fp <- "s13_explicacion_pasos.csv"
for (k in c("a", "b", "c", "d", "e", "f")) {
  x <- ps[ps$paso == k, ]
  pon(paste0("lrt_", k), num(x$estadistico, 2), fp); pon(paste0("p_", k), pval(x$p_chi2), fp)
}
pon("p_boot_f", num(ps$p_boot[ps$paso == "f"], 3), fp)
pon("aic_m0", num(ps$AIC_reducido[ps$paso == "a"], 0), fp)
pon("aic_final", num(ps$AIC_ampliado[ps$paso == "b"], 0), fp)
sg <- r("s13_explicacion_sigma_a.csv"); fs <- "s13_explicacion_sigma_a.csv"
pon("lrt_sigma", num(sg$estadistico, 2), fs); pon("p_sigma_ingenuo", num(sg$p_chi2_ingenuo, 3), fs)
pon("p_sigma_boot", num(sg$p_boot, 3), fs); pon("reps_boot", num(sg$replicas_validas), fs)
aj <- r("s13_explicacion_ajuste.csv"); fa <- "s13_explicacion_ajuste.csv"
fin <- aj[aj$modelo == "final", ]; m0 <- aj[aj$modelo == "M0", ]
pon("sigma_m0", num(m0$sigma_a, 3), fa); pon("sigma_final", num(fin$sigma_a, 3), fa)
pon("pseudo_r2", num(fin$pseudo_r2_anio, 3), fa)
pon("r2_marg_total", num(fin$r2_marginal_total, 3), fa); pon("r2_marg_dentro", num(fin$r2_marginal_dentro, 3), fa)
pon("r2_cond_total", num(fin$r2_condicional_total, 3), fa); pon("r2_cond_dentro", num(fin$r2_condicional_dentro, 3), fa)
pon("r2_marg_total_m0", num(m0$r2_marginal_total, 3), fa); pon("r2_cond_dentro_m0", num(m0$r2_condicional_dentro, 3), fa)
fc <- "s13_explicacion_coeficientes.csv"; ar <- co13[co13$termino == "arrastre_z", ]; ap <- co13[co13$termino == "aptitud_termica_pob_z", ]
pon("rr_arr", num(ar$razon_tasas, 2), fc); pon("rr_arr_inf", num(ar$ic_perfil_inf, 2), fc); pon("rr_arr_sup", num(ar$ic_perfil_sup, 2), fc)
pon("rr_apt_total", num(ap$razon_tasas, 1), fc); pon("rr_apt_total_inf", num(ap$ic_perfil_inf, 1), fc)
pon("rr_apt_total_sup", num(ap$ic_perfil_sup, 0), fc)
rr <- r("s14_aptitud_rr.csv"); fr <- "s14_aptitud_rr.csv"; ml <- rr[rr$ajuste == "ML", ]; re <- rr[rr$ajuste == "REML", ]
pon("rr_apt_dentro", num(ml$RR_por_DE_dentro, 2), fr); pon("rr_apt_dentro_inf", num(ml$IC_dentro_inf, 2), fr)
pon("rr_apt_dentro_sup", num(ml$IC_dentro_sup, 2), fr)
pon("rr_apt_dentro_reml", num(re$RR_por_DE_dentro, 2), fr); pon("rr_apt_dentro_reml_inf", num(re$IC_dentro_inf, 2), fr)
pon("rr_apt_dentro_reml_sup", num(re$IC_dentro_sup, 2), fr)
pon("de_apt_total", num(ml$de_total, 3), fr); pon("de_apt_dentro", num(ml$de_dentro_region, 3), fr)
pon("pct_var_dentro", num(100 * ml$varianza_dentro_sobre_total, 1), fr)
pon("de_total_en_de_dentro", num(1 / ml$razon_de, 1), fr)
rgo <- r("s14_aptitud_rango_por_region.csv"); fg <- "s14_aptitud_rango_por_region.csv"
pon("rango_apt_max", num(max(rgo$aptitud_termica_pob.rango), 3), fg)
pon("theta_ml", num(ajusteS$theta[1], 2), "s15_modelo_S_theta.csv")
pon("theta_reml", num(ajusteS$theta[2], 2), "s15_modelo_S_theta.csv")
pon("sigma_reml", num(ajusteS$sigma_a[2], 3), "s15_modelo_S_theta.csv")
fd <- "s15_supuestos_S_dharma.csv"
pon("dharma_unif", num(dharma$p[1], 2), fd); pon("dharma_disp", num(dharma$p[2], 2), fd)
pon("dharma_out", num(dharma$p[3], 2), fd); pon("dharma_arr", num(dharma$p[4], 2), fd); pon("dharma_apt", num(dharma$p[5], 2), fd)

# Validación (s13 y s14)
cal <- r("s14_calibracion_tabla.csv"); fk <- "s14_calibracion_tabla.csv"
cg <- function(esq, mod, col, dd = 3) num(cal[cal$esquema == esq & cal$modelo == mod, col], dd)
for (m in c("S_ML", "S_REML", "M1_REML", "M0_REML", "M1_ML", "M0_ML")) {
  k <- tolower(m)
  pon(paste0("adel_crps_", k), cg("hacia_adelante", m, "crps_medio"), fk)
  pon(paste0("adel_ip95_", k), cg("hacia_adelante", m, "cobertura_ip95"), fk)
  pon(paste0("adel_ip80_", k), cg("hacia_adelante", m, "cobertura_ip80"), fk)
  pon(paste0("adel_ip50_", k), cg("hacia_adelante", m, "cobertura_ip50"), fk)
  pon(paste0("adel_r2d_", k), cg("hacia_adelante", m, "r2_loyo_dentro_region"), fk)
  pon(paste0("loyo_crps_", k), cg("LOYO", m, "crps_medio"), fk)
  pon(paste0("loyo_ip95_", k), cg("LOYO", m, "cobertura_ip95"), fk)
  pon(paste0("loyo_r2d_", k), cg("LOYO", m, "r2_loyo_dentro_region"), fk)
}
cp <- r("s14_calibracion_pareadas.csv"); fq <- "s14_calibracion_pareadas.csv"
x <- cp[cp$esquema == "hacia_adelante" & cp$modelo == "S_REML", ]
pon("dif_reml_ml", num(x$dif_crps, 3), fq); pon("dif_reml_ml_inf", num(x$ic_crps_inf, 3), fq)
pon("dif_reml_ml_sup", num(x$ic_crps_sup, 3), fq)
sm <- r("s13_prediccion_metricas.csv"); fm <- "s13_prediccion_metricas.csv"; isl <- sm[sm$modelo == "Seleccion_ISLR", ]
pon("islr_crps", num(isl$crps_medio, 3), fm); pon("islr_ip95", num(isl$cobertura_ip95, 3), fm)
pon("islr_r2d", num(isl$r2_loyo_dentro_region, 3), fm)
sc <- r("s13_prediccion_comparaciones.csv"); fcmp <- "s13_prediccion_comparaciones.csv"
for (ref in c("M0", "M1")) {
  x <- sc[sc$referencia == ref, ]; k <- tolower(ref)
  pon(paste0("islr_vs_", k), num(x$dif_crps, 3), fcmp); pon(paste0("islr_vs_", k, "_inf"), num(x$ic_crps_inf, 3), fcmp)
  pon(paste0("islr_vs_", k, "_sup"), num(x$ic_crps_sup, 3), fcmp)
}
se <- r("s13_prediccion_seleccion_por_anio.csv"); fse <- "s13_prediccion_seleccion_por_anio.csv"
pon("pasos_aptitud", sum(se$aptitud_termica_pob), fse); pon("pasos_arrastre", sum(se$arrastre), fse)

# 2026 (s14)
fe <- "s14_2026_evaluacion.csv"; S <- ev[ev$modelo == "S", ]
reg9 <- S[S$region != "Nacional", ]; na <- S[S$region == "Nacional", ]
pon("n_fuera_ip95_S", sum(reg9$fuera_ip95), fe)
pon("p_holm_min_S", num(min(reg9$p_holm), 3), fe)
pon("nac_obs", num(na$observado), fe); pon("nac_med", num(na$mediana), fe)
pon("nac_ip95_inf", num(na$ip95_inf), fe); pon("nac_ip95_sup", num(na$ip95_sup), fe)
pon("nac_razon", num(na$razon_obs_mediana, 2), fe); pon("nac_p", num(na$p_dos_colas, 2), fe)
oc <- S[S$region == "Occidente", ]
pon("occ_obs", num(oc$observado), fe); pon("occ_med", num(oc$mediana), fe)
pon("occ_ip95_inf", num(oc$ip95_inf), fe); pon("occ_ip95_sup", num(oc$ip95_sup), fe)
pon("occ_razon", num(oc$razon_obs_mediana, 2), fe); pon("occ_p", num(oc$p_dos_colas, 3), fe)
pon("occ_holm", num(oc$p_holm, 2), fe)
for (m in c("M1", "M0")) {
  x <- ev[ev$modelo == m & ev$region == "Occidente", ]; k <- tolower(m)
  pon(paste0("occ_razon_", k), num(x$razon_obs_mediana, 2), fe); pon(paste0("occ_p_", k), num(x$p_dos_colas, 3), fe)
  pon(paste0("occ_holm_", k), num(x$p_holm, 2), fe)
  y <- ev[ev$modelo == m & ev$region == "Nacional", ]
  pon(paste0("nac_razon_", k), num(y$razon_obs_mediana, 2), fe)
}
pon("n_razon_menor_1_S", sum(reg9$razon_obs_mediana < 1), fe)
pon("razon_min_S", num(min(reg9$razon_obs_mediana), 2), fe)
pon("region_razon_min_S", reg9$region[which.min(reg9$razon_obs_mediana)], fe)
pon("ancho_ip95_min", num(min(reg9$ip95_sup / reg9$ip95_inf), 0), fe)
pon("ancho_ip95_max", num(max(reg9$ip95_sup / reg9$ip95_inf), 0), fe)
pc <- S[S$region == "Pacífico Central", ]
pon("pc_razon_S", num(pc$razon_obs_mediana, 2), fe)
pon("pc_razon_M1", num(ev$razon_obs_mediana[ev$modelo == "M1" & ev$region == "Pacífico Central"], 2), fe)
ss <- r("s14_2026_sensibilidad.csv"); pon("n_misma_conclusion", sum(ss$misma_conclusion), "s14_2026_sensibilidad.csv")
fu <- r("s14_2026_fuente_respuesta.csv"); ff <- "s14_2026_fuente_respuesta.csv"
pon("bol_suma", num(fu$suma_regiones_boletin_se40), ff); pon("ops_se40", num(fu$ops_acumulado_se40), ff)
pon("dif_bol_ops", num(100 * fu$dif_rel_boletin_vs_ops, 1), ff); pon("ops_ultima_se", fu$ops_ultima_semana_disponible, ff)
pon("bol_num", fu$boletin_numero, ff); pon("bol_fecha", format(as.Date(fu$fecha_publicacion), "%d/%m/%Y"), ff)
ex <- r("s14_2026_extrapolacion.csv"); fx <- "s14_2026_extrapolacion.csv"
hc <- ex[ex$variable == "arrastre" & ex$region == "H. Caribe", ]; pca <- ex[ex$variable == "aptitud_termica_pob" & ex$region == "Pacífico Central", ]
pon("hc_arr_2026", num(hc$valor_2026, 2), fx); pon("hc_arr_min", num(hc$min_2015_2025, 2), fx)
pon("pc_apt_2026", num(pca$valor_2026, 3), fx); pon("pc_apt_max", num(pca$max_2015_2025, 3), fx)
pon("roni_2026", num(ex$valor_2026[ex$variable == "oni"][1], 2), fx)

# Software
for (i in seq_len(nrow(soft))) pon(paste0("v_", soft$software[i]), soft$version[i], "s15_software.csv")

numeros <- do.call(rbind, N)
stopifnot(!anyDuplicated(numeros$clave), !anyNA(numeros$valor))
write.csv(numeros, "modelo/resultados/s15_numeros_texto.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat(nrow(numeros), "números guardados en s15_numeros_texto.csv\n")
