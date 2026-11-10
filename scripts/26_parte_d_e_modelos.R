# PARTES D y E: lista cerrada de modelos con predictores biológicos y diagnósticos.
# Todos: region + offset(log(poblacion)) + (1 | anio_f), nbinom2, 2015–2025 (99 filas).
# Predictores estandarizados DENTRO de cada pliegue LOYO (media y DE del entrenamiento); en el ajuste
# completo, con las 99 filas. 2026 no se usa ni se predice.
# Requiere scripts/12_cv_loyo.R (ajustar, sigma_a, simular, metricas_loyo, pseudo_r2_anio) y
# scripts/25_parte_c_predictores.R (datos_bio).

library(glmmTMB)
library(DHARMa)
library(parallel)

set.seed(2026)
d <- datos_bio()
nucleos <- max(1, detectCores() - 2)

comun <- "region + (1 | anio_f) + offset(log(poblacion))"
modelos <- list(
  M0      = character(),
  M1      = c("arrastre", "oni"),
  M3b     = c("arrastre", "oni", "reemergencia"),
  M_clima = c("arrastre", "oni", "aptitud_termica_pob", "sequia_pob"),
  M_bio   = c("arrastre", "oni", "reemergencia", "aptitud_termica_pob", "sequia_pob"),
  M_bio_area = c("arrastre", "oni", "reemergencia", "aptitud_termica_area", "sequia_area"))   # E5
formula_de <- function(v) as.formula(paste("casos_escalados ~",
  paste(c(if (length(v)) paste0(v, "_z"), comun), collapse = " + ")))

# Estandariza `vars` de ent y pru con la media y DE de ent
estandarizar <- function(ent, pru, vars) {
  for (v in vars) {
    m <- mean(ent[[v]]); s <- sd(ent[[v]])
    ent[[paste0(v, "_z")]] <- (ent[[v]] - m) / s
    pru[[paste0(v, "_z")]] <- (pru[[v]] - m) / s
  }
  list(ent = ent, pru = pru)
}

# LOYO con estandarización por pliegue; guarda predicciones, coeficientes y σ_a de cada pliegue
cv_pliegues <- function(vars, B = 5000) {
  res <- lapply(2015:2025, function(a) {
    z <- estandarizar(subset(d, anio != a), subset(d, anio == a), vars)
    ent <- droplevels(z$ent); ent$region <- factor(ent$region, levels = levels(d$region))
    mod <- ajustar(formula_de(vars), ent)
    sl  <- log1p(1e5 * simular(mod, z$pru, B) / z$pru$poblacion)
    list(pred = data.frame(anio = a, region = z$pru$region, y_log = z$pru$y_log, media_pred = rowMeans(sl),
                           ip_inf = apply(sl, 1, quantile, 0.025), ip_sup = apply(sl, 1, quantile, 0.975),
                           crps = scoringRules::crps_sample(z$pru$y_log, sl)),
         coef = data.frame(anio_fuera = a, termino = names(fixef(mod)$cond), estimado = fixef(mod)$cond),
         diag = data.frame(anio_fuera = a, sigma_a = sigma_a(mod), pdHess = isTRUE(mod$sdr$pdHess),
                           avisos = attr(mod, "avisos")))
  })
  lapply(c(pred = "pred", coef = "coef", diag = "diag"), function(k) do.call(rbind, lapply(res, `[[`, k)))
}

# Ajustes completos (99 filas, estandarización con las 99)
completo <- estandarizar(d, d, unique(unlist(modelos)))$ent
ajustes <- lapply(modelos, function(v) ajustar(formula_de(v), completo))
for (k in names(ajustes)) { cat("\n=====", k, "=====\n"); print(summary(ajustes[[k]])) }

cvs <- lapply(modelos, cv_pliegues)
for (k in names(cvs)) cvs[[k]] <- lapply(cvs[[k]], function(x) cbind(modelo = k, x))
write.csv(do.call(rbind, lapply(cvs, `[[`, "pred")), "modelo/resultados/d_cv_loyo_predicciones.csv", row.names = FALSE)

# Techo: año como efecto fijo; R² dentro de región en la muestra
techo <- ajustar(casos_escalados ~ region + anio_f + offset(log(poblacion)), d)
yh <- log1p(1e5 * fitted(techo) / d$poblacion)
yc <- d$y_log - ave(d$y_log, d$region); yhc <- yh - ave(yh, d$region)
r2_techo <- c(total = 1 - sum((d$y_log - yh)^2) / sum((d$y_log - mean(d$y_log))^2),
              dentro = 1 - sum((yc - yhc)^2) / sum(yc^2))

tabla_d <- do.call(rbind, lapply(names(modelos), function(k) {
  m <- ajustes[[k]]
  cbind(data.frame(modelo = k, evaluacion = "LOYO", pseudo_r2_anio = pseudo_r2_anio(m, ajustes$M0)),
        metricas_loyo(cvs[[k]]$pred),
        data.frame(sigma_a = sigma_a(m), AIC = AIC(m), filas_ajuste = nobs(m), pdHess = isTRUE(m$sdr$pdHess)))
}))
tabla_d <- rbind(tabla_d, data.frame(modelo = "Techo", evaluacion = "en la muestra", pseudo_r2_anio = NA,
  r2_loyo_total = r2_techo[["total"]], r2_loyo_dentro_region = r2_techo[["dentro"]], crps_medio = NA,
  cobertura_ip95 = NA, filas_evaluadas = nrow(d), sigma_a = NA, AIC = AIC(techo), filas_ajuste = nobs(techo),
  pdHess = isTRUE(techo$sdr$pdHess)))
tabla_d$pseudo_r2_ge_070 <- tabla_d$pseudo_r2_anio >= 0.70; tabla_d$pseudo_r2_ge_090 <- tabla_d$pseudo_r2_anio >= 0.90
tabla_d$r2_dentro_ge_070 <- tabla_d$r2_loyo_dentro_region >= 0.70
tabla_d$r2_dentro_ge_090 <- tabla_d$r2_loyo_dentro_region >= 0.90
cat("\nTabla D (en la fila Techo, los R² son en la muestra)\n"); print(tabla_d, digits = 3)
write.csv(tabla_d, "modelo/resultados/tabla_d_modelos_bio.csv", row.names = FALSE)
coefs <- do.call(rbind, lapply(names(ajustes), function(k) {
  s <- summary(ajustes[[k]])$coefficients$cond
  data.frame(modelo = k, termino = rownames(s), s, row.names = NULL)
}))
write.csv(coefs, "modelo/resultados/d_coeficientes.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# PARTE E
# -----------------------------------------------------------------------------
resumen <- list()
anotar <- function(problema, aparecio, detalle)
  resumen[[length(resumen) + 1]] <<- data.frame(problema = problema, aparecio = if (aparecio) "APARECIÓ" else "NO", detalle = detalle)

# E1: ajuste singular (Hessiano no definido positivo) o σ_a < 0,05, en el ajuste completo y en los pliegues
e1 <- do.call(rbind, lapply(names(modelos)[-1], function(k) {
  dg <- cvs[[k]]$diag
  data.frame(modelo = k, sigma_a_completo = sigma_a(ajustes[[k]]), pdHess_completo = isTRUE(ajustes[[k]]$sdr$pdHess),
             pliegues_sin_pdHess = sum(!dg$pdHess), pliegues_sigma_a_menor_005 = sum(dg$sigma_a < 0.05),
             sigma_a_pliegues_min = min(dg$sigma_a), sigma_a_pliegues_max = max(dg$sigma_a))
}))
print(e1, digits = 3); write.csv(e1, "modelo/resultados/e1_singularidad.csv", row.names = FALSE)
for (k in e1$modelo) { x <- e1[e1$modelo == k, ]
  anotar(paste("E1 ajuste singular o σ_a < 0,05:", k),
         !x$pdHess_completo || x$sigma_a_completo < 0.05 || x$pliegues_sin_pdHess > 0 || x$pliegues_sigma_a_menor_005 > 0,
         sprintf("σ_a completo %.3f; pliegues sin pdHess %d; pliegues con σ_a < 0,05 %d", x$sigma_a_completo,
                 x$pliegues_sin_pdHess, x$pliegues_sigma_a_menor_005)) }

# E2: estabilidad de los coeficientes de los predictores en los 11 pliegues
e2 <- do.call(rbind, lapply(names(modelos)[-1], function(k) {
  cf <- subset(cvs[[k]]$coef, grepl("_z$", termino))
  do.call(rbind, lapply(split(cf, cf$termino), function(x)
    data.frame(modelo = k, termino = x$termino[1], completo = fixef(ajustes[[k]])$cond[[x$termino[1]]],
               min = min(x$estimado), max = max(x$estimado), cambia_signo = min(x$estimado) < 0 && max(x$estimado) > 0)))
}))
print(e2, digits = 3); write.csv(e2, "modelo/resultados/e2_estabilidad_coeficientes.csv", row.names = FALSE)
for (k in unique(e2$modelo)) { x <- e2[e2$modelo == k, ]
  anotar(paste("E2 cambio de signo entre pliegues:", k), any(x$cambia_signo),
         if (any(x$cambia_signo)) paste("cambia:", paste(x$termino[x$cambia_signo], collapse = ", ")) else "ningún término cambia de signo") }

# E3: residuos DHARMa de M_bio frente a cada predictor continuo (prueba de cuantiles)
res_bio <- simulateResiduals(ajustes$M_bio, n = 1000, seed = 2026)
cont <- c("arrastre", "oni", "aptitud_termica_pob", "sequia_pob")
png("figuras/fig_e3_dharma_mbio.png", width = 2000, height = 1600, res = 200)
par(mfrow = c(2, 2))
e3 <- do.call(rbind, lapply(cont, function(v) {
  plotResiduals(res_bio, form = d[[v]], xlab = v, main = paste("M_bio:", v))
  data.frame(predictor = v, p_cuantiles = testQuantiles(res_bio, predictor = d[[v]], plot = FALSE)$p.value)
}))
dev.off()
e3$p_menor_005 <- e3$p_cuantiles < 0.05
print(e3, digits = 3); write.csv(e3, "modelo/resultados/e3_dharma_predictores.csv", row.names = FALSE)
for (i in seq_len(nrow(e3))) anotar(paste("E3 patrón en residuos DHARMa de M_bio vs", e3$predictor[i]),
  e3$p_menor_005[i], sprintf("prueba de cuantiles p = %.3f", e3$p_cuantiles[i]))

# E4: residuos de H. Caribe frente al resto (DHARMa de M_bio y error LOYO de M1 y M_bio)
hc <- d$region == "H. Caribe"
w_dh <- wilcox.test(residuals(res_bio)[hc], residuals(res_bio)[!hc])
e4 <- data.frame(medida = "residuo DHARMa M_bio (escala 0-1)", media_hcaribe = mean(residuals(res_bio)[hc]),
                 media_resto = mean(residuals(res_bio)[!hc]), p_wilcoxon = w_dh$p.value)
for (k in c("M1", "M_bio")) {
  p <- cvs[[k]]$pred; err <- p$y_log - p$media_pred; h <- p$region == "H. Caribe"
  e4 <- rbind(e4, data.frame(medida = paste("error LOYO", k, "(observado − predicho, log(1+tasa))"),
                             media_hcaribe = mean(err[h]), media_resto = mean(err[!h]),
                             p_wilcoxon = wilcox.test(err[h], err[!h])$p.value))
}
print(e4, digits = 3); write.csv(e4, "modelo/resultados/e4_hcaribe.csv", row.names = FALSE)
anotar("E4 residuos de H. Caribe distintos del resto", any(e4$p_wilcoxon < 0.05),
       paste(sprintf("%s: p = %.3f", e4$medida, e4$p_wilcoxon), collapse = "; "))

# E5: M_bio con versiones por área (fila M_bio_area de la tabla D)
x <- tabla_d[match(c("M_bio", "M_bio_area"), tabla_d$modelo), ]
b_pob  <- fixef(ajustes$M_bio)$cond[c("aptitud_termica_pob_z", "sequia_pob_z")]
b_area <- fixef(ajustes$M_bio_area)$cond[c("aptitud_termica_area_z", "sequia_area_z")]
anotar("E5 M_bio cambia con las versiones por área",
       abs(diff(x$r2_loyo_dentro_region)) > 0.05 || any(sign(b_pob) != sign(b_area)),
       sprintf("R² LOYO dentro de región: pob %.3f, área %.3f; coef. aptitud %.3f / %.3f, sequía %.3f / %.3f; criterio: diferencia > 0,05 o cambio de signo",
               x$r2_loyo_dentro_region[1], x$r2_loyo_dentro_region[2], b_pob[1], b_area[1], b_pob[2], b_area[2]))

# E6: cobertura del IP95 propagando la incertidumbre de σ_a y θ: bootstrap paramétrico por pliegue,
# 200 reajustes sobre datos simulados del modelo de entrenamiento × 100 réplicas predictivas por reajuste.
bootstrap_pliegue <- function(vars, a, R = 200, Bpred = 100) {
  z <- estandarizar(subset(d, anio != a), subset(d, anio == a), vars)
  ent <- droplevels(z$ent); ent$region <- factor(ent$region, levels = levels(d$region)); pru <- z$pru
  f <- formula_de(vars); mod <- ajustar(f, ent)
  sims <- simulate(mod, nsim = R, seed = 1000 + a)
  X <- model.matrix(delete.response(terms(formula(mod, fixed.only = TRUE))), pru)
  draws <- mclapply(seq_len(R), function(b) {
    e2 <- ent; e2$casos_escalados <- sims[[b]]
    m <- try(ajustar(f, e2), silent = TRUE)
    if (inherits(m, "try-error") || !isTRUE(m$sdr$pdHess)) return(NULL)
    set.seed(b)
    eta <- as.vector(X[, names(fixef(m)$cond)] %*% fixef(m)$cond) + log(pru$poblacion)
    u <- rnorm(Bpred, 0, sigma_a(m))
    mu <- exp(outer(eta, u, "+"))
    matrix(rnbinom(length(mu), mu = mu, size = sigma(m)), nrow(pru))
  }, mc.cores = nucleos, mc.set.seed = FALSE)
  ok <- !vapply(draws, is.null, TRUE)
  sl <- log1p(1e5 * do.call(cbind, draws[ok]) / pru$poblacion)
  data.frame(anio = a, region = pru$region, y_log = pru$y_log,
             ip_inf = apply(sl, 1, quantile, 0.025), ip_sup = apply(sl, 1, quantile, 0.975), reajustes_validos = sum(ok))
}
e6_pred <- do.call(rbind, lapply(c("M1", "M_bio"), function(k)
  cbind(modelo = k, do.call(rbind, lapply(2015:2025, function(a) bootstrap_pliegue(modelos[[k]], a))))))
write.csv(e6_pred, "modelo/resultados/e6_bootstrap_intervalos.csv", row.names = FALSE)
e6 <- do.call(rbind, lapply(c("M1", "M_bio"), function(k) {
  p <- e6_pred[e6_pred$modelo == k, ]
  data.frame(modelo = k, cobertura_ip95_plugin = tabla_d$cobertura_ip95[tabla_d$modelo == k],
             cobertura_ip95_bootstrap = mean(p$y_log >= p$ip_inf & p$y_log <= p$ip_sup),
             reajustes_validos_min = min(p$reajustes_validos))
}))
print(e6, digits = 3); write.csv(e6, "modelo/resultados/e6_cobertura_bootstrap.csv", row.names = FALSE)
for (i in seq_len(nrow(e6))) anotar(paste("E6 cobertura IP95 bootstrap por debajo de 0,90:", e6$modelo[i]),
  e6$cobertura_ip95_bootstrap[i] < 0.90,
  sprintf("cobertura plug-in %.3f; bootstrap %.3f; reajustes válidos mín. %d de 200", e6$cobertura_ip95_plugin[i],
          e6$cobertura_ip95_bootstrap[i], e6$reajustes_validos_min[i]))

resumen <- do.call(rbind, resumen)
cat("\nPARTE E: resumen\n"); print(resumen, right = FALSE)
write.csv(resumen, "modelo/resultados/e_resumen.csv", row.names = FALSE)
