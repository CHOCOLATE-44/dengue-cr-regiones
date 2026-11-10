# Sección 13 A: selección de variables para EXPLICACIÓN, según Roback y Legler (2021), versión web:
#   8.6.1 (estrategia: modelo de medias incondicionales, luego nivel uno, luego nivel dos), 1.6.7 y 8.10 (rasgos del
#   modelo final; las decisiones no se automatizan: el orden lo fijó el usuario), 4.4.4 y 8.8.2 (prueba de razón de
#   verosimilitudes con ML para modelos anidados), 9.6.4 (bootstrap paramétrico), 10.8 (quitar términos con t bajo y
#   confirmar con la prueba de razón de verosimilitudes), 8.7.2 (pseudo R²).
# Estructura fija: region + offset(log(poblacion)) + (1 | anio_f), NB2, respuesta SE 1–40, 2015–2025 (99 filas),
#   predictores estandarizados con las 99 filas (no cambia el ajuste: el modelo tiene intercepto).
# Orden fijado por el usuario: nivel uno a) arrastre, b) aptitud_termica_pob (ene–ago), c) sequia_pob,
#   d) inmunidad3_v2; nivel dos e) oni (RONI previo), f) reemergencia (POST HOC); interacciones, cada una solo si su
#   término principal quedó: g) oni:caribe, h) sequia_pob:caribe, i) sequia_pob:chorotega.
# Regla de decisión (fijada aquí): un término entra si la prueba de razón de verosimilitudes da p < 0,05. Si el p del
#   χ² cae entre 0,01 y 0,10, decide el p del bootstrap paramétrico (1000 réplicas simuladas bajo el modelo reducido).
#   σ²_a del modelo final: bootstrap paramétrico siempre (está en la frontera).
# R² marginal y condicional (NO salen de Roback y Legler): en escala log(1 + tasa), con la media ajustada sin el
#   efecto del año (marginal, u = 0) o con el efecto estimado de cada año (condicional); total y dentro de región.
# Requiere 12_cv_loyo.R (ajustar, sigma_a, pseudo_r2_anio) y 25_parte_c_predictores.R (datos_bio). No se predice 2026.
library(glmmTMB)
library(parallel)

REPS_PB <- 1000
NUCLEOS <- max(1, detectCores() - 2)

reg <- read.csv("datos/procesados/predictores_regionales.csv", fileEncoding = "UTF-8")
inm <- read.csv("datos/procesados/v2_inmunidad3.csv", fileEncoding = "UTF-8")[, c("anio", "region", "inmunidad3_v2")]
d <- merge(merge(datos_bio(), reg[, c("anio", "region", "caribe", "chorotega")], by = c("anio", "region")), inm,
           by = c("anio", "region"))
d$region <- factor(d$region); d$anio_f <- factor(d$anio); d$casos <- d$casos_escalados
d <- d[order(d$anio, d$region), ]
stopifnot(nrow(d) == 99, !anyNA(d))
cont <- c("arrastre", "aptitud_termica_pob", "sequia_pob", "inmunidad3_v2", "oni", "reemergencia")
for (v in cont) d[[paste0(v, "_z")]] <- (d[[v]] - mean(d[[v]])) / sd(d[[v]])

pasos <- data.frame(
  paso = c("a", "b", "c", "d", "e", "f", "g", "h", "i"),
  termino = c("arrastre", "aptitud_termica_pob", "sequia_pob", "inmunidad3_v2", "oni", "reemergencia",
              "oni:caribe", "sequia_pob:caribe", "sequia_pob:chorotega"),
  nivel = c(rep("uno (región-año)", 4), rep("dos (año)", 2), rep("interacción", 3)),
  requiere = c(rep(NA, 6), "oni", "sequia_pob", "sequia_pob"),
  definido = c(rep("antes", 5), "POST HOC", rep("antes (H3/H4, lista cerrada de la sección 11)", 3)))
tz <- function(t) sapply(strsplit(t, ":"), function(p) paste(ifelse(p %in% c("caribe", "chorotega"), p, paste0(p, "_z")), collapse = ":"))
formula_e <- function(t, aleatorio = TRUE) as.formula(paste("casos ~", paste(c(if (length(t)) tz(t), "region",
  if (aleatorio) "(1 | anio_f)", "offset(log(poblacion))"), collapse = " + ")))

lrt <- function(m0, m1) {
  est <- as.numeric(2 * (logLik(m1) - logLik(m0))); gl <- attr(logLik(m1), "df") - attr(logLik(m0), "df")
  c(estadistico = est, gl = gl, p = pchisq(est, gl, lower.tail = FALSE))
}
# Bootstrap paramétrico (9.6.4): simula bajo el reducido (simulate() de glmmTMB sortea efectos de año nuevos),
# reajusta los dos modelos y compara el estadístico observado con su distribución simulada.
boot_lrt <- function(f0, f1, obs, semilla) {
  m0 <- ajustar(f0, d); set.seed(semilla); sims <- simulate(m0, nsim = REPS_PB)
  est <- unlist(mclapply(seq_len(REPS_PB), function(i) {
    db <- d; db$casos <- sims[[i]]
    a0 <- suppressWarnings(try(glmmTMB(f0, family = nbinom2, data = db), silent = TRUE))
    a1 <- suppressWarnings(try(glmmTMB(f1, family = nbinom2, data = db), silent = TRUE))
    if (inherits(a0, "try-error") || inherits(a1, "try-error") || is.na(logLik(a0)) || is.na(logLik(a1))) return(NA_real_)
    max(0, as.numeric(2 * (logLik(a1) - logLik(a0))))
  }, mc.cores = NUCLEOS, mc.set.seed = FALSE))
  c(p_boot = mean(est[!is.na(est)] >= obs), replicas_validas = sum(!is.na(est)))
}

# --- Paso 0: modelo de medias incondicionales (M0, con región fija) y pasos a–i en el orden fijado -------------
actual <- character(); m_act <- ajustar(formula_e(actual), d); m0 <- m_act
reg_pasos <- list()
for (i in seq_len(nrow(pasos))) {
  t <- pasos$termino[i]; req <- pasos$requiere[i]
  fila <- data.frame(paso = pasos$paso[i], nivel = pasos$nivel[i], termino = t, definido = pasos$definido[i],
                     modelo_reducido = paste(c("M0", actual), collapse = " + "), estadistico = NA, gl = NA, p_chi2 = NA,
                     p_boot = NA, replicas_validas = NA, AIC_reducido = AIC(m_act), AIC_ampliado = NA,
                     BIC_reducido = BIC(m_act), BIC_ampliado = NA, coef_z = NA, ee = NA, decision = NA, avisos = NA)
  if (!is.na(req) && !(req %in% actual)) {
    fila$decision <- paste("no se prueba: su término principal", req, "no quedó")
  } else {
    m_amp <- ajustar(formula_e(c(actual, t)), d); r <- lrt(m_act, m_amp)
    fila[, c("estadistico", "gl", "p_chi2")] <- as.list(r)
    fila$AIC_ampliado <- AIC(m_amp); fila$BIC_ampliado <- BIC(m_amp)
    s <- summary(m_amp)$coefficients$cond; fila$coef_z <- s[tz(t), "Estimate"]; fila$ee <- s[tz(t), "Std. Error"]
    fila$avisos <- attr(m_amp, "avisos")
    p_dec <- r[["p"]]
    if (r[["p"]] >= 0.01 && r[["p"]] <= 0.10) {
      b <- boot_lrt(formula_e(actual), formula_e(c(actual, t)), r[["estadistico"]], semilla = 1300 + i)
      fila$p_boot <- b[["p_boot"]]; fila$replicas_validas <- b[["replicas_validas"]]; p_dec <- b[["p_boot"]]
    }
    if (p_dec < 0.05) { actual <- c(actual, t); m_act <- m_amp; fila$decision <- "entra" } else fila$decision <- "no entra"
  }
  reg_pasos[[i]] <- fila
  cat(sprintf("Paso %s (%s): %s\n", fila$paso, t, fila$decision))
}
tab_pasos <- do.call(rbind, reg_pasos)

# --- Quitar términos innecesarios (10.8): |z| < 2 en el modelo final, respetando la jerarquía -----------------
s <- summary(m_act)$coefficients$cond
debiles <- actual[abs(s[tz(actual), "z value"]) < 2]
debiles <- debiles[!sapply(debiles, function(t) any(grepl(paste0("^", t, ":"), setdiff(actual, debiles))))]
quitar <- data.frame(paso = "quitar (10.8)", nivel = "—", termino = paste(debiles, collapse = " + "), definido = "—",
                     modelo_reducido = paste(c("M0", setdiff(actual, debiles)), collapse = " + "), estadistico = NA, gl = NA,
                     p_chi2 = NA, p_boot = NA, replicas_validas = NA, AIC_reducido = NA, AIC_ampliado = AIC(m_act),
                     BIC_reducido = NA, BIC_ampliado = BIC(m_act), coef_z = NA, ee = NA, decision = NA, avisos = NA)
if (length(debiles)) {
  m_red <- ajustar(formula_e(setdiff(actual, debiles)), d); r <- lrt(m_red, m_act)
  quitar[, c("estadistico", "gl", "p_chi2")] <- as.list(r); quitar$AIC_reducido <- AIC(m_red); quitar$BIC_reducido <- BIC(m_red)
  p_dec <- r[["p"]]
  if (r[["p"]] >= 0.01 && r[["p"]] <= 0.10) {
    b <- boot_lrt(formula_e(setdiff(actual, debiles)), formula_e(actual), r[["estadistico"]], semilla = 1399)
    quitar$p_boot <- b[["p_boot"]]; quitar$replicas_validas <- b[["replicas_validas"]]; p_dec <- b[["p_boot"]]
  }
  if (p_dec >= 0.05) { actual <- setdiff(actual, debiles); m_act <- m_red; quitar$decision <- "se quitan" } else quitar$decision <- "se mantienen"
} else quitar$decision <- "ningún término con |z| < 2"
tab_pasos <- rbind(tab_pasos, quitar)

# --- σ²_a del modelo final: modelo sin efecto del año vs con efecto del año (frontera; bootstrap siempre) -------
m_sin <- ajustar(formula_e(actual, aleatorio = FALSE), d); r <- lrt(m_sin, m_act)
b <- boot_lrt(formula_e(actual, aleatorio = FALSE), formula_e(actual), r[["estadistico"]], semilla = 1400)
tab_sigma <- data.frame(prueba = "σ²_a = 0 (modelo final sin (1|anio_f)) vs modelo final", estadistico = r[["estadistico"]],
                        gl = r[["gl"]], p_chi2_ingenuo = r[["p"]], p_boot = b[["p_boot"]], replicas_validas = b[["replicas_validas"]],
                        sigma_a = sigma_a(m_act), AIC_sin = AIC(m_sin), AIC_con = AIC(m_act))

# --- Modelo final: razones de tasas con IC por perfil, pseudo R², R² marginal y condicional --------------------
ci <- confint(m_act, method = "profile", parm = "beta_")
sf <- summary(m_act)$coefficients$cond
coefs <- data.frame(termino = rownames(sf), estimado = sf[, "Estimate"], ee = sf[, "Std. Error"], p_wald = sf[, "Pr(>|z|)"],
                    razon_tasas = exp(sf[, "Estimate"]), ic_perfil_inf = exp(ci[rownames(sf), 1]), ic_perfil_sup = exp(ci[rownames(sf), 2]),
                    row.names = NULL)
r2 <- function(yh) { y <- d$y_log; yc <- y - ave(y, d$region); yhc <- yh - ave(yh, d$region)
  c(total = 1 - sum((y - yh)^2) / sum((y - mean(y))^2), dentro_region = 1 - sum((yc - yhc)^2) / sum(yc^2)) }
ylog <- function(mu) log1p(1e5 * mu / d$poblacion)
ajuste <- data.frame(modelo = c("M0", "final"), terminos = c("—", paste(actual, collapse = " + ")),
                     pseudo_r2_anio = c(0, pseudo_r2_anio(m_act, m0)), sigma_a = c(sigma_a(m0), sigma_a(m_act)),
                     AIC = c(AIC(m0), AIC(m_act)), BIC = c(BIC(m0), BIC(m_act)),
                     r2_marginal_total = c(r2(ylog(predict(m0, re.form = NA, type = "response")))[1],
                                           r2(ylog(predict(m_act, re.form = NA, type = "response")))[1]),
                     r2_marginal_dentro = c(r2(ylog(predict(m0, re.form = NA, type = "response")))[2],
                                            r2(ylog(predict(m_act, re.form = NA, type = "response")))[2]),
                     r2_condicional_total = c(r2(ylog(fitted(m0)))[1], r2(ylog(fitted(m_act)))[1]),
                     r2_condicional_dentro = c(r2(ylog(fitted(m0)))[2], r2(ylog(fitted(m_act)))[2]),
                     pdHess = c(isTRUE(m0$sdr$pdHess), isTRUE(m_act$sdr$pdHess)))

options(width = 220)
print(tab_pasos, digits = 3); print(tab_sigma, digits = 3); print(coefs, digits = 3); print(ajuste, digits = 3)
write.csv(tab_pasos, "modelo/resultados/s13_explicacion_pasos.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(tab_sigma, "modelo/resultados/s13_explicacion_sigma_a.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(coefs, "modelo/resultados/s13_explicacion_coeficientes.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(ajuste, "modelo/resultados/s13_explicacion_ajuste.csv", row.names = FALSE, fileEncoding = "UTF-8")
terminos_explicacion <- actual
