# Datos del modelo y validación cruzada dejando un año fuera (PASO 4).
# Se ejecuta desde la raíz del proyecto (lo llama modelo_dengue.R).

library(glmmTMB)
library(MASS)          # mvrnorm
library(scoringRules)  # crps_sample

# -----------------------------------------------------------------------------
# Datos: igual que modelo/modelo_metodologia.qmd (acumulado SE 1–40 por región y año)
# -----------------------------------------------------------------------------
preparar_datos <- function() {
  b <- read.csv("datos/procesados/bloques_region_anio.csv", fileEncoding = "UTF-8-BOM")
  d <- aggregate(cbind(casos_escalados, casos_crudos) ~ anio + region, data = b, FUN = sum)
  pob <- aggregate(poblacion ~ anio + region, data = b, FUN = function(x) x[1], na.action = na.pass)
  d <- merge(d, pob, by = c("anio", "region"), all.x = TRUE)
  for (r in unique(d$region)) {                            # Occidente 2018: interpolación 2017–2019
    i <- d$region == r
    if (anyNA(d$poblacion[i]))
      d$poblacion[i] <- approx(d$anio[i], d$poblacion[i], xout = d$anio[i], rule = 2)$y
  }
  pr <- read.csv("datos/procesados/predictores_region_anio.csv", fileEncoding = "UTF-8-BOM")
  d  <- merge(d, pr[, c("anio", "region", "arrastre", "oni", "roni_mismo", "prop_reemergente")],
              by = c("anio", "region"), all.x = TRUE)
  d$region <- factor(d$region)
  d$anio_f <- factor(d$anio)
  d$tasa   <- 1e5 * d$casos_escalados / d$poblacion
  d$y_log  <- log1p(d$tasa)                                # escala de evaluación: log(1 + tasa)

  # Estandarización con media y DE de 2015–2025
  pred <- c("arrastre", "oni", "roni_mismo", "prop_reemergente")
  est  <- sapply(subset(d, anio <= 2025)[pred], function(x) c(media = mean(x), de = sd(x)))
  for (v in pred) d[[paste0(v, "_z")]] <- (d[[v]] - est["media", v]) / est["de", v]
  attr(d, "estandarizacion") <- est
  d[order(d$anio, d$region), ]
}

# Ajuste con registro de avisos de convergencia
ajustar <- function(formula, datos) {
  w <- character()
  mod <- withCallingHandlers(glmmTMB(formula, family = nbinom2, data = datos),
                             warning = function(e) { w <<- c(w, conditionMessage(e)); invokeRestart("muffleWarning") })
  attr(mod, "avisos") <- paste(unique(w), collapse = " | ")
  mod
}

sigma_a <- function(mod) attr(VarCorr(mod)$cond$anio_f, "stddev")[[1]]

# Sorteos de la predictiva: β ~ N(β̂, V̂), u ~ N(0, σ̂²_a) común a las regiones del año, y ~ BN2
simular <- function(mod, nuevo, B) {
  beta <- mvrnorm(B, fixef(mod)$cond, vcov(mod)$cond)
  fx   <- formula(mod, fixed.only = TRUE)
  X    <- model.matrix(delete.response(terms(fx)), nuevo)[, names(fixef(mod)$cond), drop = FALSE]
  u    <- rnorm(B, 0, sigma_a(mod))
  eta  <- X %*% t(beta) + matrix(u, nrow(X), B, byrow = TRUE) + log(nuevo$poblacion)
  matrix(rnbinom(length(eta), mu = exp(eta), size = sigma(mod)), nrow(X), B)
}

# -----------------------------------------------------------------------------
# cv_loyo: para cada año 2015–2025 ajusta sin ese año y simula su predictiva
# -----------------------------------------------------------------------------
# Si un predictor queda sin variación en el entrenamiento (p. ej. prop_reemergente
# al sacar 2022), se quita de la fórmula de ese pliegue y se deja constancia.
cv_loyo <- function(formula, datos, B = 5000, anios = 2015:2025) {
  datos <- subset(datos, anio <= 2025)
  out <- lapply(anios, function(a) {
    ent <- droplevels(subset(datos, anio != a)); ent$region <- factor(ent$region, levels = levels(datos$region))
    pru <- subset(datos, anio == a)
    f   <- formula; quitados <- character()
    for (v in grep("_z$", all.vars(formula), value = TRUE))
      if (sd(ent[[v]]) == 0) { f <- update(f, paste(". ~ . -", v)); quitados <- c(quitados, v) }
    mod <- ajustar(f, ent)
    sim <- simular(mod, pru, B)
    sl  <- log1p(1e5 * sim / pru$poblacion)                  # escala log(1 + tasa)
    data.frame(anio = a, region = pru$region, y_log = pru$y_log,
               media_pred = rowMeans(sl),
               ip_inf = apply(sl, 1, quantile, 0.025), ip_sup = apply(sl, 1, quantile, 0.975),
               crps = crps_sample(pru$y_log, sl),
               pdHess = isTRUE(mod$sdr$pdHess), avisos = attr(mod, "avisos"),
               predictores_quitados = paste(quitados, collapse = ";"))
  })
  do.call(rbind, out)
}

# Métricas de la validación (sobre las filas que se le pasen)
metricas_loyo <- function(cv) {
  y <- cv$y_log; yh <- cv$media_pred
  yc  <- y  - ave(y,  cv$region)                           # centrado por región
  yhc <- yh - ave(yh, cv$region)
  data.frame(r2_loyo_total = 1 - sum((y - yh)^2) / sum((y - mean(y))^2),
             r2_loyo_dentro_region = 1 - sum((yc - yhc)^2) / sum(yc^2),
             crps_medio = mean(cv$crps),
             cobertura_ip95 = mean(y >= cv$ip_inf & y <= cv$ip_sup),
             filas_evaluadas = nrow(cv))
}

# Pseudo R² del año (Roback y Legler, sección 8.7.2): reducción relativa de σ²_a frente a M0
pseudo_r2_anio <- function(mod, mod0) (sigma_a(mod0)^2 - sigma_a(mod)^2) / sigma_a(mod0)^2
