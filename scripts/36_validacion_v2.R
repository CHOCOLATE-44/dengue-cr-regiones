# Correcciones 1c, 2 y 3: validación v2 con números aleatorios comunes, validación hacia adelante (origen
# móvil) y comparaciones pareadas con IC por bootstrap de años. Todos los modelos: region + offset(log(poblacion))
# + (1 | anio_f), NB2, 2015–2025 (99 filas); estandarización con los años de entrenamiento de cada paso.
#   LOYO: cada año 2015–2025 fuera, entrenando con los demás.
#   Hacia adelante: para a = 2019…2025, entrena con 2015…a−1 y predice a.
#   Las comparaciones pareadas están en 39_comparaciones_pareadas.R.
#   Sorteos: 20 000 por paso. En cada paso, todos los modelos usan los mismos normales estándar para β (las primeras
#   p columnas de una matriz común) y para el efecto de año, y los mismos uniformes para el ruido BN2 (cuantil
#   de la BN2 en el uniforme común).
#   Si un predictor no varía en el entrenamiento de un paso (p. ej. reemergencia antes de 2024), se quita de ese
#   paso y se deja constancia.
# Requiere 12_cv_loyo.R (preparar_datos, ajustar, sigma_a, metricas_loyo) y 25_parte_c_predictores.R (datos_bio).
# No se predice 2026.

library(glmmTMB)
library(scoringRules)

B <- 20000


# --- Datos ---------------------------------------------------------------------
b <- read.csv("datos/procesados/bloques_region_anio.csv", fileEncoding = "UTF-8-BOM")
blq <- function(n, v) setNames(subset(b, bloque == n)[, c("anio", "region", "casos_escalados")], c("anio", "region", v))
reg <- read.csv("datos/procesados/predictores_regionales.csv", fileEncoding = "UTF-8")
rt  <- read.csv("datos/procesados/v2_seco_tiempo_real.csv", fileEncoding = "UTF-8")[, c("anio", "region", "tasa_seco_rt", "vecinos_seco_rt")]
inm <- read.csv("datos/procesados/v2_inmunidad3.csv", fileEncoding = "UTF-8")[, c("anio", "region", "inmunidad3_v2", "inmunidad3_v2h")]
d <- Reduce(function(x, y) merge(x, y, by = c("anio", "region")),
            list(datos_bio(), reg[, c("anio", "region", "vecinos_arrastre", "vecinos_seco", "caribe", "chorotega")],
                 rt, inm, blq("seco", "esc_seco"), blq("lluvioso", "esc_lluv")))
d$region <- factor(d$region); d$anio_f <- factor(d$anio)
d$tasa_seco <- log1p(1e5 * d$esc_seco / d$poblacion)
d <- d[order(d$anio, d$region), ]
stopifnot(nrow(d) == 99, !anyNA(d))
anual <- transform(d, casos = casos_escalados)
pron  <- transform(d, casos = esc_lluv, y_log = log1p(1e5 * esc_lluv / poblacion))
stopifnot(all(anual$casos == round(anual$casos)), all(pron$casos == round(pron$casos)), all(pron$casos >= 0))

# --- Modelos (lista cerrada de las etapas anteriores + versiones v2 pedidas) ------------------------------------
indicadoras <- c("caribe", "chorotega")
clima <- c("arrastre", "oni", "aptitud_termica_pob", "sequia_pob")
modelos <- list(
  anual = list(M0 = character(), M1 = c("arrastre", "oni"), M3b = c("arrastre", "oni", "reemergencia"),
               M_clima = clima, M_bio = c(clima, "reemergencia"),
               A_H1 = c(clima, "inmunidad3_v2"), A_H2 = c(clima, "vecinos_arrastre"),
               A_H12 = c(clima, "inmunidad3_v2", "vecinos_arrastre"),
               A_H3 = c(clima, "sequia_pob:caribe", "oni:caribe"), A_H4 = c(clima, "sequia_pob:chorotega"),
               A_H1h = c(clima, "inmunidad3_v2h")),
  pron = list(P00 = character(), P1min = c("arrastre", "tasa_seco"),
              P_H1 = c("arrastre", "tasa_seco", "inmunidad3_v2"), P_H2 = c("arrastre", "tasa_seco", "vecinos_seco"),
              P_H12 = c("arrastre", "tasa_seco", "inmunidad3_v2", "vecinos_seco"),
              P_H1h = c("arrastre", "tasa_seco", "inmunidad3_v2h"),
              P1min_rt = c("arrastre", "tasa_seco_rt"), P_H1_rt = c("arrastre", "tasa_seco_rt", "inmunidad3_v2"),
              P_H2_rt = c("arrastre", "tasa_seco_rt", "vecinos_seco_rt"),
              P_H12_rt = c("arrastre", "tasa_seco_rt", "inmunidad3_v2", "vecinos_seco_rt")))
# Pares (modelo, referencia): cada modelo frente al modelo del que se deriva
pares <- rbind(
  data.frame(respuesta = "anual", modelo = c("M1", "M3b", "M_clima", "M_bio", "M_bio", "A_H1", "A_H2", "A_H12", "A_H3", "A_H4", "A_H1h"),
             referencia = c("M0", "M1", "M1", "M3b", "M_clima", "M_clima", "M_clima", "M_clima", "M_clima", "M_clima", "M_clima")),
  data.frame(respuesta = "pron", modelo = c("P1min", "P_H1", "P_H2", "P_H12", "P_H1h", "P1min_rt", "P_H1_rt", "P_H2_rt", "P_H12_rt"),
             referencia = c("P00", "P1min", "P1min", "P1min", "P1min", "P00", "P1min_rt", "P1min_rt", "P1min_rt")))

continuas <- function(t) setdiff(unique(unlist(strsplit(t, ":"))), indicadoras)
termino_z <- function(t) sapply(strsplit(t, ":"), function(p) paste(ifelse(p %in% indicadoras, p, paste0(p, "_z")), collapse = ":"))
formula_v2 <- function(t) as.formula(paste("casos ~", paste(c(if (length(t)) termino_z(t),
                                           "region + (1 | anio_f) + offset(log(poblacion))"), collapse = " + ")))

# --- Un paso de validación: ajusta todos los modelos de una respuesta y simula con sorteos comunes ------------
paso <- function(datos, lista, entrenar, probar, semilla) {
  ent0 <- subset(datos, anio %in% entrenar); pru0 <- subset(datos, anio %in% probar)
  set.seed(semilla)
  Zb <- matrix(rnorm(B * 40), B, 40)                  # normales comunes para β (se usan las primeras p columnas)
  Zu <- rnorm(B)                                      # normales comunes para el efecto del año de prueba
  U  <- matrix(runif(nrow(pru0) * B), nrow(pru0), B)  # uniformes comunes para el ruido BN2
  out <- lapply(names(lista), function(k) {
    t <- lista[[k]]; ent <- ent0; pru <- pru0; quitados <- character()
    for (v in continuas(t)) {
      m <- mean(ent[[v]]); s <- sd(ent[[v]])
      if (s == 0) { quitados <- c(quitados, v); next }
      ent[[paste0(v, "_z")]] <- (ent[[v]] - m) / s; pru[[paste0(v, "_z")]] <- (pru[[v]] - m) / s
    }
    if (length(quitados)) t <- t[!sapply(strsplit(t, ":"), function(p) any(p %in% quitados))]
    ent <- droplevels(ent); ent$region <- factor(ent$region, levels = levels(datos$region))
    mod <- ajustar(formula_v2(t), ent)
    bh <- fixef(mod)$cond; V <- vcov(mod)$cond
    X <- model.matrix(delete.response(terms(formula(mod, fixed.only = TRUE))), pru)[, names(bh), drop = FALSE]
    L <- t(chol(V + diag(1e-12, nrow(V))))
    beta <- sweep(Zb[, seq_along(bh), drop = FALSE] %*% t(L), 2, bh, "+")       # B × p
    eta <- X %*% t(beta) + matrix(Zu * sigma_a(mod), nrow(X), B, byrow = TRUE) + log(pru$poblacion)
    y <- matrix(qnbinom(U, mu = exp(eta), size = sigma(mod)), nrow(X), B)
    sl <- log1p(1e5 * y / pru$poblacion)
    data.frame(modelo = k, anio = pru$anio, region = pru$region, y_log = pru$y_log, media_pred = rowMeans(sl),
               ip_inf = apply(sl, 1, quantile, 0.025), ip_sup = apply(sl, 1, quantile, 0.975),
               crps = crps_sample(pru$y_log, sl), se = (pru$y_log - rowMeans(sl))^2,
               sigma_a = sigma_a(mod), pdHess = isTRUE(mod$sdr$pdHess), quitados = paste(quitados, collapse = ";"),
               avisos = attr(mod, "avisos"))
  })
  do.call(rbind, out)
}

validar <- function(esquema) {
  pasos <- if (esquema == "LOYO") lapply(2015:2025, function(a) list(ent = setdiff(2015:2025, a), pru = a))
           else lapply(2019:2025, function(a) list(ent = 2015:(a - 1), pru = a))
  do.call(rbind, lapply(names(modelos), function(r) {
    datos <- if (r == "anual") anual else pron
    do.call(rbind, lapply(pasos, function(p)
      cbind(esquema = esquema, respuesta = r, paso(datos, modelos[[r]], p$ent, p$pru, semilla = 2026 * 100 + p$pru))))
  }))
}
pred <- rbind(validar("LOYO"), validar("hacia_adelante"))
write.csv(pred, "modelo/resultados/v2_predicciones_loyo_y_adelante.csv", row.names = FALSE, fileEncoding = "UTF-8")

# --- Métricas por modelo y esquema (y solo 2021–2025 para la sensibilidad del tiempo real) --------------------
metricas <- function(x) cbind(metricas_loyo(x), data.frame(
  pasos_sigma_a_menor_005 = length(unique(x$anio[x$sigma_a < 0.05])), pasos_sin_pdHess = length(unique(x$anio[!x$pdHess])),
  pasos_con_predictor_quitado = length(unique(x$anio[nzchar(x$quitados)]))))
tabla <- do.call(rbind, lapply(split(pred, list(pred$esquema, pred$respuesta, pred$modelo), drop = TRUE), function(x)
  cbind(esquema = x$esquema[1], respuesta = x$respuesta[1], modelo = x$modelo[1], anios = paste(range(x$anio), collapse = "–"), metricas(x))))
sub <- subset(pred, respuesta == "pron" & anio >= 2021)
tabla_2125 <- do.call(rbind, lapply(split(sub, list(sub$esquema, sub$modelo), drop = TRUE), function(x)
  cbind(esquema = x$esquema[1], respuesta = "pron", modelo = x$modelo[1], anios = "2021–2025", metricas(x))))
tabla_v2 <- rbind(tabla, tabla_2125)
rownames(tabla_v2) <- NULL
write.csv(tabla_v2, "modelo/resultados/v2_tabla_metricas_loyo_y_adelante.csv", row.names = FALSE, fileEncoding = "UTF-8")

options(width = 220)
print(subset(tabla_v2, select = c(esquema, respuesta, modelo, anios, r2_loyo_total, r2_loyo_dentro_region, crps_medio,
                                  cobertura_ip95, filas_evaluadas, pasos_sigma_a_menor_005, pasos_con_predictor_quitado)), digits = 3)
