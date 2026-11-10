# Hipótesis regionales H1–H4 sobre los dos modelos finales (lista cerrada, definida antes de ver resultados).
# Todos: region + offset(log(poblacion)) + (1 | anio_f), NB2, 2015–2025 (99 filas), estandarización dentro de
# cada pliegue LOYO (las indicadoras caribe/chorotega no se estandarizan). 2026 no se usa ni se predice.
# Anual (SE 1–40), base M_clima; pronóstico (SE 19–40), base P1min. Requiere 12_cv_loyo.R y 25_parte_c_predictores.R.

library(glmmTMB)

set.seed(2026)
reg <- read.csv("datos/procesados/predictores_regionales.csv", fileEncoding = "UTF-8")
b <- read.csv("datos/procesados/bloques_region_anio.csv", fileEncoding = "UTF-8-BOM")
seco <- setNames(subset(b, bloque == "seco")[, c("anio", "region", "casos_escalados")], c("anio", "region", "esc_seco"))
lluv <- setNames(subset(b, bloque == "lluvioso")[, c("anio", "region", "casos_escalados")], c("anio", "region", "esc_lluv"))
d <- merge(merge(merge(datos_bio(), reg[, c("anio", "region", "inmunidad3", "vecinos_arrastre", "vecinos_seco", "caribe", "chorotega")],
                       by = c("anio", "region")), seco, by = c("anio", "region")), lluv, by = c("anio", "region"))
d$region <- factor(d$region); d$anio_f <- factor(d$anio)
d$tasa_seco <- log1p(1e5 * d$esc_seco / d$poblacion)
d <- d[order(d$anio, d$region), ]
stopifnot(nrow(d) == 99)
anual <- transform(d, casos = casos_escalados)                                     # SE 1–40 (y_log ya es SE 1–40)
pron  <- transform(d, casos = esc_lluv, y_log = log1p(1e5 * esc_lluv / poblacion))  # SE 19–40

indicadoras <- c("caribe", "chorotega")
base_a <- c("arrastre", "oni", "aptitud_termica_pob", "sequia_pob")
base_p <- c("arrastre", "tasa_seco")
lista_a <- list(M0 = character(), M_clima = base_a,
                A_H1 = c(base_a, "inmunidad3"), A_H2 = c(base_a, "vecinos_arrastre"),
                A_H12 = c(base_a, "inmunidad3", "vecinos_arrastre"),
                A_H3 = c(base_a, "sequia_pob:caribe", "oni:caribe"), A_H4 = c(base_a, "sequia_pob:chorotega"))
lista_p <- list(P00 = character(), P1min = base_p,
                P_H1 = c(base_p, "inmunidad3"), P_H2 = c(base_p, "vecinos_seco"),
                P_H12 = c(base_p, "inmunidad3", "vecinos_seco"))
# Signo esperado de cada término nuevo (H3: la interacción con caribe debe ir contra el término principal)
esperado <- c(inmunidad3 = "negativo", vecinos_arrastre = "positivo", vecinos_seco = "positivo",
              "sequia_pob:caribe" = "opuesto a sequia_pob", "oni:caribe" = "opuesto a oni", "sequia_pob:chorotega" = "positivo")

continuas <- function(t) setdiff(unique(unlist(strsplit(t, ":"))), indicadoras)
termino_z <- function(t) sapply(strsplit(t, ":"), function(p) paste(ifelse(p %in% indicadoras, p, paste0(p, "_z")), collapse = ":"))
formula_h <- function(t) as.formula(paste("casos ~", paste(c(if (length(t)) termino_z(t),
                                         "region + (1 | anio_f) + offset(log(poblacion))"), collapse = " + ")))
estandarizar <- function(ent, pru, vars) {
  for (v in vars) { m <- mean(ent[[v]]); s <- sd(ent[[v]])
    ent[[paste0(v, "_z")]] <- (ent[[v]] - m) / s; pru[[paste0(v, "_z")]] <- (pru[[v]] - m) / s }
  list(ent = ent, pru = pru)
}
cv_h <- function(datos, t, B = 5000) {
  res <- lapply(2015:2025, function(a) {
    z <- estandarizar(subset(datos, anio != a), subset(datos, anio == a), continuas(t))
    ent <- droplevels(z$ent); ent$region <- factor(ent$region, levels = levels(datos$region))
    mod <- ajustar(formula_h(t), ent)
    sl  <- log1p(1e5 * simular(mod, z$pru, B) / z$pru$poblacion)
    list(pred = data.frame(anio = a, region = z$pru$region, y_log = z$pru$y_log, media_pred = rowMeans(sl),
                           ip_inf = apply(sl, 1, quantile, 0.025), ip_sup = apply(sl, 1, quantile, 0.975),
                           crps = scoringRules::crps_sample(z$pru$y_log, sl)),
         coef = data.frame(anio_fuera = a, termino = names(fixef(mod)$cond), estimado = fixef(mod)$cond),
         diag = data.frame(anio_fuera = a, sigma_a = sigma_a(mod), pdHess = isTRUE(mod$sdr$pdHess)))
  })
  lapply(c(pred = "pred", coef = "coef", diag = "diag"), function(k) do.call(rbind, lapply(res, `[[`, k)))
}
vif <- function(datos, vars) sapply(vars, function(v)
  1 / (1 - summary(lm(reformulate(setdiff(vars, v), v), data = datos))$r.squared))

evaluar <- function(datos, lista, respuesta, ref) {
  completo <- estandarizar(datos, datos, unique(continuas(unlist(lista))))$ent
  aj  <- lapply(lista, function(t) ajustar(formula_h(t), completo))
  cvs <- lapply(lista, function(t) cv_h(datos, t))
  tabla <- do.call(rbind, lapply(names(lista), function(k) {
    m <- aj[[k]]; dg <- cvs[[k]]$diag
    cbind(data.frame(respuesta = respuesta, modelo = k, evaluacion = "LOYO", pseudo_r2_anio = pseudo_r2_anio(m, aj[[ref]])),
          metricas_loyo(cvs[[k]]$pred),
          data.frame(sigma_a = sigma_a(m), AIC = AIC(m), pdHess = isTRUE(m$sdr$pdHess),
                     E1_aparecio = !isTRUE(m$sdr$pdHess) || sigma_a(m) < 0.05 || any(dg$sigma_a < 0.05) || any(!dg$pdHess),
                     E1_pliegues_sigma_a_menor_005 = sum(dg$sigma_a < 0.05)))
  }))
  coefs <- do.call(rbind, lapply(names(lista)[lengths(lista) > 0], function(k) {
    s <- summary(aj[[k]])$coefficients$cond
    do.call(rbind, lapply(lista[[k]], function(t) {
      tz <- termino_z(t); x <- cvs[[k]]$coef$estimado[cvs[[k]]$coef$termino == tz]
      data.frame(respuesta = respuesta, modelo = k, termino = t, estimado = s[tz, "Estimate"], ee = s[tz, "Std. Error"],
                 p = s[tz, "Pr(>|z|)"], min_pliegues = min(x), max_pliegues = max(x), cambia_signo = min(x) < 0 && max(x) > 0)
    }))
  }))
  tabla$E2_aparecio <- sapply(tabla$modelo, function(k) if (k == ref) NA else any(coefs$cambia_signo[coefs$modelo == k]))
  pred <- do.call(rbind, lapply(names(cvs), function(k) cbind(respuesta = respuesta, modelo = k, cvs[[k]]$pred)))
  list(tabla = tabla, coefs = coefs, pred = pred)
}
techo <- function(datos, nombre, respuesta) {
  m <- ajustar(casos ~ region + anio_f + offset(log(poblacion)), datos)
  yh <- log1p(1e5 * fitted(m) / datos$poblacion)
  yc <- datos$y_log - ave(datos$y_log, datos$region); yhc <- yh - ave(yh, datos$region)
  data.frame(respuesta = respuesta, modelo = nombre, evaluacion = "en la muestra", pseudo_r2_anio = NA,
             r2_loyo_total = 1 - sum((datos$y_log - yh)^2) / sum((datos$y_log - mean(datos$y_log))^2),
             r2_loyo_dentro_region = 1 - sum((yc - yhc)^2) / sum(yc^2), crps_medio = NA, cobertura_ip95 = NA,
             filas_evaluadas = nrow(datos), sigma_a = NA, AIC = AIC(m), pdHess = isTRUE(m$sdr$pdHess),
             E1_aparecio = NA, E1_pliegues_sigma_a_menor_005 = NA, E2_aparecio = NA)
}

ra <- evaluar(anual, lista_a, "anual SE 1–40", "M0")
rp <- evaluar(pron, lista_p, "pronóstico SE 19–40", "P00")
tabla_h <- rbind(ra$tabla, techo(anual, "Techo", "anual SE 1–40"), rp$tabla, techo(pron, "TechoP", "pronóstico SE 19–40"))
for (u in c(0.60, 0.70)) {
  tabla_h[[sprintf("pseudo_r2_ge_%03d", u * 100)]] <- tabla_h$pseudo_r2_anio >= u
  tabla_h[[sprintf("r2_dentro_ge_%03d", u * 100)]] <- tabla_h$r2_loyo_dentro_region >= u
}

# Signo de los términos nuevos frente al esperado (en el ajuste completo)
coefs <- rbind(ra$coefs, rp$coefs)
coefs$esperado <- esperado[coefs$termino]
signo <- function(x) ifelse(x > 0, "positivo", "negativo")
coefs$signo_observado <- signo(coefs$estimado)
coefs$coincide_esperado <- NA
for (i in which(!is.na(coefs$esperado))) {
  e <- coefs$esperado[i]
  coefs$coincide_esperado[i] <- if (startsWith(e, "opuesto a ")) {
    principal <- coefs$estimado[coefs$modelo == coefs$modelo[i] & coefs$termino == sub("opuesto a ", "", e)]
    sign(coefs$estimado[i]) != sign(principal)
  } else coefs$signo_observado[i] == e
}

# VIF de los modelos con inmunidad3 (predictores continuos, 99 filas) y correlaciones pedidas
vifs <- rbind(
  data.frame(modelo = "A_H1",  variable = c(base_a, "inmunidad3"), VIF = vif(anual, c(base_a, "inmunidad3"))),
  data.frame(modelo = "A_H12", variable = c(base_a, "inmunidad3", "vecinos_arrastre"), VIF = vif(anual, c(base_a, "inmunidad3", "vecinos_arrastre"))),
  data.frame(modelo = "P_H1",  variable = c(base_p, "inmunidad3"), VIF = vif(pron, c(base_p, "inmunidad3"))),
  data.frame(modelo = "P_H12", variable = c(base_p, "inmunidad3", "vecinos_seco"), VIF = vif(pron, c(base_p, "inmunidad3", "vecinos_seco"))))
vifs$VIF_mayor_5 <- vifs$VIF > 5
cors <- data.frame(par = c("inmunidad3 ~ arrastre", "inmunidad3 ~ tasa_seco", "vecinos_arrastre ~ arrastre", "vecinos_seco ~ tasa_seco"),
                   r = c(cor(d$inmunidad3, d$arrastre), cor(d$inmunidad3, d$tasa_seco),
                         cor(d$vecinos_arrastre, d$arrastre), cor(d$vecinos_seco, d$tasa_seco)))

options(width = 200)
cat("\nTabla H (Techo y TechoP en la muestra)\n"); print(tabla_h, digits = 3)
cat("\nCoeficientes (escala estandarizada; signo esperado de los términos nuevos)\n"); print(coefs, digits = 3)
cat("\nVIF\n"); print(vifs, digits = 3); cat("\nCorrelaciones\n"); print(cors, digits = 3)
write.csv(tabla_h, "modelo/resultados/tabla_h_hipotesis_regionales.csv", row.names = FALSE)
write.csv(coefs, "modelo/resultados/h_coeficientes_signos.csv", row.names = FALSE)
write.csv(vifs, "modelo/resultados/h_vif.csv", row.names = FALSE)
write.csv(cors, "modelo/resultados/h_correlaciones.csv", row.names = FALSE)
write.csv(rbind(ra$pred, rp$pred), "modelo/resultados/h_cv_loyo_predicciones.csv", row.names = FALSE)
