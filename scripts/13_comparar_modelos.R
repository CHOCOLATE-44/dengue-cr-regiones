# Comparación de modelos M0, M1, M3 y M4 (PASO 5). Ajuste en 2015–2025; 2026 no se usa.
# Requiere scripts/12_cv_loyo.R cargado.

set.seed(2026)
d    <- preparar_datos()
base <- subset(d, anio <= 2025)

comun <- "region + (1 | anio_f) + offset(log(poblacion))"
formulas <- list(
  M0 = as.formula(paste("casos_escalados ~", comun)),
  M1 = as.formula(paste("casos_escalados ~ arrastre_z + oni_z +", comun)),
  M3 = as.formula(paste("casos_escalados ~ arrastre_z + oni_z + prop_reemergente_z +", comun)),
  M4 = as.formula(paste("casos_escalados ~ arrastre_z + oni_z + roni_mismo_z +", comun)))

# Subconjunto común: filas sin NA en ninguna variable de ningún modelo
vars <- unique(unlist(lapply(formulas, all.vars)))
base <- base[complete.cases(base[, vars]), ]
cat("Filas del subconjunto común (2015–2025):", nrow(base), "\n")
stopifnot(nrow(base) == 99)

ajustes <- lapply(formulas, ajustar, datos = base)
for (k in names(ajustes)) {
  cat("\n=====", k, "=====\n"); print(summary(ajustes[[k]]))
  if (nzchar(attr(ajustes[[k]], "avisos"))) cat("AVISOS:", attr(ajustes[[k]], "avisos"), "\n")
}

cvs <- lapply(names(formulas), function(k) cbind(modelo = k, cv_loyo(formulas[[k]], base, B = 5000)))
names(cvs) <- names(formulas)
write.csv(do.call(rbind, cvs), "modelo/resultados/cv_loyo_predicciones.csv", row.names = FALSE)

tabla <- function(filtro) {
  do.call(rbind, lapply(names(formulas), function(k) {
    m <- ajustes[[k]]
    cbind(data.frame(modelo = k,
                     pseudo_r2_anio = pseudo_r2_anio(m, ajustes$M0)),
          metricas_loyo(subset(cvs[[k]], filtro(anio))),
          data.frame(sigma_a = sigma_a(m), AIC = AIC(m), filas_ajuste = nobs(m),
                     pdHess = isTRUE(m$sdr$pdHess)))
  }))
}
marcar <- function(t) {
  t$pseudo_r2_ge_070 <- t$pseudo_r2_anio >= 0.70; t$pseudo_r2_ge_090 <- t$pseudo_r2_anio >= 0.90
  t$r2_dentro_ge_070 <- t$r2_loyo_dentro_region >= 0.70; t$r2_dentro_ge_090 <- t$r2_loyo_dentro_region >= 0.90
  t
}

t1 <- marcar(tabla(function(a) rep(TRUE, length(a))))
t2 <- marcar(tabla(function(a) a != 2023))      # 2023 fuera solo de la evaluación; ajustes iguales
cat("\nTabla 1 (evaluación 2015–2025)\n"); print(t1, digits = 3)
cat("\nTabla 2 (evaluación sin 2023)\n");  print(t2, digits = 3)
write.csv(t1, "modelo/resultados/tabla1_comparacion_modelos.csv", row.names = FALSE)
write.csv(t2, "modelo/resultados/tabla2_comparacion_sin2023.csv", row.names = FALSE)

# Coeficientes fijos de cada modelo (escala estandarizada)
coefs <- do.call(rbind, lapply(names(ajustes), function(k) {
  s <- summary(ajustes[[k]])$coefficients$cond
  data.frame(modelo = k, termino = rownames(s), s, row.names = NULL)
}))
write.csv(coefs, "modelo/resultados/coeficientes_modelos.csv", row.names = FALSE)

# Pliegues con predictores quitados o avisos
cat("\nPliegues con predictores quitados o avisos:\n")
pl <- unique(do.call(rbind, cvs)[, c("modelo", "anio", "pdHess", "avisos", "predictores_quitados")])
print(subset(pl, !pdHess | nzchar(avisos) | nzchar(predictores_quitados)))
