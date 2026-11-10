# Corrección 3: comparaciones pareadas sobre las predicciones de 36_validacion_v2.R (mismos sorteos comunes).
#   Diferencias por año de CRPS y de error cuadrático; IC 95 % de la diferencia media de CRPS y de la diferencia
#   de R² dentro de región por bootstrap de AÑOS (2000 réplicas). "mejora" solo si el IC del CRPS excluye 0 a su
#   favor; si lo excluye en contra, "empeora"; si no, "no se distingue de la referencia".
#   Réplicas en que todos los años remuestreados son el mismo: el R² dentro de región es 0/0 y se descartan
#   (se cuentan en la columna replicas_r2_descartadas); el IC del CRPS usa todas.
REPS <- 2000
pred <- read.csv("modelo/resultados/v2_predicciones_loyo_y_adelante.csv", fileEncoding = "UTF-8")

# Pares (modelo, referencia): cada modelo frente al modelo del que se deriva
pares <- rbind(
  data.frame(respuesta = "anual", modelo = c("M1", "M3b", "M_clima", "M_bio", "M_bio", "A_H1", "A_H2", "A_H12", "A_H3", "A_H4", "A_H1h"),
             referencia = c("M0", "M1", "M1", "M3b", "M_clima", "M_clima", "M_clima", "M_clima", "M_clima", "M_clima", "M_clima")),
  data.frame(respuesta = "pron", modelo = c("P1min", "P_H1", "P_H2", "P_H12", "P_H1h", "P1min_rt", "P_H1_rt", "P_H2_rt", "P_H12_rt"),
             referencia = c("P00", "P1min", "P1min", "P1min", "P1min", "P00", "P1min_rt", "P1min_rt", "P1min_rt")))
r2_dentro <- function(x) { yc <- x$y_log - ave(x$y_log, x$region); yhc <- x$media_pred - ave(x$media_pred, x$region)
                           1 - sum((yc - yhc)^2) / sum(yc^2) }
comparar <- function(esq, resp, m, ref, anios = NULL) {
  a <- subset(pred, esquema == esq & respuesta == resp & modelo == m)
  r <- subset(pred, esquema == esq & respuesta == resp & modelo == ref)
  if (!is.null(anios)) { a <- subset(a, anio %in% anios); r <- subset(r, anio %in% anios) }
  stopifnot(identical(paste(a$anio, a$region), paste(r$anio, r$region)))
  por_anio <- data.frame(anio = sort(unique(a$anio)),
                         dif_crps = as.numeric(tapply(a$crps - r$crps, a$anio, mean)),
                         dif_ecm = as.numeric(tapply(a$se - r$se, a$anio, mean)))
  yrs <- por_anio$anio
  set.seed(7)
  boot <- t(replicate(REPS, {
    s <- sample(yrs, replace = TRUE)
    ia <- unlist(lapply(seq_along(s), function(j) which(a$anio == s[j])))
    aa <- a[ia, ]; rr <- r[ia, ]
    # cada año remuestreado cuenta como "región-año" distinto para el centrado por región
    c(crps = mean(por_anio$dif_crps[match(s, yrs)]), r2 = r2_dentro(aa) - r2_dentro(rr))
  }))
  data.frame(esquema = esq, respuesta = resp, modelo = m, referencia = ref,
             anios = if (is.null(anios)) paste(range(yrs), collapse = "–") else paste(range(anios), collapse = "–"),
             n_anios = length(yrs), dif_crps = mean(por_anio$dif_crps),
             ic_crps_inf = quantile(boot[, "crps"], 0.025), ic_crps_sup = quantile(boot[, "crps"], 0.975),
             dif_ecm = mean(por_anio$dif_ecm),
             dif_r2_dentro = r2_dentro(a) - r2_dentro(r),
             ic_r2_inf = quantile(boot[, "r2"], 0.025, na.rm = TRUE), ic_r2_sup = quantile(boot[, "r2"], 0.975, na.rm = TRUE),
             replicas_r2_descartadas = sum(!is.finite(boot[, "r2"])),
             anios_mejor_crps = sum(por_anio$dif_crps < 0),
             veredicto = ifelse(quantile(boot[, "crps"], 0.975) < 0, "mejora",
                                ifelse(quantile(boot[, "crps"], 0.025) > 0, "empeora", "no se distingue de la referencia")),
             row.names = NULL)
}
comp <- do.call(rbind, lapply(c("LOYO", "hacia_adelante"), function(esq)
  do.call(rbind, lapply(seq_len(nrow(pares)), function(i) comparar(esq, pares$respuesta[i], pares$modelo[i], pares$referencia[i])))))
extra <- rbind(  # tiempo real frente a escalado, y sensibilidad 2021–2025 (predictor realmente en tiempo real)
  do.call(rbind, lapply(c("LOYO", "hacia_adelante"), function(esq) comparar(esq, "pron", "P1min_rt", "P1min"))),
  do.call(rbind, lapply(c("LOYO", "hacia_adelante"), function(esq) do.call(rbind, lapply(
    which(pares$respuesta == "pron" & grepl("_rt$", pares$modelo)), function(i)
      comparar(esq, "pron", pares$modelo[i], pares$referencia[i], anios = 2021:2025))))))
comp <- rbind(comp, extra)
write.csv(comp, "modelo/resultados/v2_comparaciones_pareadas.csv", row.names = FALSE, fileEncoding = "UTF-8")

options(width = 220)
print(subset(comp, select = c(esquema, modelo, referencia, anios, dif_crps, ic_crps_inf, ic_crps_sup, dif_r2_dentro,
                              ic_r2_inf, ic_r2_sup, replicas_r2_descartadas, anios_mejor_crps, n_anios, veredicto)), digits = 3)
