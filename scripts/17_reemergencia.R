# PARTE B4: reemergencia_a = 1 si en el año a-1 se detectó en Costa Rica un serotipo ausente
# 5 años o más (a-6 ... a-2); 0 en otro caso. Solo presencia, desde datos/serotipos_anual.csv.
# Post hoc: se prueba una sola vez.

ser <- read.csv("datos/serotipos_anual.csv", fileEncoding = "UTF-8")

# Casos importados: no cuentan como detección en circulación local. Regla: columna "importado" (1/0) de
# serotipos_anual.csv, con su fuente en "fuente_importado" (la agrega 34_serotipos_importado.R).
stopifnot("importado" %in% names(ser), all(ser$importado %in% c(0, 1)))
ser$importado <- ser$importado == 1
cat("Filas marcadas como caso importado:\n"); print(ser[ser$importado, c("anio", "serotipo", "nota")])

presencia <- function(datos) aggregate(presente ~ anio + serotipo, data = datos, na.action = na.pass,
  FUN = function(x) if (any(x %in% 1)) 1 else if (any(x %in% 0)) 0 else NA)
pres_local <- presencia(transform(ser, presente = ifelse(importado & presente %in% 1, 0, presente)))
pres_todo  <- presencia(ser)

marcar <- function(pres, a) {
  det <- pres$serotipo[pres$anio == a - 1 & pres$presente %in% 1]
  if (length(det) == 0) return(NA)
  r <- sapply(det, function(s) {
    v <- pres$presente[pres$serotipo == s & pres$anio %in% (a - 6):(a - 2)]
    if (length(v) < 5 || anyNA(v)) { if (any(v %in% 1)) FALSE else NA } else all(v == 0)
  })
  if (any(r %in% TRUE)) 1 else if (anyNA(r)) NA else 0
}
detalle <- function(pres, a) {
  det <- pres$serotipo[pres$anio == a - 1 & pres$presente %in% 1]
  paste(det[sapply(det, function(s) { v <- pres$presente[pres$serotipo == s & pres$anio %in% (a - 6):(a - 2)]
                                        length(v) == 5 && !anyNA(v) && all(v == 0) })], collapse = ";")
}
re <- data.frame(anio = 2015:2026)
re$reemergencia          <- sapply(re$anio, marcar, pres = pres_local)
re$serotipo_reemergente  <- sapply(re$anio, detalle, pres = pres_local)
re$reemergencia_con_importado <- sapply(re$anio, marcar, pres = pres_todo)
print(re)
write.csv(re, "modelo/resultados/reemergencia_anual.csv", row.names = FALSE)
