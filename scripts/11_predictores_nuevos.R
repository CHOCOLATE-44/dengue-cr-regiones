# Predictores nuevos (PASO 1 y 2): prop_reemergente_a y roni_mismo_a.
# Se ejecuta desde la raíz del proyecto. Escribe en datos/procesados/ y modelo/resultados/.

ser <- read.csv("datos/serotipos_anual.csv", fileEncoding = "UTF-8")

# Presencia por año y serotipo: 1 si alguna fuente lo detecta, 0 si alguna lo descarta, NA si ninguna informa
pres <- aggregate(presente ~ anio + serotipo, data = ser, na.action = na.pass,
                  FUN = function(x) if (any(x %in% 1)) 1 else if (any(x %in% 0)) 0 else NA)

# Porcentaje del informe anual de INCIENSA (fuente principal de proporciones)
pct <- subset(ser, fuente_tipo == "inciensa_anual" & !is.na(porcentaje_reportado) & anio >= 2020,
              c(anio, serotipo, porcentaje_reportado))

# Definición literal: serotipo detectado en a que no circuló en ninguno de los 10 años previos.
# Si en la ventana no hay un 1 y algún año es NA, no se puede decidir -> NA.
reemerge <- function(a, s) {
  v <- pres$presente[pres$serotipo == s & pres$anio %in% (a - 10):(a - 1)]
  if (length(v) < 10) return(NA)
  if (any(v %in% 1)) return(FALSE)
  if (anyNA(v)) return(NA)
  TRUE
}

filas <- lapply(2015:2026, function(a) {
  det <- pres$serotipo[pres$anio == a & pres$presente %in% 1]
  r   <- sapply(det, reemerge, a = a)
  p   <- if (anyNA(r)) NA
         else if (!any(r)) 0                                # ningún serotipo detectado es reemergente
         else {
           q <- pct$porcentaje_reportado[pct$anio == a & pct$serotipo %in% det[r]]
           if (length(q) < sum(r)) NA else sum(q) / 100
         }
  data.frame(anio = a, serotipos_detectados = paste(det, collapse = ";"),
             reemergentes = paste(det[r %in% TRUE], collapse = ";"), prop_reemergente = p)
})
pr_re <- do.call(rbind, filas)
print(pr_re)

# roni_mismo_a: media de las estaciones RONI centradas en enero-agosto del año a (DJF ... JAS)
roni <- read.table("datos/fuentes/extras/RONI.ascii.txt", header = TRUE)
est  <- c("DJF", "JFM", "FMA", "MAM", "AMJ", "MJJ", "JJA", "JAS")
rm_a <- aggregate(ANOM ~ YR, data = subset(roni, SEAS %in% est), FUN = function(x) if (length(x) == 8) mean(x) else NA)
names(rm_a) <- c("anio", "roni_mismo")

# Agregar a predictores (ojo: 06c_construir_predictores.py regenera este archivo sin estas columnas)
f  <- "datos/procesados/predictores_region_anio.csv"
pr <- read.csv(f, fileEncoding = "UTF-8-BOM")
pr <- pr[, setdiff(names(pr), c("prop_reemergente", "roni_mismo", "fuente_prop_reemergente", "fuente_roni_mismo"))]
pr <- merge(pr, pr_re[, c("anio", "prop_reemergente")], by = "anio", all.x = TRUE)
pr <- merge(pr, rm_a, by = "anio", all.x = TRUE)
pr$fuente_prop_reemergente <- "datos/serotipos_anual.csv; definición literal (ausente en los 10 años previos); scripts/11_predictores_nuevos.R"
pr$fuente_roni_mismo <- "NOAA CPC, RONI (ERSSTv6), media DJF-JAS del mismo año; hipótesis post hoc"
pr <- pr[order(pr$anio, pr$region), ]
write.csv(pr, f, row.names = FALSE, fileEncoding = "UTF-8")

dir.create("modelo/resultados", showWarnings = FALSE)
write.csv(pr_re, "modelo/resultados/prop_reemergente_anual.csv", row.names = FALSE)

# Matriz de correlación en 2015-2025 (filas región-año)
b  <- subset(pr, anio <= 2025)
cm <- cor(b[, c("arrastre", "oni", "roni_mismo", "prop_reemergente")], use = "pairwise.complete.obs")
dimnames(cm) <- rep(list(c("arrastre", "roni_previo", "roni_mismo", "prop_reemergente")), 2)
print(round(cm, 3))
write.csv(round(cm, 4), "modelo/resultados/correlacion_predictores.csv")
cat("Filas 2015-2025:", nrow(b), "| NA prop_reemergente:", sum(is.na(b$prop_reemergente)), "\n")
