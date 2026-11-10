# Sección 18: base final por región y año, la misma con que se ajustó el modelo final (41, 43) y se evaluó 2026 (44).
#   Filas: 9 regiones × 2015–2026 = 108 (2015–2025 para el ajuste; 2026 para la evaluación).
#   Mismas funciones que los scripts del modelo: preparar_datos() (12_cv_loyo.R) y datos_bio() (25_parte_c_predictores.R),
#   más predictores_bio.csv para 2026 (como 44) y v2_inmunidad3.csv (como 41; no tiene 2026).
#   Estandarización (_z) con la media y la DE de las 99 filas de 2015–2025, igual que 41 y 44.
# Verificación: se lee de nuevo el CSV, se reajusta S = arrastre + aptitud térmica (NB2, ML, como 41) con 2015–2025 y
#   se compara con s13_explicacion_coeficientes.csv (diferencia máxima < 1e-8).
# Salidas: datos/procesados/base_region_anio.csv y modelo/resultados/s55_verificacion_base.csv.
library(glmmTMB)
solo_funciones <- function(f) for (e in parse(f, encoding = "UTF-8"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function")))
    eval(e, globalenv())
for (f in c("scripts/12_cv_loyo.R", "scripts/25_parte_c_predictores.R")) solo_funciones(f)

bio <- read.csv("datos/procesados/predictores_bio.csv", fileEncoding = "UTF-8")
inm <- read.csv("datos/procesados/v2_inmunidad3.csv", fileEncoding = "UTF-8")[, c("anio", "region", "inmunidad3_v2")]
ajuste <- datos_bio()
n26 <- merge(subset(preparar_datos(), anio == 2026), subset(bio, anio == 2026)[, setdiff(names(bio), "fuente")], by = c("region", "anio"))
cols <- c("region", "anio", "casos_escalados", "casos_crudos", "poblacion", "arrastre", "aptitud_termica_pob", "sequia_pob",
          "oni", "reemergencia")
base <- rbind(ajuste[, cols], n26[, cols])
base$region <- as.character(base$region)
base <- merge(base, inm, by = c("anio", "region"), all.x = TRUE)
base <- base[order(base$anio, base$region), c(cols, "inmunidad3_v2")]
names(base)[names(base) == "oni"] <- "roni_previo"
base$conjunto <- ifelse(base$anio <= 2025, "ajuste", "evaluacion_2026")
cont <- c("arrastre", "aptitud_termica_pob", "sequia_pob", "roni_previo", "inmunidad3_v2", "reemergencia")
a <- base$anio <= 2025
for (v in cont) base[[paste0(v, "_z")]] <- (base[[v]] - mean(base[[v]][a])) / sd(base[[v]][a])
stopifnot(nrow(base) == 108, sum(a) == 99, !anyNA(base[a, ]), all(is.na(base$inmunidad3_v2[!a])),
          !anyNA(base[!a, setdiff(names(base), c("inmunidad3_v2", "inmunidad3_v2_z"))]))
write.csv(base, "datos/procesados/base_region_anio.csv", row.names = FALSE, fileEncoding = "UTF-8")

# Verificación con el archivo leído de vuelta
d <- read.csv("datos/procesados/base_region_anio.csv", fileEncoding = "UTF-8")
d <- subset(d, conjunto == "ajuste")
d$region <- factor(d$region); d$anio_f <- factor(d$anio); d$casos <- d$casos_escalados
m <- glmmTMB(casos ~ arrastre_z + aptitud_termica_pob_z + region + (1 | anio_f) + offset(log(poblacion)),
             family = nbinom2, data = d)
s13 <- read.csv("modelo/resultados/s13_explicacion_coeficientes.csv", fileEncoding = "UTF-8")
b <- fixef(m)$cond; e <- sqrt(diag(vcov(m)$cond))
ver <- data.frame(termino = s13$termino, estimado_s13 = s13$estimado, estimado_base = unname(b[s13$termino]),
                  ee_s13 = s13$ee, ee_base = unname(e[s13$termino]))
ver$dif_estimado <- abs(ver$estimado_base - ver$estimado_s13); ver$dif_ee <- abs(ver$ee_base - ver$ee_s13)
print(ver, digits = 10)
cat("Diferencia máxima: estimado", max(ver$dif_estimado), " EE", max(ver$dif_ee), "\n")
write.csv(ver, "modelo/resultados/s55_verificacion_base.csv", row.names = FALSE, fileEncoding = "UTF-8")
stopifnot(nrow(ver) == 11, !anyNA(ver), max(ver$dif_estimado) < 1e-8, max(ver$dif_ee) < 1e-8)
