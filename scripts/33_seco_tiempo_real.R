# Corrección tras revisión externa, punto 1 (b y c): bloque seco en tiempo real y sesgo por depuración.
#   1b. Para 2019–2025 compara el acumulado crudo a la SE 18 del boletín con corte en la SE 18 (tiempo real)
#       con lo que muestran los boletines posteriores del mismo año (mínimo acumulado posterior y SE 40), y el
#       total nacional del boletín a la SE 18 con el acumulado OPS (serie revisada) a la SE 18.
#   1c. tasa_seco_rt = log(1 + tasa SE 1–18 con casos CRUDOS del boletín con corte en la SE 18 del año).
#       2015–2020 no tienen boletín en tiempo real: se usa el acumulado crudo del Excel ASIS a la SE 18,
#       marcado "consolidado, no en tiempo real". vecinos_seco_rt con la misma lógica.
# Requiere scripts/12_cv_loyo.R cargado (preparar_datos). 2026 no se usa.

library(readxl)

REG <- c("Brunca", "C.Este", "Central Norte", "Central Sur", "Chorotega", "H. Caribe", "H.Norte", "Occidente", "Pacífico Central")
f_src <- "datos/fuentes/dengue_semanal_CR_2015-2026.xlsx"
ops <- setNames(as.data.frame(read_excel(f_src, "Serie semanal"))[, 1:3], c("anio", "se", "ops"))
ops_se18 <- tapply(ops$ops[ops$se <= 18], ops$anio[ops$se <= 18], sum)
asis <- setNames(as.data.frame(read_excel(f_src, "Regiones MinSalud"))[, 1:4], c("anio", "se", "region", "casos"))
asis18 <- aggregate(casos ~ anio + region, subset(asis, se <= 18), sum)

bol <- read.csv("datos/procesados/boletines_regiones_2021-2026.csv", fileEncoding = "UTF-8-BOM")
bol <- subset(bol, nivel == "region" & usar_en_analisis & !is.na(se_corte) & unidad %in% REG)
bol$id <- paste(bol$anio, bol$tipo_boletin, bol$numero, bol$url)
completos <- names(which(tapply(bol$unidad, bol$id, function(u) setequal(u, REG) && length(u) == 9)))
bol <- subset(bol, id %in% completos)

# Boletín con corte exactamente en la SE 18 de cada año (uno por año en 2021–2025)
rt <- subset(bol, se_corte == 18 & anio_datos %in% 2021:2025)
stopifnot(all(table(rt$anio_datos) == 9))
rt <- data.frame(anio = rt$anio_datos, region = rt$unidad, crudo_rt = rt$casos_acum,
                 fuente_rt = sprintf("Boletín %s N°%s de %d (SE 18), p. %d; publicado %s", rt$tipo_boletin, rt$numero,
                                     rt$anio, rt$pagina_pdf, rt$fecha_publicacion))

# 1b. Boletines posteriores del mismo año (SE 19 al cierre)
post <- subset(bol, anio_datos %in% 2021:2025 & se_corte > 18)
tab1b <- do.call(rbind, lapply(split(post, paste(post$anio_datos, post$unidad)), function(x) {
  x <- x[order(x$se_corte), ]
  data.frame(anio = x$anio_datos[1], region = x$unidad[1], boletines_posteriores = nrow(x),
             minimo_acumulado_posterior = min(x$casos_acum), se_del_minimo = x$se_corte[which.min(x$casos_acum)],
             acumulado_se40 = if (any(x$se_corte == 40)) x$casos_acum[x$se_corte == 40][1] else NA)
}))
tab1b <- merge(rt, tab1b, by = c("anio", "region"))
tab1b$dif_minimo_posterior_menos_rt <- tab1b$minimo_acumulado_posterior - tab1b$crudo_rt
tab1b$dif_se40_menos_rt <- tab1b$acumulado_se40 - tab1b$crudo_rt
tab1b$anomalo <- tab1b$dif_minimo_posterior_menos_rt < 0
asis_cons <- subset(asis18, anio %in% 2019:2020)
tab1b <- rbind(
  data.frame(anio = asis_cons$anio, region = asis_cons$region, crudo_rt = NA,
             fuente_rt = "sin boletín con corte en la SE 18; solo Excel ASIS consolidado",
             boletines_posteriores = NA, minimo_acumulado_posterior = asis_cons$casos, se_del_minimo = NA,
             acumulado_se40 = NA, dif_minimo_posterior_menos_rt = NA, dif_se40_menos_rt = NA, anomalo = NA),
  tab1b)
tab1b <- tab1b[order(tab1b$anio, tab1b$region), ]
print(tab1b[, c("anio", "region", "crudo_rt", "minimo_acumulado_posterior", "se_del_minimo", "dif_minimo_posterior_menos_rt",
                "acumulado_se40", "dif_se40_menos_rt")], row.names = FALSE)
write.csv(tab1b, "modelo/resultados/v2_1b_depuracion_bloque_seco.csv", row.names = FALSE, fileEncoding = "UTF-8")
x <- subset(tab1b, !is.na(crudo_rt))
res1b <- do.call(rbind, lapply(split(x, x$anio), function(z) data.frame(
  anio = z$anio[1], regiones = nrow(z), regiones_con_minimo_posterior_menor = sum(z$anomalo),
  suma_bajas = sum(pmin(z$dif_minimo_posterior_menos_rt, 0)), mayor_baja = min(z$dif_minimo_posterior_menos_rt),
  regiones_se40_menor_que_rt = sum(z$dif_se40_menos_rt < 0, na.rm = TRUE))))
print(res1b, row.names = FALSE)
write.csv(res1b, "modelo/resultados/v2_1b_resumen_por_anio.csv", row.names = FALSE)

# Nacional: total del boletín a la SE 18 frente a la serie OPS revisada a la SE 18
nac <- aggregate(crudo_rt ~ anio, rt, sum)
nac$ops_revisada_se18 <- as.numeric(ops_se18[as.character(nac$anio)])
nac$dif_ops_menos_boletin <- nac$ops_revisada_se18 - nac$crudo_rt
nac$razon_ops_boletin <- nac$ops_revisada_se18 / nac$crudo_rt
print(nac)
write.csv(nac, "modelo/resultados/v2_1b_nacional_se18_boletin_vs_ops.csv", row.names = FALSE)

# 1c. tasa_seco_rt y vecinos_seco_rt
d <- subset(preparar_datos(), anio <= 2025)[, c("anio", "region", "poblacion")]
cr <- rbind(data.frame(anio = asis18$anio, region = asis18$region, crudo_seco = asis18$casos,
                       origen_seco = "consolidado, no en tiempo real (Excel ASIS, SE 1–18)")[asis18$anio <= 2020, ],
            data.frame(anio = rt$anio, region = rt$region, crudo_seco = rt$crudo_rt,
                       origen_seco = paste("tiempo real:", rt$fuente_rt)))
d <- merge(d, cr, by = c("anio", "region"))
stopifnot(nrow(d) == 99, !anyNA(d$crudo_seco), all(d$crudo_seco >= 0))
d$tasa_seco_rt <- log1p(1e5 * d$crudo_seco / d$poblacion)
vec <- as.matrix(read.csv("datos/vecindad_regiones.csv", row.names = 1, check.names = FALSE, fileEncoding = "UTF-8"))
d$vecinos_seco_rt <- NA
for (i in seq_len(nrow(d))) {
  v <- colnames(vec)[vec[d$region[i], ] == 1]; x <- d[d$anio == d$anio[i] & d$region %in% v, ]
  d$vecinos_seco_rt[i] <- log1p(1e5 * sum(x$crudo_seco) / sum(x$poblacion))
}
# Igualdad con el bloque seco crudo de bloques_region_anio.csv (mismo corte)
b <- read.csv("datos/procesados/bloques_region_anio.csv", fileEncoding = "UTF-8-BOM")
bs <- subset(b, bloque == "seco")[, c("anio", "region", "casos_crudos")]
chk <- merge(d, bs, by = c("anio", "region"))
cat("Filas donde el crudo SE 1–18 usado aquí difiere del bloque seco crudo:", sum(chk$crudo_seco != chk$casos_crudos), "\n")
d <- d[order(d$anio, d$region), ]
write.csv(d, "datos/procesados/v2_seco_tiempo_real.csv", row.names = FALSE, fileEncoding = "UTF-8")
