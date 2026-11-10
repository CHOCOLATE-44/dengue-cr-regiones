# Correcciones 6a y 6c: inmunidad3 con una sola fuente de población y versión homogénea.
#   6c. Población de 2014 igual a la que usa arrastre (la de 2015, ver 06c_construir_predictores.py). Para tener
#       una sola fuente de población (la del modelo, bloques_region_anio), 2012 y 2013 también usan la de 2015.
#       -> inmunidad3_v2 (reemplaza a inmunidad3 en las comparaciones v2).
#   6a. Versión homogénea: los totales regionales 2019–2024 (boletines, preliminares) se escalan al total anual
#       nacional de la OPS (proporción regional × total OPS), como el resto del modelo; 2012–2018 quedan del ASIS.
#       -> inmunidad3_v2h (sensibilidad de A_H1 y P_H1).
# Requiere scripts/12_cv_loyo.R cargado.
library(readxl)

ca <- read.csv("datos/procesados/casos_anuales_region_2012-2024.csv", fileEncoding = "UTF-8")
pob_mod <- unique(subset(preparar_datos(), anio <= 2025)[, c("anio", "region", "poblacion")])
p15 <- subset(pob_mod, anio == 2015)
pob_v2 <- rbind(do.call(rbind, lapply(2012:2014, function(a) transform(p15, anio = a))), subset(pob_mod, anio <= 2024))
ca <- merge(ca[, c("anio", "region", "casos", "fuente")], pob_v2, by = c("anio", "region"))

ops <- setNames(as.data.frame(read_excel("datos/fuentes/dengue_semanal_CR_2015-2026.xlsx", "Serie semanal"))[, 1:3],
                c("anio", "se", "ops"))
ops_anual <- tapply(ops$ops, ops$anio, sum)
ca$total_ops <- as.numeric(ops_anual[as.character(ca$anio)])
ca$casos_h <- ca$casos
i <- ca$anio >= 2019
ca$casos_h[i] <- ca$casos[i] / ave(ca$casos, ca$anio)[i] / 9 * ca$total_ops[i]     # proporción regional × total OPS
chk <- aggregate(cbind(casos, casos_h, total_ops) ~ anio, ca, function(x) sum(x))
chk$total_ops <- chk$total_ops / 9
print(chk)
write.csv(chk, "modelo/resultados/v2_inmunidad3_totales_vs_ops.csv", row.names = FALSE)

ca$tasa <- 1e5 * ca$casos / ca$poblacion; ca$tasa_h <- 1e5 * ca$casos_h / ca$poblacion
inm <- do.call(rbind, lapply(2015:2025, function(a) {
  x <- subset(ca, anio %in% (a - 3):(a - 1)); stopifnot(all(table(x$region) == 3))
  data.frame(anio = a, region = sort(unique(x$region)),
             inmunidad3_v2 = log1p(as.numeric(tapply(x$tasa, x$region, sum)[sort(unique(x$region))])),
             inmunidad3_v2h = log1p(as.numeric(tapply(x$tasa_h, x$region, sum)[sort(unique(x$region))])))
}))
viejo <- read.csv("datos/procesados/predictores_regionales.csv", fileEncoding = "UTF-8")[, c("anio", "region", "inmunidad3")]
inm <- merge(inm, viejo, by = c("anio", "region"))
cat("Correlaciones: inmunidad3 vs v2", cor(inm$inmunidad3, inm$inmunidad3_v2), "| v2 vs v2h", cor(inm$inmunidad3_v2, inm$inmunidad3_v2h), "\n")
stopifnot(nrow(inm) == 99, !anyNA(inm))
write.csv(inm, "datos/procesados/v2_inmunidad3.csv", row.names = FALSE, fileEncoding = "UTF-8")
