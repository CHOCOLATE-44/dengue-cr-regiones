# Corrección 4: diagnóstico de A_H3 (M_clima + sequia_pob:caribe + oni:caribe). A_H3 NO se adopta; M_clima sigue
# siendo el modelo explicativo. Se reporta, en el ajuste completo 2015–2025 (99 filas, SE 1–40, predictores
# estandarizados con los 99 datos, como en 31_hipotesis_regionales.R):
#   - VIF de los términos de A_H3, incluidas las dos interacciones (lm de cada columna contra las demás);
#   - prueba de razón de verosimilitudes conjunta de las dos interacciones (A_H3 frente a M_clima, gl = 2);
#   - efecto neto en H. Caribe (principal + interacción) con IC 95 % de Wald a partir de vcov;
#   - correlación sequia_pob–oni (RONI previo) en las 99 filas y en H. Caribe.
# Requiere 12_cv_loyo.R (ajustar) y 25_parte_c_predictores.R (datos_bio).
library(glmmTMB)

reg <- read.csv("datos/procesados/predictores_regionales.csv", fileEncoding = "UTF-8")
d <- merge(datos_bio(), reg[, c("anio", "region", "caribe")], by = c("anio", "region"))
d$region <- factor(d$region); d$anio_f <- factor(d$anio); d$casos <- d$casos_escalados
stopifnot(nrow(d) == 99)
clima <- c("arrastre", "oni", "aptitud_termica_pob", "sequia_pob")
for (v in clima) d[[paste0(v, "_z")]] <- (d[[v]] - mean(d[[v]])) / sd(d[[v]])

base <- "region + (1 | anio_f) + offset(log(poblacion))"
f_clima <- as.formula(paste("casos ~ arrastre_z + oni_z + aptitud_termica_pob_z + sequia_pob_z +", base))
f_h3 <- update(f_clima, . ~ . + sequia_pob_z:caribe + oni_z:caribe)
m_clima <- ajustar(f_clima, d); m_h3 <- ajustar(f_h3, d)

# Prueba de razón de verosimilitudes (ML; glmmTMB ajusta por máxima verosimilitud)
lr <- as.numeric(2 * (logLik(m_h3) - logLik(m_clima)))
gl <- attr(logLik(m_h3), "df") - attr(logLik(m_clima), "df")
lrt <- data.frame(modelo = "A_H3 vs M_clima", logLik_M_clima = as.numeric(logLik(m_clima)), logLik_A_H3 = as.numeric(logLik(m_h3)),
                  estadistico = lr, gl = gl, p = pchisq(lr, gl, lower.tail = FALSE),
                  AIC_M_clima = AIC(m_clima), AIC_A_H3 = AIC(m_h3),
                  avisos = paste(attr(m_clima, "avisos"), attr(m_h3, "avisos"), sep = " / "))

# VIF con las interacciones como columnas
X <- data.frame(arrastre = d$arrastre_z, oni = d$oni_z, aptitud_termica_pob = d$aptitud_termica_pob_z,
                sequia_pob = d$sequia_pob_z, "sequia_pob:caribe" = d$sequia_pob_z * d$caribe,
                "oni:caribe" = d$oni_z * d$caribe, check.names = FALSE)
vifs <- data.frame(modelo = "A_H3", variable = names(X), VIF = sapply(names(X), function(v)
  1 / (1 - summary(lm(X[[v]] ~ ., data = X[setdiff(names(X), v)]))$r.squared)), row.names = NULL)
vifs$VIF_mayor_5 <- vifs$VIF > 5

# Efecto neto en H. Caribe (escala estandarizada, log de la razón de tasas por 1 DE)
b <- fixef(m_h3)$cond; V <- vcov(m_h3)$cond
neto <- do.call(rbind, lapply(c("sequia_pob_z", "oni_z"), function(p) {
  int <- grep(paste0("^", p, ":caribe$|^caribe:", p, "$"), names(b), value = TRUE); stopifnot(length(int) == 1)
  rbind(data.frame(termino = p, efecto = "resto de regiones (principal)", estimado = b[p], ee = sqrt(V[p, p])),
        data.frame(termino = p, efecto = "H. Caribe (principal + interacción)", estimado = b[p] + b[int],
                   ee = sqrt(V[p, p] + V[int, int] + 2 * V[p, int])))
}))
neto$ic_inf <- neto$estimado - qnorm(0.975) * neto$ee; neto$ic_sup <- neto$estimado + qnorm(0.975) * neto$ee
neto$p <- 2 * pnorm(-abs(neto$estimado / neto$ee)); neto$razon_tasas <- exp(neto$estimado)
rownames(neto) <- NULL

car <- d$caribe == 1
cors <- data.frame(par = "sequia_pob ~ oni (RONI previo)", r_99_filas = cor(d$sequia_pob, d$oni),
                   r_h_caribe = cor(d$sequia_pob[car], d$oni[car]), r_resto = cor(d$sequia_pob[!car], d$oni[!car]))

options(width = 200)
print(lrt, digits = 4); print(vifs, digits = 3); print(neto, digits = 3); print(cors, digits = 3)
print(summary(m_h3)$coefficients$cond, digits = 3)
write.csv(lrt, "modelo/resultados/v2_a_h3_lrt.csv", row.names = FALSE)
write.csv(vifs, "modelo/resultados/v2_a_h3_vif.csv", row.names = FALSE)
write.csv(neto, "modelo/resultados/v2_a_h3_efecto_neto_caribe.csv", row.names = FALSE)
write.csv(cors, "modelo/resultados/v2_a_h3_correlaciones.csv", row.names = FALSE)
