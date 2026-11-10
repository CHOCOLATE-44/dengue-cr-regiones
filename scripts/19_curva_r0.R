# PARTE B2: curva R0(T) relativa de Ae. aegypti con DENV (Mordecai et al. 2017, PLoS NTD 11(4): e0005568).
# Parámetros: mediana posterior de T0, Tm y c de cada rasgo, calculada de las muestras MCMC que los autores
# publicaron en Figshare (DOI 10.6084/m9.figshare.4563928; modelo con priors informativos = modelo principal).
# El suplemento S2 (Tabla B) solo publica medias e IC 95 %; se usan para verificar que las muestras son las mismas.

library(ggplot2)

dir_m <- "datos/fuentes/mordecai2017"
dir_r <- file.path(dir_m, "rsave")
dir.create(dir_r, showWarnings = FALSE)
necesarios <- c("Informative_Aegypti_DENV_ParameterFits_2016-03-30.Rsave",
                "LifespanFits_2016-03-30-informative.Rsave",
                "Aegypti_DENV_model_outputs-informative.Rsave")
for (f in necesarios) if (!file.exists(file.path(dir_r, f)))
  unzip(file.path(dir_m, "Rsave_files.zip"), files = paste0("Rsave files/", f), exdir = dir_r, junkpaths = TRUE)

e <- new.env(); load(file.path(dir_r, necesarios[1]), e)
l <- new.env(); n_l <- load(file.path(dir_r, necesarios[2]), l)
cat("Objetos de LifespanFits:", n_l, "\n")
lf_s <- get(n_l[grepl("samps", n_l)][1], l)

muestras <- list(a = e$a.samps, EFD = e$EFD.samps, pEA = e$e2a.samps, MDR = e$MDR.samps,
                 lf = lf_s, b = e$b.samps, c = e$c.samps, PDR = e$PDR.samps)
forma <- c(a = "briere", EFD = "briere", pEA = "quad", MDR = "briere", lf = "quad",
           b = "briere", c = "briere", PDR = "briere")
col_c <- function(s) if ("c" %in% names(s)) "c" else if ("qd" %in% names(s)) "qd" else stop("sin constante")

# Tabla B del suplemento S2 (medias publicadas), para comparar
tabla_b <- data.frame(rasgo = c("a", "EFD", "pEA", "MDR", "lf", "b", "c", "PDR"),
                      T0_media_S2 = c(13.35, 14.58, 13.56, 11.36, 9.16, 17.05, 12.22, 10.68),
                      Tm_media_S2 = c(40.08, 34.61, 38.29, 39.17, 37.73, 35.83, 37.46, 45.90),
                      c_media_S2  = c(2.02e-4, 8.56e-3, -5.99e-3, 7.86e-5, -1.48e-1, 8.49e-4, 4.91e-4, 6.65e-5))
par <- do.call(rbind, lapply(names(muestras), function(k) {
  s <- muestras[[k]]; cc <- col_c(s)
  data.frame(rasgo = k, funcion = forma[[k]], n_muestras = nrow(s),
             T0_mediana = median(s$T0), Tm_mediana = median(s$Tm), c_mediana = median(s[[cc]]),
             T0_media = mean(s$T0), Tm_media = mean(s$Tm), c_media = mean(s[[cc]]))
}))
par <- merge(par, tabla_b, by = "rasgo", sort = FALSE)
print(par, digits = 4)
write.csv(par, "modelo/resultados/mordecai_parametros_aegypti.csv", row.names = FALSE)

source("scripts/funciones_r0.R")   # rasgo(), r0_bruto(), r0_relativo()
grid_T <- seq(5, 45, by = 0.01)
R0_max <- max(r0_bruto(grid_T))
saveRDS(list(par = par, R0_max = R0_max), "modelo/resultados/mordecai_curva_r0.rds")

# Validación: pico entre 26 y 29 °C; cero fuera de 18-34 °C (resumen del artículo) y contra lo
# reportado para Ae. aegypti (pico 29,1; cero bajo 17,8 y sobre 34,6 °C) y la curva posterior publicada.
r <- r0_relativo(grid_T)
pos <- grid_T[r > 0]
val <- data.frame(t_pico = grid_T[which.max(r)], t_min_r0_pos = min(pos), t_max_r0_pos = max(pos),
                  pico_en_26_29 = grid_T[which.max(r)] >= 26 & grid_T[which.max(r)] <= 29,
                  cero_fuera_18_34 = all(r[grid_T < 18 | grid_T > 34] == 0),
                  max_r0_bajo_18 = max(r[grid_T < 18]), max_r0_sobre_34 = max(r[grid_T > 34]),
                  articulo_pico = 29.1, articulo_min = 17.8, articulo_max = 34.6)
o <- new.env(); load(file.path(dir_r, necesarios[3]), o)
post_med <- apply(o$R0, 1, median); post_med <- post_med / max(post_med)
post_med_T <- o$temp
val$pico_curva_posterior_publicada <- post_med_T[which.max(post_med)]
val$min_curva_posterior_publicada  <- min(post_med_T[post_med > 0])
val$max_curva_posterior_publicada  <- max(post_med_T[post_med > 0])
print(val)
write.csv(val, "modelo/resultados/validacion_curva_r0.csv", row.names = FALSE)

g <- ggplot() +
  annotate("rect", xmin = 26, xmax = 29, ymin = -Inf, ymax = Inf, alpha = 0.15) +
  geom_vline(xintercept = c(18, 34), linetype = "dashed") +
  geom_line(aes(post_med_T, post_med, colour = "Mediana posterior publicada (Figshare)"), linewidth = 0.8) +
  geom_line(aes(grid_T, r, colour = "Rasgos con mediana posterior (este análisis)"), linewidth = 0.8) +
  scale_colour_manual(values = c("grey50", "black"), name = NULL) +
  coord_cartesian(xlim = c(10, 40)) +
  labs(x = "Temperatura media (°C)", y = "R0 relativo (0-1)",
       caption = "Banda gris: 26-29 °C. Líneas punteadas: 18 y 34 °C. Ae. aegypti, DENV; Mordecai et al. 2017.") +
  theme_bw() + theme(legend.position = "bottom")
ggsave("figuras/fig_validacion_r0.png", g, width = 7, height = 4.5, dpi = 150)
