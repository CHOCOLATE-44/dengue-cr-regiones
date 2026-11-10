# Curva R0(T) relativa de Ae. aegypti (Mordecai et al. 2017), mediana posterior de cada rasgo.
# Requiere en el entorno `par` (tabla de parámetros de 19_curva_r0.R); `r0_relativo` requiere además `R0_max`.
# Funciones de respuesta térmica (Mordecai 2017, Métodos): Brière c·T·(T−T0)·(Tm−T)^(1/2);
# cuadrática −c(T−T0)(T−Tm) con c > 0 (en las muestras la constante "qd" ya es negativa: qd(T−T0)(T−Tm)).
# Valores negativos se truncan en 0 y las probabilidades (pEA, b, c) en 1.
rasgo <- function(k, T) {
  p <- par[par$rasgo == k, ]
  y <- if (p$funcion == "briere") p$c_mediana * T * (T - p$T0_mediana) * sqrt(pmax(p$Tm_mediana - T, 0))
       else p$c_mediana * (T - p$T0_mediana) * (T - p$Tm_mediana)
  y[T <= p$T0_mediana | T >= p$Tm_mediana] <- 0
  y <- pmax(y, 0)
  if (k %in% c("pEA", "b", "c")) y <- pmin(y, 1)
  y
}
# Ec. (1): R0(T) = ( a² b c e^(−μ/PDR) EFD pEA MDR / (N r μ³) )^(1/2), μ = 1/lf. N y r no dependen de T.
r0_bruto <- function(T) {
  lf <- rasgo("lf", T); mu <- ifelse(lf > 0, 1 / lf, Inf)
  pdr <- rasgo("PDR", T)
  v <- rasgo("a", T)^2 * rasgo("b", T) * rasgo("c", T) * ifelse(pdr > 0, exp(-mu / pdr), 0) *
       rasgo("EFD", T) * rasgo("pEA", T) * rasgo("MDR", T) / mu^3
  v[!is.finite(v)] <- 0
  sqrt(v)
}
r0_relativo <- function(T) r0_bruto(T) / R0_max     # escala 0-1; 1 en el pico
