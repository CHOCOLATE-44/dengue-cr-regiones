# H1 (inmunidad3): Excel ASIS de dengue 2012 y 2013 de la carpeta "Análisis de situación de salud" del
# Ministerio de Salud (el de 2014 ya lo bajó 06b_descargar_predictores.py). Salta lo ya descargado.
library(httr2)
A <- "https://www.ministeriodesalud.go.cr/index.php/biblioteca-de-archivos-left/documentos-ministerio-de-salud/vigilancia-de-la-salud/analisis-de-situacion-salud/"
archivos <- c("2012" = "1644-dengue-2012", "2013" = "1630-situacion-dengue-costa-rica-2013")
for (a in names(archivos)) {
  destino <- sprintf("datos/fuentes/asis/dengue_%s.xls", a)
  if (file.exists(destino) && file.size(destino) > 0) next
  r <- request(paste0(A, archivos[[a]], "/file")) |> req_retry(max_tries = 3) |> req_timeout(180) |>
    req_perform(path = destino)
  cat(a, resp_status(r), resp_content_type(r), file.size(destino), "bytes\n")
}
