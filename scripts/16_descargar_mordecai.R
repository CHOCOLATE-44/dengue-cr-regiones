# PARTE B2: descarga de Mordecai et al. (2017), PLoS NTD 11(4): e0005568, y su material suplementario.
# Salta lo ya descargado. Lo llama modelo_dengue.R.

library(httr2)

dir_m <- "datos/fuentes/mordecai2017"
dir.create(dir_m, showWarnings = FALSE, recursive = TRUE)
doi <- "10.1371/journal.pntd.0005568"
archivos <- c(
  "articulo.pdf" = paste0("https://journals.plos.org/plosntds/article/file?id=", doi, "&type=printable"),
  "articulo.xml" = paste0("https://journals.plos.org/plosntds/article/file?id=", doi, "&type=manuscript"),
  "S1.pdf"       = paste0("https://journals.plos.org/plosntds/article/file?type=supplementary&id=", doi, ".s001"),
  "S2.pdf"       = paste0("https://journals.plos.org/plosntds/article/file?type=supplementary&id=", doi, ".s002"))
for (f in names(archivos)) {
  destino <- file.path(dir_m, f)
  if (file.exists(destino) && file.size(destino) > 0) next
  Sys.sleep(2)
  r <- request(archivos[[f]]) |> req_retry(max_tries = 5) |> req_perform(path = destino)
  cat(f, "->", resp_header(r, "content-type"), resp_header(r, "content-disposition"), "\n")
}
print(file.info(list.files(dir_m, full.names = TRUE))[, "size", drop = FALSE])

# Muestras posteriores del artículo (Figshare, DOI 10.6084/m9.figshare.4563928): permiten la mediana
# posterior de T0, Tm y c, que el suplemento S2 no publica (solo media e IC 95 %).
f_zip <- file.path(dir_m, "Rsave_files.zip")
if (!file.exists(f_zip) || file.size(f_zip) < 901152798) {
  request("https://ndownloader.figshare.com/files/7394476") |> req_retry(max_tries = 5) |>
    req_timeout(7200) |> req_perform(path = f_zip)
}
cat("Rsave_files.zip:", file.size(f_zip), "bytes\n")
print(head(unzip(f_zip, list = TRUE), 60))
