# B4 (reemergencia 2015): cuadros anuales de la OPS "Number of reported cases of dengue and severe dengue
# in the Americas, by country", con columna de serotipos circulantes. Las URL originales de paho.org/hq ya no
# responden; se bajan las copias del Internet Archive (Wayback Machine). Se imprime la fila de Costa Rica.

library(httr2)

dir_o <- "datos/fuentes/ops_cuadros_dengue"
dir.create(dir_o, showWarnings = FALSE, recursive = TRUE)
cuadros <- data.frame(
  anio_datos = c(2009, 2010, 2011, 2012, 2013),
  archivo = c("dengue_cases_2009.pdf", "dengue_cases_2010_december_10_2.pdf", "dengue_cases_2011_January_21_EW_3.pdf",
              "dengue-cases-2012-27august-EW-31.pdf", "dengue-cases-2013-May-20-ew-52-update.pdf"),
  wayback = c("https://web.archive.org/web/20220305110202id_/https://www.paho.org/hq/dmdocuments/2009/dengue_cases_2009.pdf",
              "https://web.archive.org/web/20201019231510id_/https://www.paho.org/hq/dmdocuments/2010/dengue_cases_2010_december_10_2%20.pdf",
              "https://web.archive.org/web/20200707122427id_/https://www.paho.org/hq/dmdocuments/2011/dengue_cases_2011_January_21_EW_3.pdf",
              "https://web.archive.org/web/20250724124306id_/https://www3.paho.org/hq/dmdocuments/2012/dengue-cases-2012-27august-EW-31.pdf",
              "https://web.archive.org/web/20180723005455id_/https://www.paho.org/hq/dmdocuments/2013/dengue-cases-2013-May-20-ew-52-update.pdf"))
for (i in seq_len(nrow(cuadros))) {
  destino <- file.path(dir_o, cuadros$archivo[i])
  if (file.exists(destino) && file.size(destino) > 0) next
  Sys.sleep(3)
  r <- request(cuadros$wayback[i]) |> req_retry(max_tries = 4, backoff = function(k) 20 * k) |>
    req_error(is_error = function(r) FALSE) |> req_timeout(300) |> req_perform(path = destino)
  cat(cuadros$archivo[i], resp_status(r), resp_content_type(r), "\n")
  if (resp_status(r) != 200 || !grepl("pdf", resp_content_type(r))) { file.remove(destino); cat("  no disponible\n") }
}
write.csv(cuadros, file.path(dir_o, "cuadros_ops.csv"), row.names = FALSE)

for (i in seq_len(nrow(cuadros))) {
  f <- file.path(dir_o, cuadros$archivo[i])
  if (!file.exists(f)) next
  txt <- system2("pdftotext", c("-layout", shQuote(f), "-"), stdout = TRUE)
  cat("\n=====", cuadros$archivo[i], "=====\n")
  cat(grep("Serotyp|Serotip|Costa Rica|Epidemiological Week|Week|Semana|Updated|Actualiz", txt, value = TRUE, ignore.case = TRUE), sep = "\n")
}

# -----------------------------------------------------------------------------
# Agregar a datos/serotipos_anual.csv la presencia que reportan los cuadros (solo presencia = 1;
# que un serotipo no aparezca no se toma como ausencia). Idempotente: no duplica filas de la OPS.
# -----------------------------------------------------------------------------
fila_cr <- function(f) {
  pags <- strsplit(paste(system2("pdftotext", c("-layout", shQuote(f), "-"), stdout = TRUE), collapse = "\n"), "\f")[[1]]
  p <- grep("Costa Rica", pags)[1]
  lin <- grep("Costa Rica", strsplit(pags[p], "\n")[[1]], value = TRUE)[1]
  enc <- strsplit(pags[1], "\n")[[1]]
  list(pagina = p, linea = trimws(gsub("\\s+", " ", lin)),
       figuras = trimws(gsub("\\s+", " ", sub("^\\s*(Figures for [0-9]{4} \\([^)]*\\)).*", "\\1", grep("Figures for", enc, value = TRUE)[1]))),
       semana  = trimws(gsub("\\s+", " ", sub(".*(Epidemiological Week / EW [0-9]+ \\(Updated [^)]*\\)).*", "\\1", grep("Updated", enc, value = TRUE)[1]))))
}
ser <- read.csv("datos/serotipos_anual.csv", fileEncoding = "UTF-8", colClasses = "character")
nuevas <- list()
usar <- data.frame(archivo = cuadros$archivo, anio = c(2009, 2010, 2011, 2012, 2012))
usar <- usar[usar$archivo != "dengue-cases-2012-27august-EW-31.pdf", ]   # 2012 se toma del cierre (EW 52)
for (i in seq_len(nrow(usar))) {
  f <- file.path(dir_o, usar$archivo[i]); fr <- fila_cr(f)
  cod <- regmatches(fr$linea, regexpr("DEN ?[1-4](,[1-4])*", fr$linea))   # p. ej. "DEN 1,2,3"
  stopifnot(length(cod) == 1)
  sts <- paste0("DENV-", strsplit(sub("DEN ?", "", cod), ",")[[1]])
  url <- cuadros$wayback[cuadros$archivo == usar$archivo[i]]
  for (s in sts) nuevas[[length(nuevas) + 1]] <- data.frame(
    anio = as.character(usar$anio[i]), serotipo = s, presente = "1", porcentaje_reportado = NA, n_serotipo = NA,
    n_tipificados = NA, fuente_tipo = "ops",
    documento = paste0("OPS. Number of reported cases of dengue and severe dengue in the Americas, by country. ",
                       fr$figuras, "; ", fr$semana, ". Copia del Internet Archive"),
    pagina = as.character(fr$pagina), fecha_documento = NA, nivel_lectura = "completo", url = url,
    nota = paste0("Fila de Costa Rica: \"", fr$linea, "\". Columna Serotype."))
}
nuevas <- do.call(rbind, nuevas)
for (k in setdiff(names(ser), names(nuevas))) nuevas[[k]] <- NA   # columnas agregadas después (p. ej. importado)
nuevas$fecha_documento <- sapply(nuevas$documento, function(d) {
  m <- regmatches(d, regexpr("Updated [0-9]{1,2} [A-Za-z]+ [0-9]{4}", d))
  if (length(m)) format(as.Date(sub("Updated ", "", m), "%d %B %Y"), "%Y-%m-%d") else NA })
print(nuevas[, c("anio", "serotipo", "pagina", "fecha_documento", "nota")])
ser <- ser[ser$fuente_tipo != "ops" | !(ser$anio %in% nuevas$anio), ]                       # idempotente
quitar <- ser$fuente_tipo == "sin_fuente" & paste(ser$anio, ser$serotipo) %in% paste(nuevas$anio, nuevas$serotipo)
ser <- rbind(ser[!quitar, ], nuevas)
ser <- ser[order(as.integer(ser$anio), ser$serotipo, ser$fuente_tipo), ]
write.csv(ser, "datos/serotipos_anual.csv", row.names = FALSE, fileEncoding = "UTF-8", na = "NA")
cat("serotipos_anual.csv: filas", nrow(ser), "| filas sin_fuente reemplazadas:", sum(quitar), "\n")
