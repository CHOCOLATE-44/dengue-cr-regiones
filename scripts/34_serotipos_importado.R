# Corrección 6b: columna "importado" (1/0) en datos/serotipos_anual.csv, con su fuente, para que
# 17_reemergencia.R excluya los casos importados por regla. Verifica el texto en las dos fuentes.
library(httr2)
dir_i <- "datos/fuentes/inciensa"; dir.create(dir_i, showWarnings = FALSE)
f_inc <- file.path(dir_i, "2022_anual_informe_epidemiologico_arbovirus.pdf")
if (!file.exists(f_inc))
  request("https://www.inciensa.sa.cr/wp-content/uploads/simple-file-list/Informes-Epidemiologicos/Arbovirus/2022_anual_informe_epidemiologico_arbovirus.pdf") |>
    req_retry(max_tries = 3) |> req_timeout(180) |> req_perform(path = f_inc)
lineas <- function(f, p, patron) {
  t <- system2("pdftotext", c("-layout", "-f", p, "-l", p, shQuote(f), "-"), stdout = TRUE)
  trimws(t[grepl(patron, t, ignore.case = TRUE)])
}
cat("INCIENSA 2022, p. 11:\n"); print(lineas(f_inc, 11, "import|México|Quintana|DENV-3|serotipo 3"))
cat("Boletín N°33 de 2022, p. 4:\n"); print(lineas("datos/fuentes/pdfs/2022_semanal_33.pdf", 4, "Quintana|México|serotipo"))

ser <- read.csv("datos/serotipos_anual.csv", fileEncoding = "UTF-8", colClasses = "character")
ser$importado <- "0"; ser$fuente_importado <- NA
i <- ser$anio == "2022" & ser$serotipo == "DENV-3" & ser$presente == "1"
stopifnot(sum(i) == 1)
ser$importado[i] <- "1"
ser$fuente_importado[i] <- paste("INCIENSA-CNRV, informe anual de arbovirus 2022, p. 11; Ministerio de Salud,",
                                 "Boletín N°33 de 2022, p. 4: paciente que visitó el estado de Quintana Roo (México)")
write.csv(ser, "datos/serotipos_anual.csv", row.names = FALSE, fileEncoding = "UTF-8", na = "NA")
cat("Filas con importado = 1:", sum(ser$importado == "1"), "\n")
