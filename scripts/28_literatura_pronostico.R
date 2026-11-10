# Literatura del pronóstico desde la SE 18: texto completo de PMC (NCBI E-utilities, efetch db=pmc).
#   Cuong et al. 2013, Emerg Infect Dis 19(6):945–953 (PMC3713821)
#   Lauer et al. 2018, PNAS (PMC5877997): incidencia previa a la temporada para pronosticar el año
library(httr2)
dir_l <- "datos/fuentes/literatura"
dir.create(dir_l, showWarnings = FALSE, recursive = TRUE)
for (id in c("PMC3713821", "PMC5877997")) {
  destino <- file.path(dir_l, paste0(id, "_efetch.xml"))
  if (file.exists(destino) && file.size(destino) > 0) next
  request("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi") |>
    req_url_query(db = "pmc", id = sub("PMC", "", id), rettype = "xml") |>
    req_retry(max_tries = 3) |> req_perform(path = destino)
  cat(id, file.size(destino), "bytes\n")
  Sys.sleep(1)
}
