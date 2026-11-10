# Fuentes citadas para H1–H4. Se bajan para leerlas; ninguna cita se usa sin leerla.
#   Frontiers in Public Health 2025, doi:10.3389/fpubh.2025.1537019 (PDF del editor)
#   arXiv 2607.05740 (Sanchez 2026, PREPRINT sin revisión de pares)
#   García et al. 2023, PLOS Glob Public Health, doi:10.1371/journal.pgph.0002417 (PMC10586647, NCBI efetch)
#   Lowe et al. 2018, PLoS Med, doi:10.1371/journal.pmed.1002613 (PMC6049902, NCBI efetch)
#   IMN, "El clima y las regiones climáticas de Costa Rica" (PDF)
library(httr2)
dir_l <- "datos/fuentes/literatura"
dir.create(dir_l, showWarnings = FALSE, recursive = TRUE)
bajar <- function(url, destino) {
  if (file.exists(destino) && file.size(destino) > 0) return(invisible())
  r <- request(url) |> req_retry(max_tries = 3) |> req_timeout(180) |>
    req_error(is_error = function(r) FALSE) |> req_perform(path = destino)
  cat(basename(destino), resp_status(r), resp_content_type(r), file.size(destino), "bytes\n")
  if (resp_status(r) != 200) { file.remove(destino); cat("  no disponible\n") }
  Sys.sleep(1)
}
bajar("https://www.frontiersin.org/journals/public-health/articles/10.3389/fpubh.2025.1537019/pdf",
      file.path(dir_l, "fpubh_2025_1537019.pdf"))
bajar("https://arxiv.org/pdf/2607.05740", file.path(dir_l, "arxiv_2607.05740_Sanchez2026_PREPRINT.pdf"))
for (id in c("10586647", "6049902"))
  bajar(paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pmc&rettype=xml&id=", id),
        file.path(dir_l, paste0("PMC", id, "_efetch.xml")))
bajar("https://www.imn.ac.cr/documents/10179/31165/clima-regiones-climat.pdf/cb3b55c3-f358-495a-b66c-90e677e35f57",
      file.path(dir_l, "IMN_clima_regiones_climaticas.pdf"))
