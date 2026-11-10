# Sección 16: verificación en la web de Finch et al. 2025 (Crossref, OpenAlex y resumen de Europe PMC).
# Salida: semanal/resultados/s50_referencias_metadatos.csv.
library(httr2)
library(jsonlite)
dir_a <- "datos/fuentes/literatura/articulo"
pedir <- function(url) {
  Sys.sleep(1)
  request(url) |> req_user_agent("verificacion-referencias (R httr2)") |> req_retry(max_tries = 3) |>
    req_error(is_error = function(r) FALSE) |> req_timeout(60) |> req_perform()
}
uno <- function(x) if (is.null(x) || length(x) == 0) NA else paste(unlist(x)[1], collapse = "")
limpiar <- function(x) if (is.na(x)) NA else trimws(gsub("\\s+", " ", gsub("<[^>]+>", " ", x)))
k <- "finch2025"; doi <- "10.1038/s41467-025-66411-6"

r <- pedir(paste0("https://api.crossref.org/works/", URLencode(doi, TRUE)))
stopifnot(resp_status(r) == 200)
writeLines(resp_body_string(r), file.path(dir_a, paste0(k, "_crossref.json")))
m <- fromJSON(resp_body_string(r))$message
ini <- function(g) if (is.na(g)) "" else toupper(paste(substr(strsplit(gsub("[.-]", " ", g), " +")[[1]], 1, 1), collapse = ""))
autores <- paste(trimws(paste(m$author$family, vapply(m$author$given, ini, ""))), collapse = ", ")
fecha <- NA; for (f in c("published-print", "published-online", "issued")) if (is.na(fecha) && !is.null(m[[f]])) fecha <- paste(unlist(m[[f]]$`date-parts`), collapse = "-")

e <- pedir(paste0("https://www.ebi.ac.uk/europepmc/webservices/rest/search?query=DOI:%22", URLencode(doi, TRUE), "%22&resultType=core&format=json"))
res <- fromJSON(resp_body_string(e))$resultList$result
resumen <- if (!is.null(m$abstract)) limpiar(m$abstract) else if (length(res) && !is.null(res$abstractText)) limpiar(res$abstractText[1]) else NA

o <- pedir(paste0("https://api.openalex.org/works/doi:", doi))
stopifnot(resp_status(o) == 200)
writeLines(resp_body_string(o), file.path(dir_a, paste0(k, "_openalex.json")))
w <- fromJSON(resp_body_string(o), simplifyVector = FALSE); b <- w$biblio

meta <- data.frame(clave = k, tipo = "doi", id = doi, http = resp_status(r), autores = autores, titulo = limpiar(uno(m$title)),
                   revista = uno(m$`container-title`), revista_abrev = uno(m$`short-container-title`), fecha = fecha,
                   volumen = uno(m$volume), numero = uno(m$issue),
                   paginas = if (!is.null(m$page)) m$page else uno(m$`article-number`), editorial = uno(m$publisher), isbn = NA,
                   resumen = resumen, verificada = TRUE, fuente_verificacion = "api.crossref.org",
                   oa_autores = paste(vapply(w$authorships, function(a) uno(a$author$display_name), ""), collapse = ", "),
                   oa_anio = uno(w$publication_year), oa_fuente = uno(w$primary_location$source$display_name),
                   oa_volumen = uno(b$volume), oa_numero = uno(b$issue),
                   oa_paginas = if (is.null(b$first_page)) NA else paste(c(b$first_page, b$last_page), collapse = "-"))
# Lo que pide el preregistro: Nat Commun 16:11364
meta$coincide_solicitud <- grepl("Nature Communications", meta$revista) && meta$volumen == "16" && meta$paginas == "11364"
write.csv(meta, "semanal/resultados/s50_referencias_metadatos.csv", row.names = FALSE, fileEncoding = "UTF-8")
print(t(meta[, c("autores", "titulo", "revista", "fecha", "volumen", "numero", "paginas", "oa_anio", "coincide_solicitud")]))
cat(strwrap(meta$resumen, 110), sep = "\n")
