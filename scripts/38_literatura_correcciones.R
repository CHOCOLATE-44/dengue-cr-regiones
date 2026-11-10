# Corrección 7: metadatos de Crossref de las citas corregidas (se guardan para la auditoría).
#   CHIRPS v3: confirmar volumen y número de artículo (Crossref frente a la página de USGS).
#   ERA5-Land mensual: DOI doi:10.24381/cds.68d2bb30 (registro de DataCite, se consulta su título).
library(httr2)

cr <- request("https://api.crossref.org/works/10.1038/s41597-026-07096-4") |> req_perform() |> resp_body_json()
m <- cr$message
nulo <- function(x) if (is.null(x)) NA else x
chirps <- data.frame(doi = "10.1038/s41597-026-07096-4", titulo = m$title[[1]], revista = m$`container-title`[[1]],
                     volumen = nulo(m$volume), numero_articulo = nulo(m[["article-number"]]),
                     fecha = paste(unlist(m$published$`date-parts`), collapse = "-"),
                     autores = paste(sapply(m$author, function(a) paste(a$family, substr(a$given, 1, 1))), collapse = ", "),
                     fuente = "api.crossref.org")
dc <- request("https://api.datacite.org/dois/10.24381/cds.68d2bb30") |> req_perform() |> resp_body_json()
era <- data.frame(doi = "10.24381/cds.68d2bb30", titulo = dc$data$attributes$titles[[1]]$title,
                  revista = dc$data$attributes$publisher, volumen = NA, numero_articulo = NA,
                  fecha = nulo(dc$data$attributes$publicationYear),
                  autores = paste(sapply(dc$data$attributes$creators, `[[`, "name"), collapse = "; "), fuente = "api.datacite.org")
out <- rbind(chirps, era)
print(out)
write.csv(out, "modelo/resultados/v2_citas_metadatos.csv", row.names = FALSE, fileEncoding = "UTF-8")
