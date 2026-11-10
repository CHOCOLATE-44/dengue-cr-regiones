# Sección 13 (PASO 1): fuentes para la selección de variables.
#   Roback P, Legler J. Beyond Multiple Linear Regression (CRC, 2021), versión web oficial: capítulos 1, 4, 8, 9, 10 y 11.
#   James G, Witten D, Hastie T, Tibshirani R. ISLR, 2.ª ed.: PDF oficial enlazado desde statlearning.com (caps. 5 y 6).
# Se guardan el HTML/PDF y el texto plano en datos/fuentes/literatura/seleccion/. No se usan para calcular nada.
library(httr2)
library(xml2)

dir_s <- "datos/fuentes/literatura/seleccion"
dir.create(dir_s, showWarnings = FALSE, recursive = TRUE)
base <- "https://bookdown.org/roback/bookdown-BeyondMLR/"
caps <- c("1" = "ch-MLRreview.html", "4" = "ch-poissonreg.html", "8" = "ch-multilevelintro.html",
          "9" = "ch-lon.html", "10" = "ch-3level.html", "11" = "ch-GLMM.html")
for (k in names(caps)) {
  html <- file.path(dir_s, paste0("roback_cap", k, ".html"))
  if (!file.exists(html)) {
    Sys.sleep(2)
    request(paste0(base, caps[[k]])) |> req_retry(max_tries = 3) |> req_perform(path = html)
  }
  doc <- read_html(html)
  nodos <- xml_find_all(doc, "//div[contains(@class,'section') or contains(@class,'level')]//h1 | //h2 | //h3 | //h4 | //p | //li | //pre")
  txt <- vapply(nodos, function(n) {
    t <- trimws(gsub("\\s+", " ", xml_text(n)))
    if (xml_name(n) %in% c("h1", "h2", "h3", "h4")) paste0("\n### ", t) else t
  }, "")
  writeLines(txt[nzchar(txt)], file.path(dir_s, paste0("roback_cap", k, ".txt")))
  cat("Roback cap.", k, ":", length(txt), "bloques\n")
}

# ISLR 2.ª ed.: buscar el enlace al PDF en statlearning.com
islr_pdf <- file.path(dir_s, "ISLRv2.pdf")
if (!file.exists(islr_pdf)) {
  h <- request("https://www.statlearning.com/") |> req_perform() |> resp_body_string()
  enl <- unique(regmatches(h, gregexpr("https?://[^\"<> ]+\\.pdf", h))[[1]])   # el enlace termina en .pdf.download.html
  print(enl)
  url <- grep("ISLR2", enl, value = TRUE)[1]
  stopifnot(!is.na(url))
  # la página .download.html redirige (meta refresh) al archivo oficial en Google Drive
  pag <- request(paste0(url, ".download.html")) |> req_perform() |> resp_body_string()
  destino <- sub(".*url='([^']+)'.*", "\\1", gsub("\n", " ", pag))
  tmp <- tempfile(fileext = ".pdf")
  r <- request(destino) |> req_timeout(600) |> req_perform(path = tmp)
  stopifnot(identical(readBin(tmp, "raw", 4), charToRaw("%PDF")))
  file.copy(tmp, islr_pdf)
  cat("ISLR descargado de", url, "->", destino, resp_header(r, "content-type"), file.size(islr_pdf), "bytes\n")
}
system2("pdftotext", c("-layout", shQuote(islr_pdf), shQuote(file.path(dir_s, "ISLRv2.txt"))))
