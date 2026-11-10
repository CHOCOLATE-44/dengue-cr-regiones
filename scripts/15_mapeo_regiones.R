# PARTE A: mapeo cantón -> región rectora, polígonos de las 9 regiones, validación con
# población WorldPop 2020 y grilla de población de 0,1°.
# Se ejecuta desde la raíz del proyecto (lo llama modelo_dengue.R).

library(sf)
library(terra)
library(exactextractr)

dir_esp <- "datos/fuentes/espacial"
dir.create("modelo/resultados", showWarnings = FALSE)
sf_use_s2(FALSE)

# -----------------------------------------------------------------------------
# A1. Cantones GADM 4.1 (nivel 2)
# -----------------------------------------------------------------------------
can <- st_read(file.path(dir_esp, "gadm41_CRI.gpkg"), layer = "ADM_ADM_2", quiet = TRUE)
cat("Cantones en GADM 4.1:", nrow(can), "\n")
recientes <- c("Río Cuarto", "Monteverde", "Puerto Jiménez")
print(data.frame(canton = recientes, en_gadm = recientes %in% can$NAME_2))
dis <- st_read(file.path(dir_esp, "gadm41_CRI.gpkg"), layer = "ADM_ADM_3", quiet = TRUE)
padres <- unique(st_drop_geometry(dis)[grepl("Monte ?verde|Puerto Jim", dis$NAME_3, ignore.case = TRUE), c("NAME_3", "NAME_2", "NAME_1")])
names(padres) <- c("canton_nuevo", "canton_padre_gadm", "provincia")
cat("Cantones nuevos que GADM trae como distrito de su cantón padre:\n"); print(padres)
write.csv(padres, "modelo/resultados/gadm_cantones_nuevos.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# A2. Cantón -> región rectora, según los ARS listados en el ASIS de niveles locales 2023
# -----------------------------------------------------------------------------
# Cada cantón GADM se asigna a la región en cuya carpeta aparece su ARS. Cuando un cantón
# tiene varios ARS (San José, San Carlos, Puntarenas) todos están en la misma región.
# Los nombres de región son los de bloques_region_anio.csv.
ars <- read.csv(file.path(dir_esp, "ars_asis_2023.csv"), fileEncoding = "UTF-8")
asig <- list(
  "Brunca"           = list(c("Buenos Aires", "Corredores", "Coto Brus", "Golfito", "Osa", "Pérez Zeledón"),
                            "ARS Buenos Aires, Corredores, Coto Brus, Golfito, Osa, Pérez Zéledon"),
  "C.Este"           = list(c("Jiménez", "Alvarado", "Cartago", "Dota", "El Guarco", "La Unión",
                              "León Cortés Castro", "Tarrazú", "Oreamuno", "Paraíso", "Turrialba"),
                            "ARS Jiménez, Alvarado, Cartago, Dota, El Guarco, La Unión, León Cortez, Los Santos-Tarrazu, Oreamuno, Paraiso, Turrialba"),
  "Central Norte"    = list(c("Alajuela", "Atenas", "Barva", "Belén", "Flores", "Grecia", "Heredia", "Poás",
                              "Río Cuarto", "San Isidro", "San Pablo", "San Rafael", "Santa Bárbara",
                              "Santo Domingo", "Sarapiquí"),
                            "ARS Alajuela, Atenas, Barva, Belen, Flores, Grecia, Heredia, Poás, Rio Cuarto, San Isidro, San Pablo, San Rafael, Santa Barbara, Santo Domingo, Sarapiquí"),
  "Central Sur"      = list(c("San José", "Acosta", "Alajuelita", "Aserrí", "Vázquez de Coronado", "Curridabat",
                              "Desamparados", "Escazú", "Goicoechea", "Montes de Oca", "Mora", "Moravia",
                              "Puriscal", "Santa Ana", "Tibás", "Turrubares"),
                            "ARS El Carmen-Merced-La Uruca, Hatillo, Mata Redonda, Pavas y Sureste Metropolitana (cantón San José); Acosta, Alajuelita, Aserri, Coronado, Curridabat, Desamparados, Escazu, Goicochea, Montes de Oca, Mora, Moravia, Puriscal, Santa Ana, Tibas, Turrubares"),
  "Chorotega"        = list(c("Abangares", "Bagaces", "Cañas", "Carrillo", "Hojancha", "La Cruz", "Liberia",
                              "Nandayure", "Nicoya", "Santa Cruz", "Tilarán", "Upala"),
                            "ARS Abagares, Bagaces, Cañas, Carrillo, Hojancha, La Cruz, Liberia, Nandayure, Nicoya, Santa Cruz, Tilaran, Upala"),
  "H. Caribe"        = list(c("Guácimo", "Limón", "Matina", "Pococí", "Siquirres", "Talamanca"),
                            "ARS Guacimo, Limón, Matina, Pococí, Siquirres, Talamanca"),
  "H.Norte"          = list(c("San Carlos", "Guatuso", "Los Chiles"),
                            "ARS Aguas Zarcas, Ciudad Quesada, Florencia y Santa Rosa (cantón San Carlos); Guatuso; Los Chiles"),
  "Occidente"        = list(c("Naranjo", "Palmares", "San Ramón", "Sarchí", "Zarcero"),
                            "ARS Naranjo, Palmares, San Ramón, Sarchí, Zarcero"),
  "Pacífico Central" = list(c("Esparza", "Garabito", "Montes de Oro", "Orotina", "Parrita", "Puntarenas",
                              "Quepos", "San Mateo"),
                            "ARS Barranca, Islas del Golfo, Peninsular y Puntarenas (cantón Puntarenas); Esparza, Garabito, Montes de Oro, Orotina, Parrita, Quepos, San Mateo"))
carpeta <- c("Brunca" = "Brunca", "C.Este" = "C.Este", "Central Norte" = "C.Norte", "Central Sur" = "C.Sur",
             "Chorotega" = "Chorotega", "H. Caribe" = "H.Caribe", "H.Norte" = "H.Norte",
             "Occidente" = "Occidente", "Pacífico Central" = "P.Central")
mapeo <- do.call(rbind, lapply(names(asig), function(r)
  data.frame(canton = asig[[r]][[1]], region = r,
             fuente = paste0("Ministerio de Salud, ASIS de niveles locales 2023, carpeta ", carpeta[[r]], ": ", asig[[r]][[2]]))))
mapeo <- merge(mapeo, st_drop_geometry(can)[, c("NAME_2", "NAME_1", "GID_2")], by.x = "canton", by.y = "NAME_2", all = TRUE)
names(mapeo)[names(mapeo) == "NAME_1"] <- "provincia"
mapeo$nota <- ""
mapeo$nota[mapeo$canton == "Puntarenas"] <- "Incluye el distrito Monteverde (cantón desde 2021; GADM lo trae dentro de Puntarenas). El listado ASIS 2023 no tiene un ARS Monteverde."
mapeo$nota[mapeo$canton == "Golfito"]    <- "Incluye el distrito Puerto Jiménez (cantón desde 2022; GADM lo trae dentro de Golfito). El listado ASIS 2023 no tiene un ARS Puerto Jiménez; Golfito y Osa son de Brunca."
mapeo$nota[mapeo$canton == "Tarrazú"]    <- "ARS Los Santos-Tarrazú."
mapeo$nota[mapeo$canton == "Upala"]      <- "Provincia Alajuela, pero su ARS está en la carpeta de Chorotega."
mapeo$nota[mapeo$canton %in% c("Orotina", "San Mateo")] <- "Provincia Alajuela, pero su ARS está en la carpeta de Pacífico Central."
mapeo$nota[mapeo$canton %in% c("Dota", "León Cortés Castro")] <- "Provincia San José, pero su ARS está en la carpeta de C. Este."
stopifnot(!anyNA(mapeo$region), !anyNA(mapeo$GID_2), nrow(mapeo) == 82, !anyDuplicated(mapeo$canton))
write.csv(mapeo[order(mapeo$region, mapeo$canton), ], "datos/mapeo_canton_region.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("Cantones por región:\n"); print(table(mapeo$region))

# ARS del listado que no son un cantón completo (se resuelven por el cantón que los contiene)
cat("Documentos del ASIS 2023 que no corresponden a un solo cantón completo:\n")
print(ars[grepl("Carmen|Hatillo|Mata Redonda|Pavas|Sureste|Aguas Zarcas|Ciudad Quesada|Florencia|Santa Rosa|Barranca|Islas|Peninsular|Los Santos|DARS", ars$documento), c("region", "documento")])

# Polígonos de las 9 regiones
reg <- merge(can[, c("NAME_2")], mapeo[, c("canton", "region")], by.x = "NAME_2", by.y = "canton")
reg <- aggregate(reg["region"], by = list(region = reg$region), FUN = function(x) x[1])
reg <- reg[, "region"]
st_write(reg, "datos/regiones_rectoras.gpkg", delete_dsn = TRUE, quiet = TRUE)

# -----------------------------------------------------------------------------
# A3. WorldPop 2020 y validación contra la población de los boletines de 2020
# -----------------------------------------------------------------------------
wp <- rast(file.path(dir_esp, "cri_ppp_2020.tif"))
pob_wp <- exact_extract(wp, reg, "sum")
b <- read.csv("datos/procesados/bloques_region_anio.csv", fileEncoding = "UTF-8-BOM")
p20 <- aggregate(poblacion ~ region, data = subset(b, anio == 2020), FUN = function(x) x[1])
val <- merge(data.frame(region = reg$region, poblacion_worldpop_2020 = pob_wp), p20, by = "region", all = TRUE)
names(val)[names(val) == "poblacion"] <- "poblacion_boletines_2020"
val$dif_relativa <- (val$poblacion_worldpop_2020 - val$poblacion_boletines_2020) / val$poblacion_boletines_2020
val$supera_10pct <- abs(val$dif_relativa) > 0.10
# Misma comparación en cuotas (WorldPop sin ajuste ONU subestima el total nacional)
val$cuota_worldpop  <- val$poblacion_worldpop_2020 / sum(val$poblacion_worldpop_2020)
val$cuota_boletines <- val$poblacion_boletines_2020 / sum(val$poblacion_boletines_2020)
val$dif_relativa_cuota <- (val$cuota_worldpop - val$cuota_boletines) / val$cuota_boletines
cat("\nValidación de población por región (WorldPop 2020 vs boletines 2020):\n"); print(val, digits = 4)
cat("Total WorldPop:", sum(val$poblacion_worldpop_2020), "| total boletines:", sum(val$poblacion_boletines_2020), "\n")
write.csv(val, "modelo/resultados/validacion_poblacion_regiones.csv", row.names = FALSE)

# Grilla de 0,1°: suma de WorldPop por celda (peso fijo de población para todos los años)
plantilla <- rast(ext(-86.0, -82.5, 8.0, 11.3), resolution = 0.1, crs = "EPSG:4326")
pob01 <- resample(wp, plantilla, method = "sum")
names(pob01) <- "poblacion_wp2020"
writeRaster(pob01, "datos/procesados/poblacion_grilla_0p1.tif", overwrite = TRUE)
cat("Suma de la grilla de 0,1°:", global(pob01, "sum", na.rm = TRUE)[[1]],
    "| suma WorldPop original:", global(wp, "sum", na.rm = TRUE)[[1]], "\n")
