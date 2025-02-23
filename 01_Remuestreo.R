#==========================================================================#
#                      Imputación Ítems Escala Likert                      #
#                             01_Remuestreo                                #
#                        Valentina Cardona Saldaña                         #          
#==========================================================================#

#================================#
#### 1. Directorio de trabajo ####
#================================#

# Limpiar consola
rm(list = ls())

# Conocer el directorio actual
getwd()

# Directorio de carpeta
myPath <- '/Users/valentinacardona/Documents/Code Nerd/GitHub/imputacion-likert'
setwd(myPath)

# Directorio de datos
dataPath <- "./data"

#==================================#
#### 2. Instalación de paquetes ####
#==================================#

# Lista de paquetes que se necesitan
paquetes <- c("haven", "readstata13", "Hmisc", "readxl", "openxlsx",
              "tidyverse", "magrittr", "dplyr", "tidyr", "data.table", "scales")
# Utilizar lapply para cargar cada paquete si aún no está instalado
paquetes_cargar <- lapply(paquetes, 
                          FUN = function(x){
                            if (!require(x, character.only = TRUE)) {
                              install.packages(x, dependencies = TRUE)
                              library(x, character.only = TRUE)
                            }
                          }
)
#update.packages(ask = FALSE)

rm(paquetes, paquetes_cargar)
#===============================#
#### 3. Importación de datos ####
#===============================#
list.files(dataPath)

##### 3.1 Base principal ####
# SSES 2019 - Public Use File
SSES_ST_PUF2019 <- read.dta13(file.path(dataPath, "INT_01_ST_(2021.04.14)_Public.dta"), generate.factors = TRUE)
SSES_ST_PUF2019 %<>% dplyr::mutate(across(where(is.character), ~ na_if(., "")),
                                   StdID = as.numeric(StdID))
glimpse(SSES_ST_PUF2019)

#### 3.2 Trend Scales ####
# SSES 2023 - Escalas de Tendencia
SSES_TScales <- read_sav(file.path(dataPath, "SSES R2 TREND PUF INTERNATIONAL.sav"))
# Extraer "Sitio" de ronda 1 (2019) dado que SSES2019 no tiene esta variable
SSES_TScales %<>% 
  dplyr::mutate(across(where(is.character), ~ trimws(.))) %>% 
  filter(Round == 1) %>%  
  select(SiteID, Site) %>% 
  distinct()

##### 3.3 HSE ####
# 15 Habilidades Sociales y Emocionales
varsHSE15 <- c("Asertividad" = "ASS_WLE_ADJ", "Sociabilidad" = "SOC_WLE_ADJ", "Energía" = "ENE_WLE_ADJ",
               "Resistencia al estrés" = "STR_WLE_ADJ", "Control emocional" = "EMO_WLE_ADJ", "Optimismo" = "OPT_WLE_ADJ",
               "Curiosidad" = "CUR_WLE_ADJ", "Creatividad" = "CRE_WLE_ADJ", "Tolerancia" = "TOL_WLE_ADJ",
               "Persistencia" = "PER_WLE_ADJ", "Responsabilidad" = "RES_WLE_ADJ", "Autocontrol" = "SEL_WLE_ADJ",
               "Empatía" = "EMP_WLE_ADJ", "Confianza" = "TRU_WLE_ADJ", "Cooperación" = "COO_WLE_ADJ")
# Extraer ítems aplicados en el cuestionario para cada habilidad
itemsHSE15 <- lapply(1:length(varsHSE15), function(x){
  colnames(SSES_ST_PUF2019)[grepl(paste0("^STA\\_", 
                                         gsub("_WLE_ADJ", "", varsHSE15[[x]]), 
                                         "[0-9]*$"), 
                                   colnames(SSES_ST_PUF2019), perl = TRUE)]})
names(itemsHSE15) <- names(varsHSE15)
# ítems inversos
itemsInversos <- c("STA_ASS05", "STA_COO04", "STA_CRE03", "STA_CRE07", "STA_CRE08",
                   "STA_CUR06", "STA_EMO03", "STA_EMO05", "STA_EMO06", "STA_EMO08",
                   "STA_EMP08", "STA_ENE03", "STA_ENE04", "STA_ENE06", "STA_ENE07",
                   "STA_OPT01", "STA_OPT08", "STA_PER03", "STA_PER05", "STA_PER06",
                   "STA_RES01", "STA_RES03", "STA_RES04", "STA_RES07", "STA_RES08",
                   "STA_SEL05", "STA_SEL08", "STA_SOC04", "STA_SOC08", "STA_STR02",
                   "STA_STR03", "STA_STR04", "STA_STR05", "STA_STR06", "STA_STR07",
                   "STA_STR08", "STA_TOL06", "STA_TRU05")

#===============================#
#### 4. Preparación de datos ####
#===============================#

##### 4.1 Base inicial ####
# Solo ítems a analizar
SSES_Inicial <- SSES_ST_PUF2019 %>% 
  select(FullID, SiteID,
         all_of(unname(unlist(itemsHSE15))),
         all_of(as.vector(varsHSE15)),
         st_anxtest) %>% 
  mutate(across(unname(unlist(itemsHSE15)), ~ifelse(.x %in% c(7,9), NA, .x)),
         across(unname(unlist(itemsInversos)), ~ 6 - .x),
         Site = SSES_TScales$Site[match(SiteID, SSES_TScales$SiteID)]) %>% 
  select(-SiteID) %>% relocate(Site, .after = FullID)
dim(SSES_Inicial) # 60985
colSums(is.na(SSES_Inicial))

##### 4.2 Manejo de datos faltantes ####

# Verificación de datos faltantes
datosNA <- SSES_Inicial %>% 
  mutate(missing_count = rowSums(is.na(SSES_Inicial))) %>% 
  filter(missing_count > 0) %>% 
  arrange(desc(missing_count))
## Por sitio
datosNA %>% 
  group_by(Site) %>% 
  summarise(num_NA = sum(missing_count))
## Por dimensión
datosNA %>% 
  bind_cols(
    imap_dfc(itemsHSE15, ~ {
      cols <- .x
      tibble(!!paste0("NA_", .y) := rowSums(is.na(select(datosNA, all_of(cols)))))
    })
  ) %>% 
  select(all_of(paste0("NA_", names(itemsHSE15)))) %>% 
  pivot_longer(everything()) %>% 
  group_by(name) %>% summarise(num_NA = sum(value)) %>% 
  arrange(desc(num_NA))

# Eliminar datos faltantes (temporal)
SSES_Inicial %<>% 
  na.omit()
dim(SSES_Inicial) # 54050

# Verificar
colSums(is.na(SSES_Inicial))

# # Número de ítems con datos faltantes
# length(colnames(SSES_Inicial)[colSums(is.na(SSES_Inicial)) > 0])
# # % de ítems con datos faltantes
# length(colnames(SSES_Inicial)[colSums(is.na(SSES_Inicial)) > 0]) / ncol(SSES_Inicial) * 100
# # Número de personas con datos faltantes
# nrow(datosNA)
# # % de personas con datos faltantes
# nrow(datosNA)/nrow(SSES_Inicial) * 100
# # Total de datos faltantes
# sum(datosNA$missing_count)
# # % de datos faltantes
# sum(datosNA$missing_count)/(dim(SSES_Inicial[-c(1, 122)])[1]*dim(SSES_Inicial[-c(1, 122)])[2]) * 100

##### 4.3 Escoger variables al azar ####

# Seleccionar aleatoriamente un ítem
set.seed(2025)
itemImput <- lapply(names(itemsHSE15), function(x){
  sample(itemsHSE15[[x]], 1)})
names(itemImput) <- names(itemsHSE15)

# Seleccionar item a utilizar en MAR
## Opción 1
## Menores niveles de atributo (escala donde 500 es la media, con 100 puntos de desviación estándar)
# SSES_Inicial %>%
#   mutate(
#     across(all_of(as.vector(varsHSE15)), 
#            ~ 0.1 * (1 - rescale(.x, to = c(0, 1))), # Probabilidad de NA entre 0 y 10% (inversamente proporcional al puntaje)
#            .names = "prob_NA_{.col}"),
#     across(all_of(as.vector(varsHSE15)), 
#            ~ runif(n()) < get(paste0("prob_NA_", cur_column())), # Remuestrear NA con esa probabilidad
#            .names = "eliminar_{.col}"),
#     across(all_of(unname(unlist(itemImput))),  
#            ~ ifelse(get(paste0("eliminar_", sub(".*_([A-Z]{3}).*", "\\1", cur_column()), "_WLE_ADJ")), NA, .x))  # Aplicar NA
#   ) %>%
#   select(FullID, all_of(unname(unlist(itemsHSE15))))  # Eliminar columnas auxiliares

## Opción 2
## Mayor ansiedad
# SSES_Inicial %>%
#   mutate(
#     prob_NA_st_anxtest = 0.1 * rescale(st_anxtest, to = c(0, 1)), # Probabilidad de NA entre 0 y 10% (directamente proporcional al puntaje)
#     eliminar_st_anxtest = runif(n()) < prob_NA_st_anxtest,
#     across(all_of(unname(unlist(itemImput))),  
#            ~ ifelse(eliminar_st_anxtest, NA, .x))  # Aplicar NA
#   ) %>%
#   select(-c(prob_NA_st_anxtest, eliminar_st_anxtest))  # Eliminar columnas auxiliares

## Opción 3
## ítems más correlacionados
# lapply(names(itemsHSE15), function(x){
#   mat_cor <- cor(SSES_Inicial[itemsHSE15[[x]]], method = "spearman")
#   mat_cor <- as.data.frame(mat_cor)
#   mat_cor <- mat_cor[itemImput[[x]]]
#   max_val <- max(mat_cor[[itemImput[[x]]]][mat_cor[[itemImput[[x]]]] != 1]) 
#   rownames(mat_cor[mat_cor[[itemImput[[x]]]] == max_val, , drop = FALSE])
# })

#=====================#
#### 5. Remuestreo ####
#=====================#

#### 5.1 Completamente al azar (MCAR) ####
# Cada observación tiene la misma probabilidad de perderse, sin depender de ninguna variable.

# 10% de valores perdidos al azar
set.seed(2025)
SSES_MCAR_10 <- SSES_Inicial %>%
  mutate(across(unname(unlist(itemImput)), 
                ~ ifelse(runif(n()) < 0.1, NA, .))) %>% 
  select(FullID, all_of(unname(unlist(itemsHSE15))))
colMeans(is.na(SSES_MCAR_10[unname(unlist(itemImput))]))

# 25% de valores perdidos al azar
set.seed(2025)
SSES_MCAR_25 <- SSES_Inicial %>%
  mutate(across(unname(unlist(itemImput)), 
                ~ ifelse(runif(n()) < 0.25, NA, .))) %>% 
  select(FullID, all_of(unname(unlist(itemsHSE15))))
colMeans(is.na(SSES_MCAR_25[unname(unlist(itemImput))]))

# 50% de valores perdidos al azar
set.seed(2025)
SSES_MCAR_50 <- SSES_Inicial %>%
  mutate(across(unname(unlist(itemImput)), 
                ~ ifelse(runif(n()) < 0.5, NA, .))) %>% 
  select(FullID, all_of(unname(unlist(itemsHSE15))))
colMeans(is.na(SSES_MCAR_50[unname(unlist(itemImput))]))

#### 5.2 Al azar (MAR) ####
# La probabilidad de datos faltantes depende de otras variables observadas.

# 10% de valores perdidos al azar
set.seed(2025)
SSES_MAR_10 <- SSES_Inicial %>%
  mutate(
    across(all_of(as.vector(varsHSE15)), 
           ~ 0.1 * (1 - rescale(.x, to = c(0, 1))), # Probabilidad de NA entre 0 y 10% (inversamente proporcional al puntaje)
           .names = "prob_NA_{.col}"),
    across(all_of(as.vector(varsHSE15)), 
           ~ runif(n()) < get(paste0("prob_NA_", cur_column())), # Remuestrear NA con esa probabilidad
           .names = "eliminar_{.col}"),
    across(all_of(unname(unlist(itemImput))),  
           ~ ifelse(get(paste0("eliminar_", sub(".*_([A-Z]{3}).*", "\\1", cur_column()), "_WLE_ADJ")), NA, .x))  # Aplicar NA
  ) %>%
  select(FullID, all_of(unname(unlist(itemsHSE15))))
colMeans(is.na(SSES_MAR_10[unname(unlist(itemImput))])) 

# 25% de valores perdidos al azar
set.seed(2025)
SSES_MAR_25 <- SSES_Inicial %>%
  mutate(
    across(all_of(as.vector(varsHSE15)), 
           ~ 0.25 * (1 - rescale(.x, to = c(0, 1))), # Probabilidad de NA entre 0 y 25% (inversamente proporcional al puntaje)
           .names = "prob_NA_{.col}"),
    across(all_of(as.vector(varsHSE15)), 
           ~ runif(n()) < get(paste0("prob_NA_", cur_column())), # Remuestrear NA con esa probabilidad
           .names = "eliminar_{.col}"),
    across(all_of(unname(unlist(itemImput))),  
           ~ ifelse(get(paste0("eliminar_", sub(".*_([A-Z]{3}).*", "\\1", cur_column()), "_WLE_ADJ")), NA, .x))  # Aplicar NA
  ) %>%
  select(FullID, all_of(unname(unlist(itemsHSE15))))
colMeans(is.na(SSES_MAR_25[unname(unlist(itemImput))]))

# 50% de valores perdidos al azar
set.seed(2025)
SSES_MAR_50 <- SSES_Inicial %>%
  mutate(
    across(all_of(as.vector(varsHSE15)), 
           ~ 0.5 * (1 - rescale(.x, to = c(0, 1))), # Probabilidad de NA entre 0 y 50% (inversamente proporcional al puntaje)
           .names = "prob_NA_{.col}"),
    across(all_of(as.vector(varsHSE15)), 
           ~ runif(n()) < get(paste0("prob_NA_", cur_column())), # Remuestrear NA con esa probabilidad
           .names = "eliminar_{.col}"),
    across(all_of(unname(unlist(itemImput))),  
           ~ ifelse(get(paste0("eliminar_", sub(".*_([A-Z]{3}).*", "\\1", cur_column()), "_WLE_ADJ")), NA, .x))  # Aplicar NA
  ) %>%
  select(FullID, all_of(unname(unlist(itemsHSE15))))
colMeans(is.na(SSES_MAR_50[unname(unlist(itemImput))]))

#### 5.3 No al azar (NMAR) ####
# La probabilidad de datos faltantes depende del mismo valor de la variable.

# 10% de valores perdidos al azar
set.seed(2025)
SSES_NMAR_10 <- SSES_Inicial %>%
  mutate(across(unname(unlist(itemImput)), 
                ~ ifelse(. %in% c(1, 2) & runif(n()) < 0.1, NA, .))) %>% 
  select(FullID, all_of(unname(unlist(itemsHSE15))))
colMeans(is.na(SSES_NMAR_10[unname(unlist(itemImput))]))

# 25% de valores perdidos al azar
set.seed(2025)
SSES_NMAR_25 <- SSES_Inicial %>%
  mutate(across(unname(unlist(itemImput)), 
                ~ ifelse(. %in% c(1, 2) & runif(n()) < 0.25, NA, .))) %>% 
  select(FullID, all_of(unname(unlist(itemsHSE15))))
colMeans(is.na(SSES_NMAR_25[unname(unlist(itemImput))]))

# 50% de valores perdidos al azar
set.seed(2025)
SSES_NMAR_50 <- SSES_Inicial %>%
  mutate(across(unname(unlist(itemImput)), 
                ~ ifelse(. %in% c(1, 2) & runif(n()) < 0.5, NA, .))) %>% 
  select(FullID, all_of(unname(unlist(itemsHSE15))))
colMeans(is.na(SSES_NMAR_50[unname(unlist(itemImput))]))

#======================#
#### 6. Exportación ####
#======================#

# Datos remuestreados
wb <- createWorkbook()
addWorksheet(wb, "MCAR_10")
writeData(wb, sheet = "MCAR_10", SSES_MCAR_10)
addWorksheet(wb, "MCAR_25")
writeData(wb, sheet = "MCAR_25", SSES_MCAR_25)
addWorksheet(wb, "MCAR_50")
writeData(wb, sheet = "MCAR_50", SSES_MCAR_50)

addWorksheet(wb, "MAR_10")
writeData(wb, sheet = "MAR_10", SSES_MAR_10)
addWorksheet(wb, "MAR_25")
writeData(wb, sheet = "MAR_25", SSES_MAR_25)
addWorksheet(wb, "MAR_50")
writeData(wb, sheet = "MAR_50", SSES_MAR_50)

addWorksheet(wb, "NMAR_10")
writeData(wb, sheet = "NMAR_10", SSES_NMAR_10)
addWorksheet(wb, "NMAR_25")
writeData(wb, sheet = "NMAR_25", SSES_NMAR_25)
addWorksheet(wb, "NMAR_50")
writeData(wb, sheet = "NMAR_50", SSES_NMAR_50)

saveWorkbook(wb, file.path(dataPath, "Datos_Remuestreo.xlsx"), overwrite = TRUE)

# Información relevante

rm(list = c("dataPath", "datosNA", "itemsInversos", "myPath", "SSES_Inicial", "SSES_ST_PUF2019", "SSES_TScales",
            "varsHSE15", "wb"))

save.image(file = "./data/01_Remuestreo.RData")
