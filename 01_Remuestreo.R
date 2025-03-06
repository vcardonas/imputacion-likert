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
  dplyr::select(SiteID, Site) %>% 
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
  dplyr::select(FullID, SiteID,
         all_of(unname(unlist(itemsHSE15))),
         all_of(as.vector(varsHSE15)),
         st_anxtest) %>% 
  mutate(across(unname(unlist(itemsHSE15)), ~ifelse(.x %in% c(7,9), NA, .x)),
         across(unname(unlist(itemsInversos)), ~ 6 - .x),
         Site = SSES_TScales$Site[match(SiteID, SSES_TScales$SiteID)]) %>% 
  dplyr::select(-SiteID) %>% relocate(Site, .after = FullID)
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
      tibble(!!paste0("NA_", .y) := rowSums(is.na(dplyr::select(datosNA, all_of(cols)))))
    })
  ) %>% 
  dplyr::select(all_of(paste0("NA_", names(itemsHSE15)))) %>% 
  pivot_longer(everything()) %>% 
  group_by(name) %>% summarise(num_NA = sum(value)) %>% 
  arrange(desc(num_NA))

# Eliminar datos faltantes
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

#===============================#
#### 5. Partición de datos ####
#===============================#

# Escoger aleatoriamente los datos
## Datos de Entrenamiento (70%)
## Datos de Prueba (30%)
set.seed(2025)
train1 <- sample(1:nrow(SSES_Inicial), size = 0.7*nrow(SSES_Inicial), replace = FALSE)
train <- rep(FALSE, nrow(SSES_Inicial))
train[train1] <- TRUE
test <- (!train)

# Particionar datos
SSES_Train <- SSES_Inicial[train, ]
SSES_Test <- SSES_Inicial[test, ]

nrow(SSES_Train)
nrow(SSES_Test)

#=====================#
#### 6. Remuestreo ####
#=====================#

#### 6.1 Completamente al azar (MCAR) ####
# Cada observación tiene la misma probabilidad de perderse, sin depender de ninguna variable.

# 30% de valores perdidos al azar
set.seed(2025)
SSES_MCAR_30 <- SSES_Test %>%
  mutate(across(unname(unlist(itemImput)), 
                ~ ifelse(runif(n()) < 0.3, NA, .))) %>% 
  dplyr::select(FullID, all_of(unname(unlist(itemsHSE15))))
colMeans(is.na(SSES_MCAR_30[unname(unlist(itemImput))]))

# 50% de valores perdidos al azar
set.seed(2025)
SSES_MCAR_50 <- SSES_Test %>%
  mutate(across(unname(unlist(itemImput)), 
                ~ ifelse(runif(n()) < 0.5, NA, .))) %>% 
  dplyr::select(FullID, all_of(unname(unlist(itemsHSE15))))
colMeans(is.na(SSES_MCAR_50[unname(unlist(itemImput))]))

# 70% de valores perdidos al azar
set.seed(2025)
SSES_MCAR_70 <- SSES_Test %>%
  mutate(across(unname(unlist(itemImput)), 
                ~ ifelse(runif(n()) < 0.7, NA, .))) %>% 
  dplyr::select(FullID, all_of(unname(unlist(itemsHSE15))))
colMeans(is.na(SSES_MCAR_70[unname(unlist(itemImput))]))

#### 6.2 Al azar (MAR) ####
# La probabilidad de datos faltantes depende de otras variables observadas.

# 30% de valores perdidos al azar
set.seed(2025)
SSES_MAR_30 <- SSES_Test %>%
  mutate(
    across(all_of(as.vector(varsHSE15)), 
           ~ 0.3 * (1 - rescale(.x, to = c(0, 1))), # Probabilidad de NA entre 0 y 30% (inversamente proporcional al puntaje)
           .names = "prob_NA_{.col}"),
    across(all_of(as.vector(varsHSE15)), 
           ~ runif(n()) < get(paste0("prob_NA_", cur_column())), # Remuestrear NA con esa probabilidad
           .names = "eliminar_{.col}"),
    across(all_of(unname(unlist(itemImput))),  
           ~ ifelse(get(paste0("eliminar_", sub(".*_([A-Z]{3}).*", "\\1", cur_column()), "_WLE_ADJ")), NA, .x))  # Aplicar NA
  ) %>%
  dplyr::select(FullID, all_of(unname(unlist(itemsHSE15))))
colMeans(is.na(SSES_MAR_30[unname(unlist(itemImput))]))

# 50% de valores perdidos al azar
set.seed(2025)
SSES_MAR_50 <- SSES_Test %>%
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
  dplyr::select(FullID, all_of(unname(unlist(itemsHSE15))))
colMeans(is.na(SSES_MAR_50[unname(unlist(itemImput))]))

# 70% de valores perdidos al azar
set.seed(2025)
SSES_MAR_70 <- SSES_Test %>%
  mutate(
    across(all_of(as.vector(varsHSE15)), 
           ~ 0.7 * (1 - rescale(.x, to = c(0, 1))), # Probabilidad de NA entre 0 y 70% (inversamente proporcional al puntaje)
           .names = "prob_NA_{.col}"),
    across(all_of(as.vector(varsHSE15)), 
           ~ runif(n()) < get(paste0("prob_NA_", cur_column())), # Remuestrear NA con esa probabilidad
           .names = "eliminar_{.col}"),
    across(all_of(unname(unlist(itemImput))),  
           ~ ifelse(get(paste0("eliminar_", sub(".*_([A-Z]{3}).*", "\\1", cur_column()), "_WLE_ADJ")), NA, .x))  # Aplicar NA
  ) %>%
  dplyr::select(FullID, all_of(unname(unlist(itemsHSE15))))
colMeans(is.na(SSES_MAR_70[unname(unlist(itemImput))]))

#### 6.3 No al azar (NMAR) ####
# La probabilidad de datos faltantes depende del mismo valor de la variable.

# 30% de valores perdidos al azar
set.seed(2025)
SSES_NMAR_30 <- SSES_Test %>%
  mutate(across(unname(unlist(itemImput)), 
                ~ ifelse(. %in% c(1, 2) & runif(n()) < 0.3, NA, .))) %>% 
  dplyr::select(FullID, all_of(unname(unlist(itemsHSE15))))
colMeans(is.na(SSES_NMAR_30[unname(unlist(itemImput))]))

# 50% de valores perdidos al azar
set.seed(2025)
SSES_NMAR_50 <- SSES_Test %>%
  mutate(across(unname(unlist(itemImput)), 
                ~ ifelse(. %in% c(1, 2) & runif(n()) < 0.5, NA, .))) %>% 
  dplyr::select(FullID, all_of(unname(unlist(itemsHSE15))))
colMeans(is.na(SSES_NMAR_50[unname(unlist(itemImput))]))

# 70% de valores perdidos al azar
set.seed(2025)
SSES_NMAR_70 <- SSES_Test %>%
  mutate(across(unname(unlist(itemImput)), 
                ~ ifelse(. %in% c(1, 2) & runif(n()) < 0.7, NA, .))) %>% 
  dplyr::select(FullID, all_of(unname(unlist(itemsHSE15))))
colMeans(is.na(SSES_NMAR_70[unname(unlist(itemImput))]))

#======================#
#### 7. Exportación ####
#======================#

# Datos remuestreados
wb <- createWorkbook()
addWorksheet(wb, "SSES_Train")
writeData(wb, sheet = "SSES_Train", SSES_Train)
addWorksheet(wb, "SSES_Test")
writeData(wb, sheet = "SSES_Test", SSES_Test)

addWorksheet(wb, "MCAR_30")
writeData(wb, sheet = "MCAR_30", SSES_MCAR_30)
addWorksheet(wb, "MCAR_50")
writeData(wb, sheet = "MCAR_50", SSES_MCAR_50)
addWorksheet(wb, "MCAR_70")
writeData(wb, sheet = "MCAR_70", SSES_MCAR_70)

addWorksheet(wb, "MAR_30")
writeData(wb, sheet = "MAR_30", SSES_MAR_30)
addWorksheet(wb, "MAR_50")
writeData(wb, sheet = "MAR_50", SSES_MAR_50)
addWorksheet(wb, "MAR_70")
writeData(wb, sheet = "MAR_70", SSES_MAR_70)

addWorksheet(wb, "NMAR_30")
writeData(wb, sheet = "NMAR_30", SSES_NMAR_30)
addWorksheet(wb, "NMAR_50")
writeData(wb, sheet = "NMAR_50", SSES_NMAR_50)
addWorksheet(wb, "NMAR_70")
writeData(wb, sheet = "NMAR_70", SSES_NMAR_70)

saveWorkbook(wb, file.path(dataPath, "Datos_Remuestreo.xlsx"), overwrite = TRUE)

# Información relevante

rm(list = c("dataPath", "datosNA", "itemsInversos", "myPath", "SSES_Inicial", "SSES_ST_PUF2019", "SSES_TScales",
            "varsHSE15", "wb", "test", "train", "train1"))

save.image(file = "./data/01_Remuestreo.RData")
