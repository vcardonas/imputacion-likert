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

# Directorio temporal
dataPath <- "./data"

#==================================#
#### 2. Instalación de paquetes ####
#==================================#

# Lista de paquetes que se necesitan
paquetes <- c("haven", "readstata13", "Hmisc", "Rrepest", "readxl", "openxlsx",
              "tidyverse", "magrittr", "dplyr", "tidyr", "data.table")
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

#====================================#
#### 3. Importación y preparación ####
#====================================#
list.files(dataPath)

##### 3.1 Base principal ####
SSES_ST_PUF2019 <- read.dta13(file.path(dataPath, "INT_01_ST_(2021.04.14)_Public.dta"), generate.factors = TRUE)
glimpse(SSES_ST_PUF2019)

##### 3.2 HSE ####
varsHSE15 <- c("Asertividad" = "ASS_WLE_ADJ", "Sociabilidad" = "SOC_WLE_ADJ", "Energía" = "ENE_WLE_ADJ",
               "Resistencia al estrés" = "STR_WLE_ADJ", "Control emocional" = "EMO_WLE_ADJ", "Optimismo" = "OPT_WLE_ADJ",
               "Curiosidad" = "CUR_WLE_ADJ", "Creatividad" = "CRE_WLE_ADJ", "Tolerancia" = "TOL_WLE_ADJ",
               "Persistencia" = "PER_WLE_ADJ", "Responsabilidad" = "RES_WLE_ADJ", "Autocontrol" = "SEL_WLE_ADJ",
               "Empatía" = "EMP_WLE_ADJ", "Confianza" = "TRU_WLE_ADJ", "Cooperación" = "COO_WLE_ADJ")

itemsHSE15 <- lapply(1:length(varsHSE15), function(x){
  colnames(SSES_ST_PUF2019)[grepl(paste0("^STA\\_", 
                                         gsub("_WLE_ADJ", "", varsHSE15[[x]]), 
                                         "[0-9]*$"), 
                                   colnames(SSES_ST_PUF2019), perl = TRUE)]})
names(itemsHSE15) <- names(varsHSE15)

##### 3.3 Preparación ####

SSES_Inicial <- SSES_ST_PUF2019 %>% 
  select(FullID,
         #all_of(as.vector(varsHSE15)), 
         all_of(unname(unlist(itemsHSE15)))) %>% 
  mutate(across(unname(unlist(itemsHSE15)), ~ifelse(.x %in% c(7,9), NA, .x)))
dim(SSES_Inicial)

# Verificación de datos faltantes
SSES_Inicial$missing_count <- rowSums(is.na(SSES_Inicial))

datosNA <- SSES_Inicial %>% 
  filter(missing_count > 0) %>% 
  arrange(desc(missing_count))

# Número de ítems con datos faltantes
length(colnames(SSES_Inicial)[colSums(is.na(SSES_Inicial)) > 0])
# % de ítems con datos faltantes
length(colnames(SSES_Inicial)[colSums(is.na(SSES_Inicial)) > 0]) / ncol(SSES_Inicial) * 100
# Número de personas con datos faltantes
nrow(datosNA)
# % de personas con datos faltantes
nrow(datosNA)/nrow(SSES_Inicial) * 100
# Total de datos faltantes
sum(datosNA$missing_count)
# % de datos faltantes
sum(datosNA$missing_count)/(dim(SSES_Inicial[-c(1, 122)])[1]*dim(SSES_Inicial[-c(1, 122)])[2]) * 100

##### 3.4 Escoger variables al azar ####

# Semilla
set.seed(2025)

# Seleccionar aleatoriamente un ítem
itemImput <- lapply(names(itemsHSE15), function(x){sample(itemsHSE15[[x]], 1)})
names(itemImput) <- names(itemsHSE15)

#=====================#
#### 4. Remuestreo ####
#=====================#

#### 4.1 Completamente al azar (MCAR) ####
# Cada observación tiene la misma probabilidad de perderse, sin depender de ninguna variable.

# 10% de valores perdidos al azar
set.seed(2025)
SSES_MCAR_10 <- SSES_Inicial %>%
  mutate(across(unname(unlist(itemImput)), 
                ~ ifelse(runif(n()) < 0.1, NA, .)))  

# 20% de valores perdidos al azar
set.seed(2025)
SSES_MCAR_20 <- SSES_Inicial %>%
  mutate(across(unname(unlist(itemImput)), 
                ~ ifelse(runif(n()) < 0.2, NA, .))) 

# 50% de valores perdidos al azar
set.seed(2025)
SSES_MCAR_50 <- SSES_Inicial %>%
  mutate(across(unname(unlist(itemImput)), 
                ~ ifelse(runif(n()) < 0.5, NA, .)))  

#### 4.2 Al azar (MAR) ####
# La probabilidad de datos faltantes depende de otras variables observadas.
set.seed(2025)
SSES_MAR <- SSES_Inicial %>%
  mutate(
    Item2 = ifelse(Item1 %in% c(4, 5) & runif(n()) < 0.3, NA, Item2),  # Si Item1 es 4 o 5, mayor probabilidad de NA en Item2
    Item3 = ifelse(ID > 50 & runif(n()) < 0.3, NA, Item3)  # Mayor probabilidad de NA en Item3 si ID > 50
  )

#### 4.3 No al azar (NMAR) ####
# La probabilidad de datos faltantes depende del mismo valor de la variable.

# 10% de valores perdidos al azar
set.seed(2025)
SSES_NMAR_10 <- SSES_Inicial %>%
  mutate(across(unname(unlist(itemImput)), 
                ~ ifelse(. %in% c(4, 5) & runif(n()) < 0.1, NA, .)))

# 20% de valores perdidos al azar
set.seed(2025)
SSES_NMAR_20 <- SSES_Inicial %>%
  mutate(across(unname(unlist(itemImput)), 
                ~ ifelse(. %in% c(4, 5) & runif(n()) < 0.2, NA, .)))

# 50% de valores perdidos al azar
set.seed(2025)
SSES_NMAR_50 <- SSES_Inicial %>%
  mutate(across(unname(unlist(itemImput)), 
                ~ ifelse(. %in% c(4, 5) & runif(n()) < 0.5, NA, .)))

#======================#
#### 5. Exportación ####
#======================#

wb <- createWorkbook()
addWorksheet(wb, "MCAR_10")
writeData(wb, sheet = "MCAR_10", SSES_MCAR_10)
addWorksheet(wb, "MCAR_20")
writeData(wb, sheet = "MCAR_20", SSES_MCAR_20)
addWorksheet(wb, "MCAR_50")
writeData(wb, sheet = "MCAR_50", SSES_MCAR_50)

addWorksheet(wb, "MAR_10")
writeData(wb, sheet = "MAR_10", SSES_MAR_10)
addWorksheet(wb, "MAR_20")
writeData(wb, sheet = "MAR_20", SSES_MAR_20)
addWorksheet(wb, "MAR_50")
writeData(wb, sheet = "MAR_50", SSES_MAR_50)

addWorksheet(wb, "NMAR_10")
writeData(wb, sheet = "NMAR_10", SSES_NMAR_10)
addWorksheet(wb, "NMAR_20")
writeData(wb, sheet = "NMAR_20", SSES_NMAR_20)
addWorksheet(wb, "NMAR_50")
writeData(wb, sheet = "NMAR_50", SSES_NMAR_50)

saveWorkbook(wb, file.path(dataPath, "Datos_Remuestreo.xlsx"), overwrite = TRUE)



