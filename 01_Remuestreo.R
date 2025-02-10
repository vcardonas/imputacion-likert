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
inPath <- "./data"

#==================================#
#### 2. Instalación de paquetes ####
#==================================#

# Lista de paquetes que se necesitan
paquetes <- c("haven", "readstata13", "Hmisc","Rrepest", "readxl",
              "tidyverse","magrittr","dplyr","tidyr","data.table")
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
list.files(inPath)

##### 3.1 Base principal ####
SSES_ST_PUF2019 <- read.dta13(file.path(inPath, "INT_01_ST_(2021.04.14)_Public.dta"), generate.factors = TRUE)
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
  select(FullID, CohortID,
         #all_of(as.vector(varsHSE15)), 
         all_of(unname(unlist(itemsHSE15))))
dim(SSES_Inicial)

# Verificación de datos faltantes
colnames(SSES_Inicial)[colSums(is.na(SSES_Inicial)) > 0]

#=====================#
#### 4. Remuestreo ####
#=====================#

#### 4.1 Completamente al azar (MCAR) ####
# Cada observación tiene la misma probabilidad de perderse, sin depender de ninguna variable.

SSES_MCAR <- SSES_Inicial %>%
  mutate(across(Item1:Item4, 
                ~ ifelse(runif(n()) < 0.2, NA, .)))  # 20% de valores perdidos al azar

#### 4.2 Al azar (MAR) ####
# La probabilidad de datos faltantes depende de otras variables observadas.
SSES_MAR <- SSES_Inicial %>%
  mutate(
    Item2 = ifelse(Item1 %in% c(4,5) & runif(n()) < 0.3, NA, Item2),  # Si Item1 es 4 o 5, mayor probabilidad de NA en Item2
    Item3 = ifelse(ID > 50 & runif(n()) < 0.3, NA, Item3)  # Mayor probabilidad de NA en Item3 si ID > 50
  )

#### 4.3 No al azar (NMAR) ####
# La probabilidad de datos faltantes depende del mismo valor de la variable.
SSES_NMAR <- SSES_Inicial %>%
  mutate(
    Item4 = ifelse(Item4 == 5 & runif(n()) < 0.4, NA, Item4)  # Si Item4 es 5, hay 40% de probabilidad de ser NA
  )

#======================#
#### 5. Exportación ####
#======================#

