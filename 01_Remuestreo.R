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

#======================#
#### 3. Importación ####
#======================#
list.files(inPath)

SSES_ST_PUF2019 <- read.dta13(file.path(inPath, "INT_01_ST_(2021.04.14)_Public.dta"), generate.factors = TRUE)

# #### HSE ####
# varsBig5 <- list("Involucramiento con otros" = c("ASS_WLE_ADJ","SOC_WLE_ADJ","ENE_WLE_ADJ"),
#                  "Regulación emocional" = c("STR_WLE_ADJ","EMO_WLE_ADJ","OPT_WLE_ADJ"),
#                  "Apertura mental" = c("CUR_WLE_ADJ","CRE_WLE_ADJ","TOL_WLE_ADJ"),
#                  "Desempeño de tareas" = c("PER_WLE_ADJ","RES_WLE_ADJ","SEL_WLE_ADJ","MOT_WLE_ADJ"),
#                  "Colaboración" = c("EMP_WLE_ADJ","TRU_WLE_ADJ"))
# varsHSE15 <- c("Asertividad" = "ASS_WLE_ADJ", "Sociabilidad" = "SOC_WLE_ADJ", "Energía" = "ENE_WLE_ADJ",
#                "Resistencia al estrés" = "STR_WLE_ADJ", "Control emocional" = "EMO_WLE_ADJ", "Optimismo" = "OPT_WLE_ADJ",
#                "Curiosidad" = "CUR_WLE_ADJ", "Creatividad" = "CRE_WLE_ADJ", "Tolerancia" = "TOL_WLE_ADJ",
#                "Persistencia" = "PER_WLE_ADJ", "Responsabilidad" = "RES_WLE_ADJ", "Autocontrol" = "SEL_WLE_ADJ", "Motivación al logro" = "MOT_WLE_ADJ",
#                "Empatía" = "EMP_WLE_ADJ", "Confianza" = "TRU_WLE_ADJ")
# ## Bogotá 2023
# SSES_ST_PUF_BOG <- SSES_ST_PUF2019 %>% filter(Site == "BOG")

