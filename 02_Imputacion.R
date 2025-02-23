#==========================================================================#
#                      Imputación Ítems Escala Likert                      #
#                             02_Imputación                                #
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
paquetes <- c("openxlsx", "tidyverse","magrittr","dplyr","tidyr","data.table")
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

#======================#
#### 3. Importación ####
#======================#
list.files(dataPath)

# Cargar datos de remuestreo
load("./data/01_Remuestreo.RData")

#======================================#
#### 4. Regresión Logística Ordinal ####
#======================================#













