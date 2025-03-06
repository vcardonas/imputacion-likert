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
paquetes <- c("openxlsx", "tidyverse","magrittr","dplyr","tidyr","data.table", "MASS")
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

#====================================#
#### 3. Importación y Preparación ####
#====================================#
list.files(dataPath)

# Cargar datos de remuestreo
load("./data/01_Remuestreo.RData")

# Lista de bases
bases <- expand.grid(prefix = factor(paste0("SSES_", c("MCAR_", "MAR_", "NMAR_"))),
                     suffix = c("30", "50", "70")) |> apply(1, paste0, collapse = "")

# Convertir formatos de ítems
SSES_Train[unname(unlist(itemsHSE15))] <- lapply(SSES_Train[unname(unlist(itemsHSE15))], as.factor)
SSES_Train[unname(unlist(itemImput))] <- lapply(SSES_Train[unname(unlist(itemImput))], function(x) ordered(x, levels = c(1, 2, 3, 4, 5)))

SSES_Test[unname(unlist(itemsHSE15))] <- lapply(SSES_Test[unname(unlist(itemsHSE15))], as.factor)
SSES_Test[unname(unlist(itemImput))] <- lapply(SSES_Test[unname(unlist(itemImput))], function(x) ordered(x, levels = c(1, 2, 3, 4, 5)))

# 
for (base in bases) {
  temp_df <- get(base)  # Obtener el data frame a partir de su nombre como string
  
  # Convertir variables a factores
  temp_df[colnames(temp_df) != "FullID"] <- lapply(temp_df[colnames(temp_df) != "FullID"], as.factor)
  
  # Convertir ítems a imputar a categorías ordenadas
  temp_df[unname(unlist(itemImput))] <- lapply(temp_df[unname(unlist(itemImput))],
                                               function(x) ordered(x, levels = c(1, 2, 3, 4, 5)))
  
  assign(base, temp_df, envir = .GlobalEnv)  # Guardar los cambios en el entorno global
}

rm(base, temp_df)

#======================================#
#### 4. Regresión Logística Ordinal ####
#======================================#

x = "Asertividad"
base = "SSES_MAR_30"

lapply(bases, function(base){
  # datos
  datos <- get(base)
  
  HSE <- lapply(names(itemsHSE15), function(x){
    # Item a imputar
    y <- itemImput[[x]]
    # Items de la misma dimensión
    X <- itemsHSE15[[x]][itemsHSE15[[x]] != y]
    # Estructura del modelo
    POModel <- paste0(y, "~", paste(X, collapse = "+"))
    
    # Proportional Odds Logistic Regression
    fit <- polr(formula = POModel, method = "logistic", data = SSES_Train)
    
    # Predecir datos faltantes
    predict(fit, newdata = datos, type = "class")
  })
  names(HSE) <- unname(unlist(itemImput))
  HSE <- as.data.frame(HSE)
})

predict(fit, newdata = datos[is.na(datos$STA_ASS05),], type = "class")

