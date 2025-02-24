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
                     suffix = c("10", "25", "50")) |> apply(1, paste0, collapse = "")

# Ítems como variable ordenada
for (base in bases) {
  temp_df <- get(base)  # Obtener el data frame a partir de su nombre como string
  temp_df[colnames(temp_df) != "FullID"] <- lapply(temp_df[colnames(temp_df) != "FullID"], 
                                                   function(x) ordered(x, levels = c(1, 2, 3, 4, 5)))
  assign(base, temp_df, envir = .GlobalEnv)  # Guardar los cambios en el entorno global
}

rm(base, temp_df)

#======================================#
#### 4. Regresión Logística Ordinal ####
#======================================#
# Proportional Odds Model
x = "Asertividad"
base = "SSES_MAR_10"

lapply(bases, function(base){
  # datos
  datos <- get(base)
  
  HSE <- lapply(names(itemsHSE15), function(x){
    # Item a imputar
    y <- itemImput[[x]]
    # Items de la misma dimensión
    X <- itemsHSE15[[x]][itemsHSE15[[x]] != y]
    # Modelo
    POModel <- paste0(y, "~", paste(X, collapse = "+"))
    # correr proportional odds model
    fit <- polr(formula = POModel, data = datos)
    
    # Coeficientes
    coefficients <- summary(fit)$coefficients
    
    # calcular p-values
    p_value <- (1 - pnorm(abs(coefficients[ ,"t value"]), 0, 1))*2
    coefficients <- cbind(coefficients, p_value)
    
    # calcular odds ratios
    odds_ratio <- exp(coefficients[ ,"Value"])
    coefficients <- cbind(coefficients, odds_ratio)
    
    return(coefficients)
  })
  names(HSE) <- names(itemsHSE15)
  
})

#predict(model_polr, newdata = dummy_test, type = "class")

