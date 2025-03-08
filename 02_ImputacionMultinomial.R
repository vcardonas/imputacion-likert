#==========================================================================#
#                      Imputación Ítems Escala Likert                      #
#                        02_Imputación Multinomial                         #
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
paquetes <- c("openxlsx", "tidyverse","magrittr","dplyr","tidyr","data.table", "nnet")
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
                     suffix = c("30", "50")) |> apply(1, paste0, collapse = "")

# Convertir formatos de ítems
SSES_Train[unname(unlist(itemsHSE15))] <- lapply(SSES_Train[unname(unlist(itemsHSE15))], function(x) {factor(x, levels = c(1, 2, 3, 4, 5))})
SSES_Test[unname(unlist(itemsHSE15))] <- lapply(SSES_Test[unname(unlist(itemsHSE15))], function(x) {factor(x, levels = c(1, 2, 3, 4, 5))})

# Aplicar configuración a todas las bases
for (base in bases) {
  # Obtener el data frame a partir de su nombre como string
  temp_df <- get(base)
  
  # Convertir variables a factores
  temp_df[colnames(temp_df) != "Username_Std"] <- lapply(temp_df[colnames(temp_df) != "Username_Std"], function(x) {
    factor(x, levels = c(1, 2, 3, 4, 5))})
  
  assign(base, temp_df, envir = .GlobalEnv)  # Guardar los cambios en el entorno global
}

rm(base, temp_df)

#======================================#
#### 4. Regresión Logística Ordinal ####
#======================================#

Predicciones <- lapply(bases, function(base){
  
  # Datos de prueba
  datos <- get(base)
  
  HSE <- lapply(names(itemsHSE15), function(x){
    # Item a imputar
    y <- itemImput[[x]]
    # Items de la misma dimensión
    X <- itemsHSE15[[x]][itemsHSE15[[x]] != y]
    # Estructura del modelo
    POModel <- paste0(y, "~", paste(X, collapse = "+"))
    
    # Modelo Multinomial
    fit <- multinom(formula = POModel, data = SSES_Train)
    
    # Predecir datos faltantes
    predict(fit, newdata = datos, type = "class")
  })
  names(HSE) <- paste0(unname(unlist(itemImput)), "_", gsub("SSES_", "", base))
  HSE <- as.data.frame(HSE)
  
  datos <- cbind(datos, HSE)
  datos %<>% 
    dplyr::select(all_of(unname(unlist(itemImput))), colnames(HSE))
  
  return(datos)
})
names(Predicciones) <- bases

#======================#
#### 5. Exportación ####
#======================#

# Datos imputados
wb <- createWorkbook()

addWorksheet(wb, "MCAR_30")
writeData(wb, sheet = "MCAR_30", Predicciones[[1]])
addWorksheet(wb, "MCAR_50")
writeData(wb, sheet = "MCAR_50", Predicciones[[4]])

addWorksheet(wb, "MAR_30")
writeData(wb, sheet = "MAR_30", Predicciones[[2]])
addWorksheet(wb, "MAR_50")
writeData(wb, sheet = "MAR_50", Predicciones[[5]])

addWorksheet(wb, "NMAR_30")
writeData(wb, sheet = "NMAR_30", Predicciones[[3]])
addWorksheet(wb, "NMAR_50")
writeData(wb, sheet = "NMAR_50", Predicciones[[6]])

saveWorkbook(wb, file.path(dataPath, "Datos_ImputaciónMult.xlsx"), overwrite = TRUE)

# Información relevante
assign("PrediccionesMult", Predicciones, envir = .GlobalEnv)
rm(list = c("SSES_Train","SSES_MCAR_30","SSES_MCAR_50","SSES_MAR_30","SSES_MAR_50",
            "SSES_NMAR_30","SSES_NMAR_50", "wb", "bases", "dataPath", "myPath", "itemsHSE15", "Predicciones"))

save.image(file = "./data/02_ImputacionMult.RData")
