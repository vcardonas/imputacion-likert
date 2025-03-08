#==========================================================================#
#                      Imputación Ítems Escala Likert                      #
#                             02_Evaluación                                #
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
paquetes <- c("openxlsx", "tidyverse","magrittr","dplyr","tidyr","data.table",
              "purrr", "pracma", "ggplot2")
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
#### 4. Importación ####
#======================#
list.files(dataPath)

# Cargar datos de imputación
load("./data/02_ImputacionOrd.RData")
load("./data/02_ImputacionMult.RData")

#====================================#
#### 5. Evaluación de los modelos ####
#====================================#

# Extraer datos reales de los datos de prueba
Reales <- SSES_Test %>% dplyr::select("Username_Std", unname(unlist(itemImput)))

#### 5.1 Multinomial ####
metricasMult <- lapply(names(PrediccionesMult), function(x){
  base <- cbind(SSES_Test["Username_Std"], PrediccionesMult[[x]])
  
  metricas <- lapply(unname(unlist(itemImput)), function(HSE){
    datos_HSE <- base %>% 
      dplyr::select("Username_Std", starts_with(HSE)) %>% 
      filter(is.na(!!sym(HSE))) %>% 
      dplyr::select(-HSE) %>% 
      left_join(y = Reales[c("Username_Std", HSE)], by = "Username_Std")
    colnames(datos_HSE) <- c("Username_Std", "Predicción", "Real")
    
    # Número de valores imputados
    N <- nrow(datos_HSE)
    # Número de valores imputados correctamente
    A <- nrow(datos_HSE[as.character(datos_HSE$Predicción) == as.character(datos_HSE$Real), ])
    
    # Overall Accuracy
    OA <- round(A/N, 3)
    
    # Matriz de confusión
    MC <- table(datos_HSE[-1], deparse.level = 2)
    MC <- as.data.frame(MC) %>%
      mutate(Predicción = paste0("P", Predicción),
             Real = paste0("R", Real)) %>%
      unite("Etiqueta", Real, Predicción, sep = "-") %>%
      pivot_wider(names_from = Etiqueta, values_from = Freq)
    
    # Error Absoluto Medio
    MAE <- sum(abs(as.numeric(datos_HSE$Real) - as.numeric(datos_HSE$Predicción)))/N
    MAE <- round(MAE, 3)
    
    # Etiquetas
    Porcentaje <- str_extract(x, "[0-9]+")
    Tipo <- gsub(paste0("_", Porcentaje), "", gsub("SSES_", "", x))
    
    # Retornar todo
    metricas <- cbind(HSE, Tipo, Porcentaje, N, A, OA, MAE, MC)
    return(metricas)
  })
  
  metricas <- bind_rows(metricas)
  return(metricas)
})
metricasMult <- bind_rows(metricasMult) %>% 
  mutate(Modelo = "Multinomial") %>% relocate(Modelo, .before = everything())

#### 5.2 Ordinal ####
metricasOrd <- lapply(names(PrediccionesOrd), function(x){
  base <- cbind(SSES_Test["Username_Std"], PrediccionesOrd[[x]])
  
  metricas <- lapply(unname(unlist(itemImput)), function(HSE){
    datos_HSE <- base %>% 
      dplyr::select("Username_Std", starts_with(HSE)) %>% 
      filter(is.na(!!sym(HSE))) %>% 
      dplyr::select(-HSE) %>% 
      left_join(y = Reales[c("Username_Std", HSE)], by = "Username_Std")
    colnames(datos_HSE) <- c("Username_Std", "Predicción", "Real")
    
    # Número de valores imputados
    N <- nrow(datos_HSE)
    # Número de valores imputados correctamente
    A <- nrow(datos_HSE[as.character(datos_HSE$Predicción) == as.character(datos_HSE$Real), ])
    
    # Overall Accuracy
    OA <- round(A/N, 3)
    
    # Matriz de confusión
    MC <- table(datos_HSE[-1], deparse.level = 2)
    MC <- as.data.frame(MC) %>%
      mutate(Predicción = paste0("P", Predicción),
             Real = paste0("R", Real)) %>%
      unite("Etiqueta", Real, Predicción, sep = "-") %>%
      pivot_wider(names_from = Etiqueta, values_from = Freq)
    
    # Error Absoluto Medio
    MAE <- sum(abs(as.numeric(datos_HSE$Real) - as.numeric(datos_HSE$Predicción)))/N
    MAE <- round(MAE, 3)
    
    # Etiquetas
    Porcentaje <- str_extract(x, "[0-9]+")
    Tipo <- gsub(paste0("_", Porcentaje), "", gsub("SSES_", "", x))
    
    # Retornar todo
    metricas <- cbind(HSE, Tipo, Porcentaje, N, A, OA, MAE, MC)
    return(metricas)
  })
  
  metricas <- bind_rows(metricas)
  return(metricas)
})
metricasOrd <- bind_rows(metricasOrd) %>% 
  mutate(Modelo = "Ordinal") %>% relocate(Modelo, .before = everything())

#### 5.3 Métricas conjuntas ####

metricas <- rbind(metricasMult, metricasOrd)

MC <- metricas %>% 
  dplyr::select(Modelo, Tipo, Porcentaje, starts_with("R")) %>% 
  group_by(Modelo, Tipo, Porcentaje) %>% 
  summarise(across(starts_with("R"), ~sum(.)), .groups = "drop")

metricas %<>% 
  dplyr::select(-starts_with("R")) %>% 
  rename("Item" = "HSE") %>% 
  mutate(HSE = sapply(Item, function(x) {
    nombre <- names(itemImput)[sapply(itemImput, function(y) x %in% y)]
    if (length(nombre) == 0) NA else nombre
  })) %>% relocate(HSE, .after = Modelo)

# Curva Sumas Acumuladas
CS <- MC %>% 
  rowwise() %>% 
  mutate(`0` = sum(`R1-P1`, `R2-P2`, `R3-P3`, `R4-P4`, `R5-P5`),
         `1` = sum(`0`, 
                   `R1-P2`,`R2-P1`,`R2-P3`,`R3-P2`,`R3-P4`,`R4-P3`,`R4-P5`,`R5-P4`),
         `2` = sum(`0`, `1`, 
                   `R1-P3`,`R2-P4`,`R3-P1`,`R3-P5`,`R4-P2`,`R5-P3`),
         `3` = sum(`0`, `1`, `2`,
                   `R1-P4`,`R2-P5`,`R4-P1`,`R5-P2`),
         `4` = sum(`0`, `1`, `2`, `3`,
                   `R1-P5`,`R5-P1`),
         across(`0`:`4`, ~.x/`4`*100)) %>% 
  dplyr::select(-starts_with("R")) %>% 
  pivot_longer(`0`:`4`, names_to = "Distancia", values_to = "CS(%)") %>% 
  mutate(ref = case_when(Distancia == 0 ~ 0,
                           Distancia == 1 ~ 25,
                           Distancia == 2 ~ 50,
                           Distancia == 3 ~ 75,
                           Distancia == 4 ~ 100))

# Área entre Curvas
#ABC
CS %>%
  mutate(Distancia = as.numeric(Distancia),
         `CS(%)` = as.numeric(`CS(%)`),
         ref = as.numeric(ref)) %>%
  arrange(Modelo, Tipo, Porcentaje, Distancia) %>% 
  group_by(Modelo, Tipo, Porcentaje) %>%
  summarise(ABC = trapz(Distancia, `CS(%)` - ref), .groups = "drop")

#### 5.4 Exportar ####

wb <- createWorkbook()

addWorksheet(wb, "Metricas")
writeData(wb, sheet = "Metricas", metricas)
addWorksheet(wb, "MatricesConfusion")
writeData(wb, sheet = "MatricesConfusion", MC)
addWorksheet(wb, "CurvaSumas")
writeData(wb, sheet = "CurvaSumas", CS)

saveWorkbook(wb, "./output/Resultados.xlsx", overwrite = TRUE)
rm(wb)

#===================#
#### 6. Gráficas ####
#===================#

#### 6.1 Métricas ####
## Limites
# OA = 0 a 1
# MSE = 0 a 4
metricas %>% 
  filter(Tipo == "MCAR") %>% 
  mutate(Porcentaje = paste0(Porcentaje, "%")) %>% 
  ggplot(aes(x = HSE)) +
  geom_point(aes(y = OA, colour = Porcentaje, shape = "OA"), size = 3) + 
  geom_bar(aes(y = MAE * 0.25, fill = Porcentaje), stat = "identity", position = "dodge", alpha = 0.7) + 
  scale_y_continuous(name = "OA", limits = c(0, 1),
                     sec.axis = sec_axis(~ . / 0.25, name = "MAE", breaks = seq(0, 4, by = 1))) +
  scale_colour_manual(values = c("30%" = "blue3", "50%" = "orange")) +
  scale_fill_manual(values = c("30%" = "blue3", "50%" = "orange")) +
  scale_shape_manual(labels = c('MAE', 'OA'), values = c(15, 16)) +
  facet_grid(~Modelo) +
  theme_minimal(base_size = 13) +
  labs(colour = NULL, fill = NULL, shape = NULL) +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray85", linetype = "dashed"),
        panel.border = element_rect(fill = "transparent", color = "black", linewidth = 1),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 15, face = "bold")) +
  guides(shape = guide_legend(override.aes = list(size = 4))) + 
  geom_point(aes(x = Inf, y = Inf, shape = "MAE"), size = 3, colour = "black")  # Punto invisible para forzar la leyenda
ggsave("./output/metricas_MCAR.png", width = 10, height = 6, dpi = 600)
dev.off()

metricas %>% 
  filter(Tipo == "MAR") %>% 
  mutate(Porcentaje = paste0(Porcentaje, "%")) %>% 
  ggplot(aes(x = HSE)) +
  geom_point(aes(y = OA, colour = Porcentaje, shape = "OA"), size = 3) + 
  geom_bar(aes(y = MAE * 0.25, fill = Porcentaje), stat = "identity", position = "dodge", alpha = 0.7) + 
  scale_y_continuous(name = "OA", limits = c(0, 1),
                     sec.axis = sec_axis(~ . / 0.25, name = "MAE", breaks = seq(0, 4, by = 1))) +
  scale_colour_manual(values = c("30%" = "blue3", "50%" = "orange")) +
  scale_fill_manual(values = c("30%" = "blue3", "50%" = "orange")) +
  scale_shape_manual(labels = c('MAE', 'OA'), values = c(15, 16)) +
  facet_grid(~Modelo) +
  theme_minimal(base_size = 13) +
  labs(colour = NULL, fill = NULL, shape = NULL) +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray85", linetype = "dashed"),
        panel.border = element_rect(fill = "transparent", color = "black", linewidth = 1),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 15, face = "bold")) +
  guides(shape = guide_legend(override.aes = list(size = 4))) + 
  geom_point(aes(x = Inf, y = Inf, shape = "MAE"), size = 3, colour = "black")  # Punto invisible para forzar la leyenda
ggsave("./output/metricas_MAR.png", width = 10, height = 6, dpi = 600)
dev.off()

metricas %>% 
  filter(Tipo == "NMAR") %>% 
  mutate(Porcentaje = paste0(Porcentaje, "%")) %>% 
  ggplot(aes(x = HSE)) +
  geom_point(aes(y = OA, colour = Porcentaje, shape = "OA"), size = 3) + 
  geom_bar(aes(y = MAE * 0.25, fill = Porcentaje), stat = "identity", position = "dodge", alpha = 0.7) + 
  scale_y_continuous(name = "OA", limits = c(0, 1),
                     sec.axis = sec_axis(~ . / 0.25, name = "MAE", breaks = seq(0, 4, by = 1))) +
  scale_colour_manual(values = c("30%" = "blue3", "50%" = "orange")) +
  scale_fill_manual(values = c("30%" = "blue3", "50%" = "orange")) +
  scale_shape_manual(labels = c('MAE', 'OA'), values = c(15, 16)) +
  facet_grid(~Modelo) +
  theme_minimal(base_size = 13) +
  labs(colour = NULL, fill = NULL, shape = NULL) +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray85", linetype = "dashed"),
        panel.border = element_rect(fill = "transparent", color = "black", linewidth = 1),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 15, face = "bold")) +
  guides(shape = guide_legend(override.aes = list(size = 4))) + 
  geom_point(aes(x = Inf, y = Inf, shape = "MAE"), size = 3, colour = "black")  # Punto invisible para forzar la leyenda
ggsave("./output/metricas_NMAR.png", width = 10, height = 6, dpi = 600)
dev.off()


#### 6.1 Curva Sumas Acumuladas ####

# 30%
CS %>% 
  filter(Porcentaje == "30") %>% 
  mutate(Tipo = factor(Tipo, levels = c("MCAR", "MAR", "NMAR"))) %>% 
  ggplot(aes(x = Distancia, y = `CS(%)`, group = Modelo, color = Modelo)) +
  geom_line(alpha = 0.5) + 
  geom_point(aes(shape = Modelo), size = 2.5) +
  geom_line(aes(y = ref),linetype = "dotted", color = "red") +
  scale_y_continuous(labels = scales::percent_format(scale = 1))  +
  scale_colour_manual(values = c("blue3", "orange")) +
  theme_minimal(base_size = 13) +
  labs(color = NULL, shape = NULL) +
  facet_wrap(~Tipo) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray85", linetype = "dashed"),
        panel.border = element_rect(fill = "transparent", color = "black", linewidth = 1)) 
ggsave("./output/CS_30.png", width = 8, height = 4, dpi = 600)
dev.off()

# 50%
CS %>% 
  filter(Porcentaje == "50") %>% 
  mutate(Tipo = factor(Tipo, levels = c("MCAR", "MAR", "NMAR"))) %>% 
  mutate(linea = case_when(Distancia == 0 ~ 0,
                           Distancia == 1 ~ 25,
                           Distancia == 2 ~ 50,
                           Distancia == 3 ~ 75,
                           Distancia == 4 ~ 100)) %>% 
  ggplot(aes(x = Distancia, y = `CS(%)`, group = Modelo, color = Modelo)) +
  geom_line(alpha = 0.5) + 
  geom_point(aes(shape = Modelo), size = 2.5) +
  geom_line(aes(y = linea),linetype = "dotted", color = "red") +
  scale_y_continuous(labels = scales::percent_format(scale = 1))  +
  scale_colour_manual(values = c("blue3", "orange")) +
  theme_minimal(base_size = 13) +
  labs(color = NULL, shape = NULL) +
  facet_wrap(~Tipo) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray85", linetype = "dashed"),
        panel.border = element_rect(fill = "transparent", color = "black", linewidth = 1)) 
ggsave("./output/CS_50.png", width = 8, height = 4, dpi = 600)
dev.off()
