# Librerias necesarias
# BiocManager::install("curatedMetagenomicData")
library(dplyr)
# install.packages("DT")
library(DT)
library(tidyverse)
library(readxl)
# install.packages("zoo")
library(zoo)
# install.packages("readODS")
library(readODS)
# install.packages("writexl")
library(writexl)

rm(list=ls()) # Clean global environment 

## DATOS DE NUTRIENTES
# Establecer directorio de trabajo
setwd("/home/vant/TFM/Nutricion/")
getwd()

# Obtener datos y exploración
DIAL_nocluster <- read.csv("Originales/DietaALL_noCLus.csv")
DIAL_visitas <- readxl::read_xlsx("Originales/V1 y V4_DIAL_COMPLETO.xlsx", sheet = "UNIFICADO")
metadatos <- readxl::read_xlsx("metadatados.xlsx")
colnames(DIAL_nocluster)
colnames(DIAL_visitas)

# Comprobar que solo estan incluidas las muestras de interes
DIAL_filtro <- DIAL_nocluster |> 
  filter(ID %in% metadatos$Sample_ID)
DIAL_filtro$Visita <- 1
DIAL_filtro$Estudio <- "IMDEA"
DIAL_filtro$ID <- as.character(DIAL_filtro$ID)
DIAL_filtro$Visita <- as.character(DIAL_filtro$Visita)

# Extraer alcohol solo de la visita 1, quitar la V del nombre y pasarlo a DIAL_filtro
DIAL_visitas <- DIAL_visitas |> 
  separate(CODIGO, into = c("Estudio", "ID", "Visita"), sep = "_") |> 
  mutate(Visita = gsub("V", "", Visita)) |> 
  filter(Visita == "1") |> 
  select(-Estudio)

DIAL_filtro <- DIAL_filtro |> 
  left_join(DIAL_visitas %>% select(ALCOHOL, ID), by = "ID")

# Cargar datos nutricionales estudios escogidos
# Sacar el nombre de los cuestionarios (visitas, porque combina un FFQ y dos recordatorios) y rellenar espacios en blanco
cuestionarios <- readxl::read_xlsx("../Estudios_escogidos/Heitz-BuschartA_2016/Datos_nutricionales.xlsx", sheet = "d - food-nutrientsFoodClasses",  range = "B1:FY1", col_names = FALSE)
cuestionarios <- na.locf(as.character(unlist(cuestionarios)), fromLast = FALSE)

datos <- readxl::read_xlsx("../Estudios_escogidos/Heitz-BuschartA_2016/Datos_nutricionales.xlsx", sheet = "d - food-nutrientsFoodClasses", skip = 1)
nombres_base <- names(datos)
# Quitar los numeros que pone read_xlsx automaticamente
nombres_base_limpios <- gsub("\\.\\.\\.\\d+$", "", nombres_base)
nombres_base_sin_id <- nombres_base_limpios[-1]

# Generar nombres únicos con la fecha
nuevos_nombres <- c("ID", paste0(nombres_base_sin_id, "_", cuestionarios))
# Asignar los nuevos nombres al dataframe
names(datos) <- nuevos_nombres

# Pivotar
BuschartA_2016 <- datos |> 
  pivot_longer(
    cols = -ID,
    names_to = c("nutriente", "cuestionario"),
    names_pattern = "^(.*)_(.*)$",  # separa por el ÚLTIMO guion bajo
    values_to = "valor"
  ) |> 
  pivot_wider(
    id_cols = c(ID, cuestionario),
    names_from = nutriente,
    values_from = valor
  ) |> 
  drop_na()

BuschartA_2016 <- BuschartA_2016 |> 
  mutate(cuestionario = case_when(cuestionario == "data from food frequency questionnaire (last 6 months)" ~ "FFQ",
                                  cuestionario == "data from daily recall questionnaire (2nd visit)" ~ "2",
                                  cuestionario == "data from daily recall questionnaire (3rd visit)" ~ "3",
                                  TRUE ~ cuestionario))

write_xlsx(BuschartA_2016, "../Estudios_escogidos/Heitz-BuschartA_2016/Nutrición_corregidos.xlsx")

# Ordenar en excel las dos tablas de estudios 
# Unificación de tablas con datos IMDEA
Estudios_unificados <- read_ods("../Estudios_escogidos/Datos_organizados_Nutrientes.ods")
Datos_unificados <- bind_rows(Estudios_unificados, DIAL_filtro)

Datos_unificados$`Energía (Kcal)`[is.na(Datos_unificados$`Energía (Kcal)`)] <- Datos_unificados$CALORIAS[is.na(Datos_unificados$`Energía (Kcal)`)]
Datos_unificados$`Proteinas (g)`[is.na(Datos_unificados$`Proteinas (g)`)] <- Datos_unificados$PROTEINAS[is.na(Datos_unificados$`Proteinas (g)`)]
Datos_unificados$`Grasa total (g)`[is.na(Datos_unificados$`Grasa total (g)`)] <- Datos_unificados$LIPIDOS[is.na(Datos_unificados$`Grasa total (g)`)]
Datos_unificados$`Glucidos total (g)`[is.na(Datos_unificados$`Glucidos total (g)`)] <- Datos_unificados$CARBOHIDRA[is.na(Datos_unificados$`Glucidos total (g)`)]
Datos_unificados$`Azucares simples (g)`[is.na(Datos_unificados$`Azucares simples (g)`)] <- Datos_unificados$AZ_SENCILL[is.na(Datos_unificados$`Azucares simples (g)`)]
Datos_unificados$`Fibra dietetica (g)`[is.na(Datos_unificados$`Fibra dietetica (g)`)] <- Datos_unificados$FIBRA_VEGE[is.na(Datos_unificados$`Fibra dietetica (g)`)]
Datos_unificados$`Vitamina A (mcg)`[is.na(Datos_unificados$`Vitamina A (mcg)`)] <- Datos_unificados$A[is.na(Datos_unificados$`Vitamina A (mcg)`)]
Datos_unificados$`Vitamina D (mcg)`[is.na(Datos_unificados$`Vitamina D (mcg)`)] <- Datos_unificados$VITD[is.na(Datos_unificados$`Vitamina D (mcg)`)]
Datos_unificados$`Vitamina E (mg)`[is.na(Datos_unificados$`Vitamina E (mg)`)] <- Datos_unificados$E[is.na(Datos_unificados$`Vitamina E (mg)`)]
Datos_unificados$`Folato (mcg)`[is.na(Datos_unificados$`Folato (mcg)`)] <- Datos_unificados$AC_FOLICO[is.na(Datos_unificados$`Folato (mcg)`)]
Datos_unificados$`Vitamina B12 (mcg)`[is.na(Datos_unificados$`Vitamina B12 (mcg)`)] <- Datos_unificados$B12[is.na(Datos_unificados$`Vitamina B12 (mcg)`)]
Datos_unificados$`Calcio (mg)`[is.na(Datos_unificados$`Calcio (mg)`)] <- Datos_unificados$CALCIO[is.na(Datos_unificados$`Calcio (mg)`)]
Datos_unificados$`Hierro (mg)`[is.na(Datos_unificados$`Hierro (mg)`)] <- Datos_unificados$HIERRO[is.na(Datos_unificados$`Hierro (mg)`)]
Datos_unificados$`Sodio (mg)`[is.na(Datos_unificados$`Sodio (mg)`)] <- Datos_unificados$SODIO[is.na(Datos_unificados$`Sodio (mg)`)]
Datos_unificados$`Alcohol (g)`[is.na(Datos_unificados$`Alcohol (g)`)] <- Datos_unificados$ALCOHOL[is.na(Datos_unificados$`Alcohol (g)`)]
Datos_unificados$`Zinc (mg)`[is.na(Datos_unificados$`Zinc (mg)`)] <- Datos_unificados$CINC[is.na(Datos_unificados$`Zinc (mg)`)]
Datos_unificados2 <- Datos_unificados |> 
  rename(`Folico (mcg)` = `Folato (mcg)`, `Energía (Kcal/día)` = `Energía (Kcal)`)

Datos_unificados3 <- Datos_unificados2[, colSums(is.na(Datos_unificados2)) == 0]

# Retirar los ID de los voluntarios menores de edad o que faltan en metagenomica

Datos_unificados3 <- Datos_unificados3 |> 
  filter(!ID %in% c("M-01-05", "M-03-03", "M-03-04", "M-04-02", "M-04-03", "M-04-04", "M-04-05"))

Datos_unificados3 <- Datos_unificados3 |> 
  filter(!(ID == "M-03-01" & Visita == "FFQ"))

write_xlsx(Datos_unificados4, "Nutrientes_totales.xlsx") 

## DATOS CONSUMO ALIMENTOS
# Datos DIAL
# Los numeros para el calculo de gramos proceden de una estimación a partir de las tablas de composición de alimentos de la UCM
DIAL_alimentos <- DIAL_nocluster |> 
  select(1:(which(names(DIAL_nocluster) == "IAS") - 1)) |> 
  select(-ENERGIA) |> 
  mutate(
    Cereales_g = NR_CEREAL * 50,
    Vegetales_g = NR_VEGETAL * 100, 
    Fruta_g = NR_FRUTA * 150, 
    Lacteos_g = NR_LACTEOS * 200,
    Carnes_g = NR_CARNES * 100)
DIAL_alimentos$Estudio <- "IMDEA"
DIAL_alimentos$Visita <- "1"
DIAL_alimentos$ID <-as.character(DIAL_alimentos$ID)

# Datos Heitz-BuschartA_2016
BuschartA_2016_alimentos <- BuschartA_2016 |> 
  select("ID", "cuestionario", `Vegetables [g]`, `Cereals and cereal products [g]`, `Eggs and egg dishes [g]`, `Fats and oils [g]`, `Fish & fish products [g]`, `Fruit [g]`, `Meat and meat products [g]`, `Milk and milk products [g]`, `Non-alcoholic beverages [g]`,`Nuts and seeds [g]`, `Potatoes [g]`) |>
  mutate(across(everything(), ~ replace_na(., 0))) |> 
  rename(Visita = cuestionario, Fruta_g = `Fruit [g]`, Vegetales_g = `Vegetables [g]`, Lacteos_g = `Milk and milk products [g]`) |> 
  mutate(Carnes_g = `Meat and meat products [g]` + `Fish & fish products [g]` + `Eggs and egg dishes [g]`, Cereales_g = `Cereals and cereal products [g]` + `Nuts and seeds [g]` + `Potatoes [g]`)
BuschartA_2016_alimentos$Estudio <- "Heitz-BuschartA_2016"
  
# Datos HansenLBS_2018
HansenLBS_2018 <- read_xlsx("../Estudios_escogidos/HansenLBS_2018/Dietary_data.xlsx", sheet = "Dietary data")
HansenLBS_2018_alimentos <- HansenLBS_2018 |> 
  select("ID", "visit_number", `Ice cream, g/day`, `Juice, g/day`, `Cheese and cheese products, g/day`, `Milk and dairy products, g/day`, `Grains and starchy products, g/day`, `Vegetables and vegetable products, g/day`, `Fruit and fruit products, g/day`, `Meat and meat products, g/day`,
         `Fish and fish products, g/day`, `Fats and fat products, g/day`, `Composite foods, g/day`, `Poultry and poultry products, g/day`, `Eggs and egg products, g/day`, `Potatoes, g/day`) |> 
  mutate(across(everything(), ~ replace_na(., 0))) |> 
  mutate(Lacteos_g = `Milk and dairy products, g/day` + `Cheese and cheese products, g/day` + `Ice cream, g/day`, 
         Carnes_g = `Meat and meat products, g/day` + `Fish and fish products, g/day` + `Poultry and poultry products, g/day` + `Eggs and egg products, g/day`,
         Fruta_g = `Fruit and fruit products, g/day` + `Juice, g/day`, 
         Cereales_g = `Grains and starchy products, g/day` + `Potatoes, g/day`) |> 
  rename("Visita" = "visit_number", Vegetales_g = `Vegetables and vegetable products, g/day`) |> 
  drop_na(ID)
HansenLBS_2018_alimentos$Estudio <-"HansenLBS_2018"
HansenLBS_2018_alimentos$ID <-as.character(HansenLBS_2018_alimentos$ID)
HansenLBS_2018_alimentos$Visita <-as.character(HansenLBS_2018_alimentos$Visita)

# Unificacion de datos
Unificados_alimentos <- bind_rows(DIAL_alimentos, HansenLBS_2018_alimentos, BuschartA_2016_alimentos) |> 
  select(where(~ !any(is.na(.)))) |> 
  rename("Carnes_g/día" = "Carnes_g",
         "Vegetales_g/día" = "Vegetales_g",
         "Cereales_g/día" = "Cereales_g",
         "Fruta_g/día" = "Fruta_g",
         "Lacteos_g/día" = "Lacteos_g")

# Filtrar voluntarios menores de edad o no presentes en la metagenomica
Unificados_alimentos <- Unificados_alimentos |> 
  filter(!ID %in% c("M-01-05", "M-03-03", "M-03-04", "M-04-02", "M-04-03", "M-04-04", "M-04-05"))

Unificados_alimentos <- Unificados_alimentos |> 
  filter(!(ID == "M-03-01" & Visita == "FFQ"))

write_xlsx(Unificados_alimentos, "Alimentos_totales.xlsx") 
