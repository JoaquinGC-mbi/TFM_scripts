library(BiocManager)
library(dplyr)
# install.packages("DT")
library(DT)
library(tidyverse)
library(readxl)
# install.packages("readODS")
library(readODS)
# install.packages("writexl")
library(writexl)
library(stringr)

# Carga de archivos de abundancia de alimentos y nutrientes
rm(list=ls()) # Clean global environment 

setwd("/home/vant/TFM")
getwd()

### Cargar bases de datos 
## Metadata
Metadata <- read_xlsx("Microbiota/Metadata_combinado.xlsx")

# Edad y IMC
resumen_por_estudio <- Metadata |> 
  group_by(study_name) |>   
  summarise(
    Media_age = mean(age, na.rm = TRUE),
    SD_age = sd(age, na.rm = TRUE),
    SE_age = sd(age, na.rm = TRUE) / sqrt(sum(!is.na(age))),
    Media_BMI = mean(BMI, na.rm = TRUE),
    SD_BMI = sd(BMI, na.rm = TRUE),
    SE_BMI = sd(BMI, na.rm = TRUE) / sqrt(sum(!is.na(BMI))))

# Para el genero y N
# Primero, extraemos el ID único eliminando la parte de la visita
Metadata_unico <- Metadata |> 
  mutate(ID_unico = str_extract(sample_id, "^[^-]+"))  

# Agrupamos por estudio y contamos
resumen_sexo <- Metadata_unico |> 
  distinct(study_name, ID_unico, .keep_all = TRUE) %>%  # Tomamos cada sujeto solo una vez
  group_by(study_name) |> 
  summarise(
    N = n(),  # Número total de sujetos únicos
    Mujeres = sum(gender == "female", na.rm = TRUE),
    Hombres = sum(gender == "male", na.rm = TRUE))

## Nutricion
Nutrición_todo <- read_xlsx("Nutricion/Unificado_nutrición_indices.xlsx")

# TABLA RESUMEN

tabla_resumen <- Nutrición_todo |> 
  group_by(Group_diet, Estudio) |> 
  summarise(
    across(
      where(is.numeric),
      list(
        promedio = ~mean(.x, na.rm = TRUE),
        desviacion = ~sd(.x, na.rm = TRUE),
        error_estandar = ~sd(.x, na.rm = TRUE) / sqrt(length(.x))),
      .names = "{.col}_{.fn}"),
    .groups = "drop")

# Pasar a nombres en ingles
Ingles <- c(
  "Cereales_g/día"       = "Cereals (g/day)",
  "Vegetales_g/día"      = "Vegetables (g/day)",
  "Fruta_g/día"          = "Fruit (g/day)",
  "Lacteos_g/día"        = "Dairy (g/day)",
  "Carnes_g/día"         = "Meat (g/day)",
  "Energía (Kcal/día)"   = "Energy (kcal/day)",
  "Proteinas (g)"        = "Protein (g/day)",
  "Grasa total (g)"      = "Total fat (g/day)",
  "Glucidos total (g)"   = "Total carbohydrates (g/day)",
  "Azucares simples (g)" = "Simple sugars (g/day)",
  "Fibra dietetica (g)"  = "Dietary fiber (g/day)",
  "Alcohol (g)"          = "Alcohol (g/day)",
  "Vitamina A (mcg)"     = "Vitamin A (µg/day)",
  "Vitamina D (mcg)"     = "Vitamin D (µg/day)",
  "Vitamina E (mg)"      = "Vitamin E (mg/day)",
  "Folico (mcg)"         = "Folate (µg/day)",
  "Vitamina B12 (mcg)"   = "Vitamin B12 (µg/day)",
  "Sodio (mg)"           = "Sodium (mg/day)",
  "Calcio (mg)"          = "Calcium (mg/day)",
  "Hierro (mg)"          = "Iron (mg/day)",
  "Zinc (mg)"            = "Zinc (mg/day)",
  "carnes_r"             = "Meat servings",
  "cereales_r"           = "Cereals servings",
  "frutas_r"             = "Fruit servings",
  "vegetales_r"          = "Vegetables servings",
  "lacteos_r"            = "Dairy servings",
  "IAS_100"              = "IAS (0–100)",
  "HEI_100"              = "HEI (0–100)",
  "DASH_rescalado_8_40"  = "DASH score (8–40)",
  "G_totales"            = "Total grams"
)

# Columnas a excluir
no_renombrar <- c("Estudio", "Group_diet")

tabla_resumen2 <- tabla_resumen |> 
  rename_with(
    ~ {
      # Solo renombrar si no está en la lista de excepciones
      ifelse(.x %in% no_renombrar, 
             .x,  # dejar igual
             {
               # Extraer la parte base antes del sufijo
               base <- str_replace(.x, "_(promedio|desviacion|error_estandar)$", "")
               # Extraer el sufijo si existe
               sufijo <- str_extract(.x, "_(promedio|desviacion|error_estandar)$")
               # Traducir la base si está en el diccionario
               base_en <- ifelse(base %in% names(Ingles), Ingles[base], base)
               # Reconstruir el nombre final
               paste0(base_en, sufijo)})})

write_xlsx(tabla_resumen2, "Tabla_resumen_todo.xlsx") 

## Graficar resultados

# Convertir de formato ancho a largo
tabla_larga <- tabla_resumen2 |>
  pivot_longer(
    cols = -c(Group_diet, Estudio),
    names_to = c("Nutrients", ".value"),
    names_pattern = "(.*)_(promedio|error_estandar)") |> 
  drop_na()

# Funcion para graficar cada nutriente
graficar_nutriente <- function(nutriente_sel) {
  df <- tabla_larga |> filter(Nutrients == nutriente_sel)
  
  ggplot(df, aes(x = Estudio, y = promedio, fill = Group_diet)) +
    geom_col(position = position_dodge(width = 0.9)) +
    geom_errorbar(
      aes(ymin = promedio - error_estandar, ymax = promedio + error_estandar),
      position = position_dodge(width = 0.9),
      width = 0.2) +
    labs(
      title = paste(nutriente_sel),
      x = "Study",
      y = "",
      fill = "Group diet") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 0, hjust = 0.5))}

# Lista de nutrientes disponibles
nutrientes <- unique(tabla_larga$Nutrients)

# Crear una lista de gráficos
for (n in nutrientes) {
  g <- graficar_nutriente(n)
  print(g)
  message("\nMostrando gráfico de ", n)
  readline("Presiona [Enter] para ver el siguiente...\n")}

### Crear tabla para word
# Crear columna con "promedio ± error"
tabla_larga2 <- tabla_larga |> 
  mutate(average_se = paste0(round(promedio, 2), " ± ", round(error_estandar, 2)))

# Pivotar la tabla para que los grupos de dieta sean columnas
tabla_word <- tabla_larga2 |> 
  select(Nutrients, Group_diet, average_se) |> 
  pivot_wider(
    names_from = Group_diet,
    values_from = average_se)

write_xlsx(tabla_word, "Tabla_resumen_para_word.xlsx") 

## Estadistica comparación IMDEA vs Hansel
# Estudio normalidad
library(nortest) # test de Lilliefors (K-S corregido)
# Seleccionamos los nombres de solo las columnas numéricas
cols_num <- Nutrición_todo |> 
  select(where(is.numeric)) |> 
  colnames()

# Función para variante de K-S
lillie_fun <- function(x) {
  x <- na.omit(x)
  if (length(x) < 3) return(NA)
  lillie.test(x)$p.value}

# Aplicar por grupo de estudio
normalidad_por_estudio <- Nutrición_todo |>
  group_by(Estudio) |>
  summarise(across(all_of(cols_num), lillie_fun)) |>
  pivot_longer(-Estudio, names_to = "variable", values_to = "p_value") |>
  mutate(normal = p_value > 0.05)

normalidad_por_estudio

normalidad_todas <- Nutrición_todo |> 
  summarise(across(all_of(cols_num), lillie_fun)) |> 
  pivot_longer(everything(), names_to = "variable", values_to = "p_value") |> 
  mutate(normal = p_value > 0.05)

normalidad_todas
# No son normales
# Comparación de medias (Wilcoxon)
## Test de Wilcoxon
# Seleccionar solo las columnas numéricas
datos_num <- Nutrición_todo |> 
  select(where(is.numeric))

# Pivotar a formato largo
datos_largos <- Nutrición_todo |> 
  pivot_longer(
    cols = all_of(names(datos_num)),
    names_to = "nutriente",
    values_to = "valor")

# Hacer test de Wilcoxon agrupando por group_diet, nutriente y visita
resultados_final <- datos_largos |> 
  group_by(nutriente) |>
  nest() |>
  mutate(
    p_valor = map_dbl(data, ~{
      x <- .x$valor[.x$Estudio == "IMDEA"]
      y <- .x$valor[.x$Estudio == "HansenLBS_2018"]
      
      if (length(x) > 0 && length(y) > 0) {
        wilcox.test(x, y, paired = FALSE)$p.value
      } else {
        NA_real_}})) |>
  mutate(significativo = ifelse(p_valor < 0.05, "Sí", "No")) |>
  select(nutriente, p_valor, significativo) |>
  arrange(nutriente)

print(resultados_final)
