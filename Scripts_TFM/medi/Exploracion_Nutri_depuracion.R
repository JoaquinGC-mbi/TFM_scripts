### DEPURACION DATOS MEDI

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

setwd("/home/vant/TFM/")
getwd()

Datos_nutrientes <- read_csv("medi/result_prueba_3_(buena)/food_content.csv")

# Exploración de datos
colnames(Datos_nutrientes)

# Datos nutricionales
# Nuestro principal interes son los datos de macronutrientes y varios micronutrientes, pero no de otros compuestos como polifenoles 
writeLines(sort(unique(Datos_nutrientes$name)), "medi/lista_compuestos.txt")

Nutrientes_final <- Datos_nutrientes |> 
  filter(unit %in% c('mg/100g', 'kcal/100g')) |> 
  filter(name %in% c("Energy", "Fat", "Carbohydrate", "Fiber (dietary)", "Proteins", "Sugars", "Ethanol", "Retinol", "alpha-Carotene", "beta-Carotene",
                     "beta-Cryptoxanthin", "Vitamin D", "Vitamin D3", "Ergocalciferol", "25-Hydroxycholecalciferol", "24,25-Dihydroxyvitamin D3", "alpha-Tocopherol",
                     "gamma-Tocopherol", "d-Tocopherol", "Folic acid", "Cobalamin", "Cyanocobalamin", "Sodium", "Calcium", "Iron", "Zinc")) |> 
  select("sample_id", "name", "unit", "abundance", "abundance_sd")

# Combinar nombres y unidades
Nutrientes_final2 <- Nutrientes_final |> 
  mutate(Nutrient = paste(name, unit, sep = "_")) |> 
  relocate(Nutrient, .after = sample_id) |> 
  select(-name, -unit, -abundance_sd) |> 
  pivot_wider(names_from = Nutrient, values_from = abundance)

colnames(Nutrientes_final2)

# Promediar los resultados de las muestras que son A y B
Nutrientes_final_agrupados <- Nutrientes_final2 |>
  mutate(sample_id = str_remove(sample_id, "^D"), ID_base = str_remove(sample_id, "[a-z]$")) |>  
  group_by(ID_base) |>
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) |>  # Promedia todas las columnas numéricas
  ungroup() |>
  rename(sample_id = ID_base)

# Crear nuevas columnas para la suma de los diferentes compuestos que componen las vitaminas
Nutrientes_final3 <- Nutrientes_final_agrupados |> 
  mutate(
    `Vitamina A (mg/100g)` = rowSums(across(c(`Retinol_mg/100g`, `alpha-Carotene_mg/100g`, `beta-Carotene_mg/100g`, `beta-Cryptoxanthin_mg/100g`)), na.rm = TRUE),
    `Vitamina D (mg/100g)` = rowSums(across(c(`Vitamin D_mg/100g`, `Vitamin D3_mg/100g`, `Ergocalciferol_mg/100g`, `25-Hydroxycholecalciferol_mg/100g`, `24,25-Dihydroxyvitamin D3_mg/100g`)), na.rm = TRUE),
    `Vitamina E (mg/100g)` = rowSums(across(c(`alpha-Tocopherol_mg/100g`, `gamma-Tocopherol_mg/100g`, `d-Tocopherol_mg/100g`)), na.rm = TRUE),
    `Vitamina B12 (mg/100g)` = rowSums(across(c(`Cobalamin_mg/100g`, `Cyanocobalamin_mg/100g`)), na.rm = TRUE)
  ) |>
  select("sample_id", "Fat_mg/100g", "Proteins_mg/100g", "Carbohydrate_mg/100g", "Fiber (dietary)_mg/100g",
         "Energy_kcal/100g", "Sugars_mg/100g", "Calcium_mg/100g", "Iron_mg/100g", "Sodium_mg/100g", "Zinc_mg/100g",
         "Vitamina A (mg/100g)", "Vitamina D (mg/100g)", "Vitamina E (mg/100g)", "Vitamina B12 (mg/100g)", "Folic acid_mg/100g", "Ethanol_mg/100g"
  ) |>
  rename(ID = sample_id, `Grasa total (mg/100g)`  = `Fat_mg/100g`, `Proteinas (mg/100g)` = `Proteins_mg/100g`, `Glucidos total (mg/100g)` = `Carbohydrate_mg/100g`, 
         `Fibra dietetica (mg/100g)` = `Fiber (dietary)_mg/100g`, `Energía (Kcal/100g)` = `Energy_kcal/100g`, `Azucares simples (mg/100g)` = `Sugars_mg/100g`, `Calcio (mg/100g)` = `Calcium_mg/100g`,
         `Hierro (mg/100g)` = `Iron_mg/100g`, `Sodio (mg/100g)` = `Sodium_mg/100g`, `Zinc (mg/100g)` = `Zinc_mg/100g`, `Folico (mg/100g)` = `Folic acid_mg/100g`, `Alcohol (mg/100g)` = `Ethanol_mg/100g`)
  
  
write_xlsx(Nutrientes_final3, "medi/Nutrientes_MEDI_celiacos.xlsx")

## Conversion de datos del DIAL a formato de datos de MEDI
# Es más correcto convertir los datos del DIAL al formato de MEDI, ya que MEDI da una abundancia relativa y calcular a partir de esos datos g/día es bastante subjetivo
# Empleando el computo total de g de alimento que la persona ha comido, se puede asignar nutrientes/100 g de alimento como tiene el DIAL

DIAL <- read_xlsx("Nutricion/Unificado_nutrición_indices.xlsx")

# Creación de nueva tabla con los valores en nutrientes / 100 g de alimentos
DIAL_g_alimento <- DIAL |> 
  select(-`Cereales_g/día`, -`Vegetales_g/día`, -`Fruta_g/día`, -`Lacteos_g/día`, -`Carnes_g/día`, -carnes_r, -cereales_r, -frutas_r, -vegetales_r, -lacteos_r, -IAS_100, -HEI_100, -DASH_rescalado_8_40) |> 
  mutate(across(-c(ID, Visita, G_totales, Estudio, Group_diet), ~ .x / G_totales * 100)) |> 
  rename(`Energía (Kcal/100g)` = `Energía (Kcal/día)`) |> 
  select(-G_totales) 

# Hay que tener cuidado con las unidades, en DIAL no todo esta en mg/100g
DIAL_mg_alimento <- DIAL_g_alimento |> 
  mutate(across(contains("(g)"), ~ . * 1000)) |> 
  mutate(across(contains("(mcg)"), ~ . / 1000)) |> 
  rename_with(~ str_replace(., "\\(mg\\)", "(mg/100g)"), contains("(mg)")) |> 
  rename_with(~ str_replace(., "\\(g\\)", "(mg/100g)"), contains("(g)")) |> 
  rename_with(~ str_replace(., "\\(mcg\\)", "(mg/100g)"), contains("(mcg)"))

write_xlsx(DIAL_mg_alimento, "Nutricion/Cuestionarios_nutrientes_mg_100g.xlsx")

### Repetir para los controles

## JUNTAR LOS DIFERENTES EXCELS DE RESULTADOS
# Alimentos

df2 <- read_csv("medi/result_Hansen_2018_V1/food_abundance.csv")
df3 <- read_csv("medi/result_Hansen_2018_V2/food_abundance.csv")
df4 <- read_csv("medi/result_Hansen_2018_V4/food_abundance.csv")

Alimentos_completo <- bind_rows(df2, df3, df4)
# Nutrientes

df2N <- read_csv("medi/result_Hansen_2018_V1/food_content.csv")
df3N <- read_csv("medi/result_Hansen_2018_V2/food_content.csv")
df4N <- read_csv("medi/result_Hansen_2018_V4/food_content.csv")

Nutrientes_completo <- bind_rows(df2N, df3N, df4N)

write_xlsx(Alimentos_completo, "medi/Alimentos_controles_MEDI_unificado.xlsx")
write_xlsx(Nutrientes_completo, "medi/Nutrientes_controles_MEDI_unificado.xlsx")

Datos_nutrientes <- read_xlsx("medi/Nutrientes_controles_MEDI_unificado.xlsx")

# Exploración de datos
colnames(Datos_nutrientes)

# Datos nutricionales
# Nuestro principal interes son los datos de macronutrientes y varios micronutrientes, pero no de otros compuestos como polifenoles 
writeLines(sort(unique(Datos_nutrientes$name)), "medi/lista_compuestos.txt")

Nutrientes_final <- Datos_nutrientes |> 
  filter(unit %in% c('mg/100g', 'kcal/100g')) |> 
  filter(name %in% c("Energy", "Fat", "Carbohydrate", "Fiber (dietary)", "Proteins", "Sugars", "Ethanol", "Retinol", "alpha-Carotene", "beta-Carotene",
                     "beta-Cryptoxanthin", "Vitamin D", "Vitamin D3", "Ergocalciferol", "25-Hydroxycholecalciferol", "24,25-Dihydroxyvitamin D3", "alpha-Tocopherol",
                     "gamma-Tocopherol", "d-Tocopherol", "Folic acid", "Cobalamin", "Cyanocobalamin", "Sodium", "Calcium", "Iron", "Zinc")) |> 
  select("sample_id", "name", "unit", "abundance", "abundance_sd")

# Combinar nombres y unidades
Nutrientes_final2 <- Nutrientes_final |> 
  mutate(Nutrient = paste(name, unit, sep = "_")) |> 
  relocate(Nutrient, .after = sample_id) |> 
  select(-name, -unit, -abundance_sd) |> 
  pivot_wider(names_from = Nutrient, values_from = abundance)

colnames(Nutrientes_final2)

# Crear nuevas columnas para la suma de los diferentes compuestos que componen las vitaminas
Nutrientes_final3 <- Nutrientes_final2 |> 
  mutate(
    `Vitamina A (mg/100g)` = rowSums(across(c(`Retinol_mg/100g`, `alpha-Carotene_mg/100g`, `beta-Carotene_mg/100g`, `beta-Cryptoxanthin_mg/100g`)), na.rm = TRUE),
    `Vitamina D (mg/100g)` = rowSums(across(c(`Vitamin D_mg/100g`, `Vitamin D3_mg/100g`, `Ergocalciferol_mg/100g`, `25-Hydroxycholecalciferol_mg/100g`, `24,25-Dihydroxyvitamin D3_mg/100g`)), na.rm = TRUE),
    `Vitamina E (mg/100g)` = rowSums(across(c(`alpha-Tocopherol_mg/100g`, `gamma-Tocopherol_mg/100g`, `d-Tocopherol_mg/100g`)), na.rm = TRUE),
    `Vitamina B12 (mg/100g)` = rowSums(across(c(`Cobalamin_mg/100g`, `Cyanocobalamin_mg/100g`)), na.rm = TRUE)
  ) |>
  select("sample_id", "Fat_mg/100g", "Proteins_mg/100g", "Carbohydrate_mg/100g", "Fiber (dietary)_mg/100g",
    "Energy_kcal/100g", "Sugars_mg/100g", "Calcium_mg/100g", "Iron_mg/100g", "Sodium_mg/100g", "Zinc_mg/100g",
    "Vitamina A (mg/100g)", "Vitamina D (mg/100g)", "Vitamina E (mg/100g)", "Vitamina B12 (mg/100g)", "Folic acid_mg/100g", "Ethanol_mg/100g"
  ) |>
  rename(ID = sample_id, `Grasa total (mg/100g)`  = `Fat_mg/100g`, `Proteinas (mg/100g)` = `Proteins_mg/100g`, `Glucidos total (mg/100g)` = `Carbohydrate_mg/100g`, 
         `Fibra dietetica (mg/100g)` = `Fiber (dietary)_mg/100g`, `Energía (Kcal/100g)` = `Energy_kcal/100g`, `Azucares simples (mg/100g)` = `Sugars_mg/100g`, `Calcio (mg/100g)` = `Calcium_mg/100g`,
         `Hierro (mg/100g)` = `Iron_mg/100g`, `Sodio (mg/100g)` = `Sodium_mg/100g`, `Zinc (mg/100g)` = `Zinc_mg/100g`, `Folico (mg/100g)` = `Folic acid_mg/100g`, `Alcohol (mg/100g)` = `Ethanol_mg/100g`)

Nutrientes_final3$Fuente <- "MEDI"
Nutrientes_final3$Estudio <- "HansenLBS_2018"

# Para sacar la visita del numero del estudio de Hansen
Nutrientes_final4 <- Nutrientes_final3 |> 
  mutate(
    Visita = if_else(
      str_starts(Estudio, "HansenLBS_2018"),
      str_extract(ID, "(?<=-).*"),  
      NA_character_
    ),
    ID = if_else(
      str_starts(Estudio, "HansenLBS_2018"),
      str_extract(ID, "^[^-]+"),    # lo que está antes del "-"
      ID                           
    )
  )

# Limpiar nombres de Buscharta (no quitar los %>%, son necesarios para que funcione el codigo)
Nutrientes_final4 <- Nutrientes_final4 %>%
  mutate(
    ID = if_else(
      Estudio == "Heitz-BuschartA_2016",
      # Solo modifica los IDs de este estudio
      ID %>%
        str_remove("-V[0-9]+(-[A-Za-z]+)?$") %>%      # quita "-V1-stool" o "-V1"
        str_extract("M[0-9]+-[0-9]+") %>%              # extrae "M01-1"
        str_match("M([0-9]+)-([0-9]+)") %>%
        (\(.) sprintf("M-%02d-%02d", as.integer(.[,2]), as.integer(.[,3])))(),
      # En caso contrario, deja el ID original
      ID
    )
  )

write_xlsx(Nutrientes_final4, "medi/Nutrientes_MEDI_controles_final.xlsx")
