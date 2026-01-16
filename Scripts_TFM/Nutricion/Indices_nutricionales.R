library(tidyverse)
library(readxl)
# install.packages("writexl")
library(writexl)
library(psych) # para hace confidencia

rm(list=ls()) # Clean global environment 

## DATOS DE NUTRIENTES
# Establecer directorio de trabajo
setwd("/home/vant/TFM/Nutricion/")
getwd()

## Creacion de indices
# Creacion de funcion para paso de gramos a raciones en el caso de que fuera necesario

Alimentos_g <- read_xlsx("Alimentos_totales.xlsx")
Nutrientes <- read_xlsx("Nutrientes_totales.xlsx")
colnames(Alimentos_g)
colnames(Nutrientes)

gramos_a_raciones <- function(df) {
  df$carnes_r <- df$"Carnes_g/día" / 100
  df$lacteos_r <- df$"Lacteos_g/día" / 200
  df$frutas_r <- df$"Fruta_g/día" / 150
  df$vegetales_r <- df$"Vegetales_g/día" / 100
  df$cereales_r <- df$"Cereales_g/día" / 50
  return(df)}

Alimentos_r <- gramos_a_raciones(Alimentos_g)

# Combinacion de ambas tablas

Unificado_tablas <- Alimentos_g |> 
  left_join(Nutrientes, by = c("ID", "Estudio", "Visita")) |> 
  left_join(Alimentos_r %>% select(ID, Estudio, Visita, carnes_r, cereales_r, frutas_r, vegetales_r, lacteos_r), by = c("ID", "Estudio", "Visita")) |> 
  drop_na()

## Indice de alimentación Sana adaptado
# este es el español, el que calcula el DIAL

IAS <- Unificado_tablas |> 
  mutate(
    Grasa_e = (`Grasa total (g)` * 9) / `Energía (Kcal/día)` * 100, 
    score_cereales = case_when(cereales_r >= 6 & cereales_r <= 10 ~ 10,
                               cereales_r > 0 & cereales_r < 6 ~ cereales_r / 6 * 10,
                               TRUE ~ 0),
    score_carnes = case_when(carnes_r >= 2 & carnes_r <= 3 ~ 10,
                             carnes_r > 0 & carnes_r < 2 ~ carnes_r / 2 * 10,
                             TRUE ~ 0),
    score_frutas = case_when(frutas_r >= 2 & frutas_r <= 4 ~ 10,
                             frutas_r > 0 & frutas_r < 2 ~ frutas_r / 2 * 10,
                             TRUE ~ 0),
    score_vegetales = case_when(vegetales_r >= 3 & vegetales_r <= 5 ~ 10,
                                vegetales_r > 0 & vegetales_r < 3 ~ vegetales_r / 3 * 10,
                                TRUE ~ 0),
    score_lacteos = case_when(lacteos_r >= 2 & lacteos_r <= 3 ~ 10,
                              lacteos_r > 0 & lacteos_r < 2 ~ lacteos_r / 2 * 10,
                             TRUE ~ 0),
    score_grasa_total = case_when(Grasa_e <= 30 ~ 10, Grasa_e >= 45 ~ 0, 
                                  Grasa_e > 30 & Grasa_e < 45 ~ pmax(0, 10 - (Grasa_e - 30) * (10 / 15))),
    score_sodio = case_when(`Sodio (mg)` <= 2400 ~ 10, `Sodio (mg)` >= 4800 ~ 0, 
                            `Sodio (mg)` > 2400 & `Sodio (mg)` < 4800 ~ pmax(0, 10 - (`Sodio (mg)` - 2400) * (10 / 2400))),
    IAS_simplificado = score_cereales + score_carnes + score_frutas + score_vegetales + score_lacteos + score_grasa_total + score_sodio,
    IAS_100 = IAS_simplificado * (100 / 70))

# Comparación con el IAS calculado por el DIAL 

Compacion_IAS <- IAS |> 
  filter(Estudio == "IMDEA") |> 
  select("ID", "IAS_simplificado", "IAS_100")

DIAL_nocluster <- read.csv("Originales/DietaALL_noCLus.csv")
DIAL_nocluster$ID <- as.character(DIAL_nocluster$ID)

Compacion_IAS <- Compacion_IAS |> 
  left_join(DIAL_nocluster  %>% select("ID", "IAS"), by = "ID")

# Estadistico para comprobar que no existen diferencias estadisticamente significativas entre ambas interpretaciones

shapiro.test(Compacion_IAS$IAS_100)
shapiro.test(Compacion_IAS$IAS)
# Los datos siguen una distribución normal

t.test(Compacion_IAS$IAS_100, Compacion_IAS$IAS, paired = TRUE)
boxplot(Compacion_IAS$IAS_100, Compacion_IAS$IAS, names = c("IAS_alternativo", "IAS_DIAL"), main = "Comparación de IAS")
# Comprobamos que no existen diferencias significativas entre el calculo de indices de alimentación saludable con mi metodo y el metodo del DIAL 
# Prueba de confidencia
x <- Compacion_IAS$IAS_100
y <- Compacion_IAS$IAS

mediciones_IAS <- data.frame(x, y)

# Calcular ICC
icc_result_IAS <- ICC(mediciones_IAS)
print(icc_result_IAS)
# Obtenemos una moderada a buena concordancia entre los dos métodos, 

## Healthy Eating Index adaptado
# Esta es una version adaptada por mí para los datos de nuestro estudio basado en la oficial de USDA, 2020
# Los numeros que aparecen para calcular los scores son las recomendaciones puestas por la USDA

HEI <- Unificado_tablas |> 
  mutate(
    # % kcal provenientes de azúcares y grasas
    Azucares_e = (`Azucares simples (g)` * 4) / `Energía (Kcal/día)` * 100,
    Grasa_e = (`Grasa total (g)` * 9) / `Energía (Kcal/día)` * 100,
    # Componentes de adecuación
    Score_FrutasTotales     = pmin(`Fruta_g/día` / 128 * 5, 5),     # Meta: 0.8 cup eq. ≈ 128g / 1000 kcal
    Score_VerdurasTotales   = pmin(`Vegetales_g/día` / 165 * 5, 5), # Meta: 1.1 cup eq. ≈ 165g / 1000 kcal
    Score_GranosIntegrales  = pmin(`Cereales_g/día` / 225 * 10, 10),# Supuesto: todo integral, meta ≈ 1.5 oz eq.
    Score_Lacteos           = pmin(`Lacteos_g/día` / 195 * 10, 10), # Meta: 1.3 cup eq. ≈ 195g / 1000 kcal
    Score_ProteinasTotales  = pmin(`Carnes_g/día` / 70 * 5, 5),     # Meta: ≈ 2.5 oz eq. ≈ 70g
    Score_Fibra             = pmin(`Fibra dietetica (g)` / 14 * 5, 5), # Meta: 14g / 1000 kcal
    # Componentes de moderación (inversamente proporcionales)
    Score_GrasaTotal = pmin(10, ifelse(Grasa_e <= 30, 10, pmax(0, 10 - (Grasa_e - 30)))),
    Score_AzucaresAgregados = pmin(10, ifelse(Azucares_e <= 10, 10, pmax(0, 10 - (Azucares_e - 10) * 2))),
    Score_Sodio = pmin(10, ifelse(`Sodio (mg)` <= 2300, 10, pmax(0, 10 - (`Sodio (mg)` - 2300) / 100))),
    # Suma de puntuaciones individuales (máximo teórico = 65)
    HEI_simplificado = Score_FrutasTotales + Score_VerdurasTotales + Score_GranosIntegrales +
      Score_Lacteos + Score_ProteinasTotales + Score_Fibra +
      Score_GrasaTotal + Score_AzucaresAgregados + Score_Sodio,
    # Escalado
    HEI_100 = pmin(100, HEI_simplificado * (100 / 65)))

# Comparación con HEI sin simplificar
# dietaryindex tiene una base de datos que incluye puntuaciones del HEI, perfecto para probar con el HEI simplificado 
#remotes::install_github("jamesjiadazhan/dietaryindex")
library(dietaryindex)
validacion_HEI <- HEI2020_VALIDATION
validacion_HEI <- validacion_HEI |> 
  mutate(HEI_simplificado = EXP_HEI2020_TOTALFRT + EXP_HEI2020_VEG + EXP_HEI2020_TOTALPRO + EXP_HEI2020_WHOLEGRAIN + EXP_HEI2020_DAIRY + EXP_HEI2020_FATTYACID + EXP_HEI2020_SODIUM + EXP_HEI2020_ADDEDSUGAR) |> 
  mutate(HEI_100 = pmin(100, HEI_simplificado * (100 / 65)))

# Estadistica
shapiro.test(validacion_HEI$HEI_100)
shapiro.test(validacion_HEI$EXP_HEI2020_ALL)

tt <- t.test(validacion_HEI$HEI_100, validacion_HEI$EXP_HEI2020_ALL, paired = TRUE)

# Graficar
HEI_df <- data.frame(
  HEI = c(validacion_HEI$HEI_100, validacion_HEI$EXP_HEI2020_ALL),
  Tipo = rep(c("Modified", "Official"), each = length(validacion_HEI$HEI_100)))

ggplot(HEI_df, aes(x = Tipo, y = HEI)) +
  geom_boxplot(width = 0.6) +
  geom_segment(aes(x = 1, xend = 2, y = max(HEI_df$HEI) + 1, yend = max(HEI_df$HEI) + 1), size = 1) +
  geom_segment(aes(x = 1, xend = 1, y = max(HEI_df$HEI) + 1, yend = max(HEI_df$HEI) + 0.5), size = 1) +
  geom_segment(aes(x = 2, xend = 2, y = max(HEI_df$HEI) + 1, yend = max(HEI_df$HEI) + 0.5), size = 1) +
  geom_text(aes(x = 1.5, y = max(HEI_df$HEI) + 1.5, label = "n.s."), size = 5) +
  labs(
    title = "Healthy Eating Index (HEI)",
    x = "",
    y = "Score") +
  theme_minimal(base_size = 14)

# se comportan de forma muy similar y no se observan diferencias estadisticas, pero la dispersion es mayor con el HEI_alternativo
# Prueba de confidencia
x <- validacion_HEI$HEI_100
y <- validacion_HEI$EXP_HEI2020_ALL

mediciones_HEI <- data.frame(x, y)

# Calcular ICC
icc_result_HEI <- ICC(mediciones_HEI)
print(icc_result_HEI)
# Esto indica que las mediciones de los dos métodos de mediciones_HEI son muy consistentes y confiables.

## DASH adaptado 
# Igual que lo anterior, adaptado por mí para el conjunto de datos a partir del DASH score original (Mellen et al., 2008) 
# Lo más importante, no incluye grasas saturasdas y asumimos que los cereales son integrales y las carnes magras
# Este indice se basa en cuartiles por consumo

DASH <- Unificado_tablas |>
  mutate(
    fruta_adj = `Fruta_g/día` / `Energía (Kcal/día)` * 1000,
    vegetal_adj = `Vegetales_g/día` / `Energía (Kcal/día)` * 1000,
    cereal_adj = `Cereales_g/día` / `Energía (Kcal/día)` * 1000,
    lacteo_adj = `Lacteos_g/día` / `Energía (Kcal/día)` * 1000,
    carne_adj = `Carnes_g/día` / `Energía (Kcal/día)` * 1000,
    azucar_adj = `Azucares simples (g)` / `Energía (Kcal/día)` * 1000,
    sodio_adj = `Sodio (mg)` / `Energía (Kcal/día)` * 1000,
    score_fruta = ntile(fruta_adj, 5),
    score_vegetal = ntile(vegetal_adj, 5),
    score_cereal = ntile(cereal_adj, 5),
    score_lacteo = ntile(lacteo_adj, 5),
    score_carne = ntile(carne_adj, 5),
    score_azucar = 6 - ntile(azucar_adj, 5),
    score_sodio = 6 - ntile(sodio_adj, 5),
    DASH_score = score_fruta + score_vegetal + score_cereal + score_lacteo + score_carne + score_azucar + score_sodio,
    DASH_rescalado_8_40 = (DASH_score - 7) / 28 * 32 + 8 # rescalado para que las puntuaciones sean comparables con el DASH original
  )

# Hacemos lo mismo que con el HEI
validacion_DASH <- DASH_VALIDATION
validacion_DASH <- validacion_DASH |> 
  mutate(DASH_simplificado = EXP_DASH_WHOLE_FRUIT + EXP_DASH_VEGETABLE + EXP_DASH_WHOLE_GRAIN + EXP_DASH_LOW_FAT_DAIRY + EXP_DASH_SODIUM + EXP_DASH_RED_PROCESSED_MEAT + EXP_DASH_SSB) |> 
  mutate(DASH_rescalado = DASH_simplificado * (40 / 35))

# Estadistica
shapiro.test(validacion_DASH$DASH_rescalado)
shapiro.test(validacion_DASH$EXP_DASH_ALL)

t.test(validacion_DASH$DASH_rescalado, validacion_DASH$EXP_DASH_ALL, paired = TRUE)

# Graficar
DASH_df <- data.frame(
  DASH = c(validacion_DASH$DASH_rescalado, validacion_DASH$EXP_DASH_ALL),
  Tipo = rep(c("Modified", "Official"), each = length(validacion_DASH$DASH_rescalado)))

ggplot(DASH_df, aes(x = Tipo, y = DASH)) +
  geom_boxplot(width = 0.6) +
  geom_segment(aes(x = 1, xend = 2, y = max(DASH_df$DASH) + 1, yend = max(DASH_df$DASH) + 1), size = 1) +
  geom_segment(aes(x = 1, xend = 1, y = max(DASH_df$DASH) + 1, yend = max(DASH_df$DASH) + 0.5), size = 1) +
  geom_segment(aes(x = 2, xend = 2, y = max(DASH_df$DASH) + 1, yend = max(DASH_df$DASH) + 0.5), size = 1) +
  geom_text(aes(x = 1.5, y = max(DASH_df$DASH) + 1.5, label = "n.s."), size = 5) +
  labs(
    title = "Dietary Approaches to Stop Hypertension (DASH)",
    x = "",
    y = "Score") +
  theme_minimal(base_size = 14)

# sin diferencias significativas entre indices
# Prueba de confidencia
x <- validacion_DASH$DASH_rescalado
y <- validacion_DASH$EXP_DASH_ALL

mediciones_DASH <- data.frame(x, y)

# Calcular ICC
icc_result_DASH <- ICC(mediciones_DASH)
print(icc_result_DASH)
# Esto indica que las mediciones de los dos métodos de medir DASH son muy consistentes y confiables.

## Unificar indices, nutrientes y alimentos en una sola tabla

Unificado_tablas_indices <- Unificado_tablas |> 
  left_join(IAS %>% select(IAS_100, ID, Estudio, Visita), by = c("ID", "Estudio", "Visita")) |> 
  left_join(HEI %>% select(HEI_100, ID, Estudio, Visita), by = c("ID", "Estudio", "Visita")) |>
  left_join(DASH %>% select(DASH_rescalado_8_40, ID, Estudio, Visita), by = c("ID", "Estudio", "Visita")) |> 
  relocate(c(Estudio, Visita), .after = ID) |> 
  drop_na()

Unificado_tablas_indices2 <- Unificado_tablas_indices |> 
  mutate(G_totales = `Cereales_g/día` + `Vegetales_g/día` +`Lacteos_g/día` + `Carnes_g/día` + `Fruta_g/día`)

## Corregir e incluir grupos
Nutrición_todo <- Unificado_tablas_indices2 |> 
  filter(Estudio != "Heitz-BuschartA_2016")

# Filtrar las muestras eliminadas mediante boxplot
Nutrición_todo <- Nutrición_todo |> 
  filter(!ID %in% c("1797", "1827", "1838", "1824"))

outliers_eliminar <- data.frame(
  ID = c("5415", "5438", "5454"),
  Visita = c("1", "2", "4"))

Nutrición_todo <- Nutrición_todo |> 
  anti_join(outliers_eliminar, by = c("ID", "Visita"))

# Creación de grupos para Hansen de acuerdo a si es low o high gluten
Grupos_Hansen <- read_xlsx("../Estudios_escogidos/HansenLBS_2018/Dietary_data.xlsx",  sheet = "Dietary data")
Grupos_Hansen_2 <- Grupos_Hansen |> 
  mutate(Group_diet = case_when(
    visit_number == 1            ~ "Regular diet",
    Beh == "Fend"             ~ "Low gluten",
    Beh == "Hend"             ~ "High gluten",
    TRUE                    ~ NA_character_))

# Combinar con el original y hacer que para los datos del IMDEA no ponga NA
Grupos_Hansen_2 <- Grupos_Hansen_2 |> 
  rename(Visita = visit_number) |> 
  mutate(ID = as.character(ID),
         Visita = as.character(Visita))

Nutrición_todo <- Nutrición_todo |> 
  mutate(ID = as.character(ID))

Nutrición_todo_2 <- Nutrición_todo |> 
  left_join(
    Grupos_Hansen_2 %>% select(ID, Group_diet, Visita),
    by = c("ID", "Visita"))

Nutrición_todo_2 <- Nutrición_todo_2 |> 
  mutate(
    Group_diet = ifelse(
      is.na(Group_diet),
      "Celiac",
      Group_diet))

write_xlsx(Nutrición_todo_2, "Unificado_nutrición_indices.xlsx") 
