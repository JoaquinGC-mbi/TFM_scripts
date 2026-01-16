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
library(car) # Test de Levenne

# Carga de archivos de abundancia de alimentos y nutrientes
rm(list=ls()) # Clean global environment 

setwd("/home/vant/TFM/medi/")
getwd()

Datos_abundancia <- read_csv("result_prueba_3_(buena)/food_abundance.csv")
Datos_DIAL <- read_xlsx("../Nutricion/Alimentos_totales.xlsx")

### ESTUDIO DE GRUPOS DE ALIMENTOS
# Exploración de datos
colnames(Datos_abundancia)

Total_grupos_alimentos <- Datos_abundancia |> 
  group_by(sample_id, food_group) |> 
  summarise(Abundancia_total = sum(relative_abundance), .groups = "drop") |> 
  pivot_wider(names_from = food_group, values_from = Abundancia_total, values_fill = 0)

# Para combinar los valores de A y B de las muestras
Total_grupos_alimentos_2 <- Total_grupos_alimentos |>
  mutate(sample_id = str_remove(sample_id, "^D"), ID_base = str_remove(sample_id, "[a-z]$")) |>  
  group_by(ID_base) |>
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) |>  # Promedia todas las columnas numéricas
  ungroup() |>
  rename(ID = ID_base)

# Crear nueva columna que sea suma de abundancias de todos los grupos de alimentos por voluntario
# IMPORTANTE: MEDI no detecta leche y productos variados ya que, aunque detecte ADN de vacuno, no puede diferenciar su procedencia y en la leche encontramos menos ADN que en la carne
Total_grupos_alimentos_3 <- Total_grupos_alimentos_2 |> 
  mutate(Abundancia_total = rowSums(across(where(is.numeric))), 
         "Vegetales_%" = Vegetables + Gourds + `Herbs and Spices` + Teas, 
         "Cereales_%" = `Cereals and cereal products` + Pulses + Soy + Nuts, 
         "Carnes_%" = `Animal foods` + `Aquatic foods`,
         "Frutas_%" = Fruits + `Cocoa and cocoa products`) |> 
  select(ID, Abundancia_total, "Vegetales_%", "Frutas_%", "Carnes_%", "Cereales_%")

# Ahora creamos porcentajes de consumo a partir de la abundancia total 
Total_grupos_alimentos_porc <- Total_grupos_alimentos_3 |> 
  mutate(across(-c(ID, Abundancia_total), ~ .x / Abundancia_total * 100))

## Hacer lo mismo con los datos de los cuestionarios
Datos_DIAL_porc <- Datos_DIAL |> 
  mutate(G_totales = `Cereales_g/día` + `Vegetales_g/día` +`Lacteos_g/día` + `Carnes_g/día` + `Fruta_g/día`) |> 
  mutate(across(-c(ID, G_totales, Visita, Estudio), ~ .x / G_totales * 100)) |> 
  filter(Estudio == "IMDEA")

Datos_DIAL_porc <- Datos_DIAL_porc |> 
  rename("Vegetales_%" = `Vegetales_g/día`,
         "Cereales_%" = `Cereales_g/día`,
         "Carnes_%" = `Carnes_g/día`,
         "Frutas_%" = `Fruta_g/día`,
         "Lacteos_%" = `Lacteos_g/día`)

# Unificar tablas
Total_grupos_alimentos_porc$Fuente <- "MEDI"
Total_grupos_alimentos_porc$Estudio <- "IMDEA"
Total_grupos_alimentos_porc$Visita <- "1"
Datos_DIAL_porc$Fuente <- "Cuestionario"

Tablas_unificadas <- bind_rows(Total_grupos_alimentos_porc, Datos_DIAL_porc) 
Tablas_unificadas <- Tablas_unificadas |> 
  select(where(~ !any(is.na(.))))

library(openxlsx)
write.xlsx(Tablas_unificadas, "../Nutricion/Alimentos_DIAL_MEDI_sinboxplot.xlsx") 

## BOXPLOT
# Hay datos bastante extremos, por lo que sería util llevar a cabo un boxplot para eliminar los outliers

# Pasar la tabla a formato largo
Tablas_long <- Tablas_unificadas |> 
  pivot_longer(cols = c("Vegetales_%", "Frutas_%", "Carnes_%", "Cereales_%"),
               names_to = "Grupo_alimento",
               values_to = "Porcentaje")

# Detectar outliers por Grupo_alimento y Fuente
outliers <- Tablas_long |> 
  group_by(Fuente, Grupo_alimento) |> 
  mutate(Q1 = quantile(Porcentaje, 0.25),
         Q3 = quantile(Porcentaje, 0.75),
         IQR = Q3 - Q1,
         Lower = Q1 - 1.5 * IQR,
         Upper = Q3 + 1.5 * IQR,
         Es_outlier = Porcentaje < Lower | Porcentaje > Upper) |> 
  filter(Es_outlier) |> 
  ungroup()

# Boxplot por Grupo_alimento y separado por Fuente
ggplot(Tablas_long, aes(x = Grupo_alimento, y = Porcentaje)) +
  geom_boxplot(outlier.shape = NA, fill = "lightblue") +  # ocultamos los outliers por defecto
  geom_text(data = outliers, aes(label = ID), 
            position = position_jitter(width = 0.2), size = 3, color = "red") +
  labs(title = "Boxplot con IDs de outliers por Fuente",
       x = "Grupo de alimento",
       y = "Porcentaje de abundancia") +
  facet_wrap(~Fuente) +
  theme_minimal()

# La decisión de eliminar estos numeros es en consenso con el boxplot de nutrientes
Tablas_unificadas <- Tablas_unificadas |> 
  filter(!ID %in% c(1797, 1827, 1838, 1824))

## NORMALIDAD
# Seleccionamos los nombres de solo las columnas numéricas
cols_num <- Tablas_unificadas |> 
  select(where(is.numeric)) |> 
  colnames()

# Función para Shapiro-Wilk
shapiro_fun <- function(x) {
  x <- na.omit(x)
  if(length(x) < 3) return(NA)
  shapiro.test(x)$p.value}

# Aplicar por grupo de fuente
normalidad_por_fuente <- Tablas_unificadas |>
  group_by(Fuente) |>
  summarise(across(all_of(cols_num), shapiro_fun)) |>
  pivot_longer(-Fuente, names_to = "variable", values_to = "p_value") |>
  mutate(normal = p_value > 0.05)

normalidad_por_fuente

normalidad_todas <- Tablas_unificadas |> 
  summarise(across(all_of(cols_num), shapiro_fun)) |> 
  pivot_longer(everything(), names_to = "variable", values_to = "p_value") |> 
  mutate(normal = p_value > 0.05)

normalidad_todas

# No siguen una distribución normal al ser porcentajes e ir de un rango del 0 al 100

levene_resultados <- Tablas_unificadas |>
  pivot_longer(
    cols = all_of(cols_num),
    names_to = "Nutriente",
    values_to = "Valor") |>
  group_by(Nutriente) |>
  summarise(
    p_value = leveneTest(Valor ~ Fuente)$`Pr(>F)`[1],
    .groups = "drop") |>
  mutate(
    homocedastico = p_value > 0.05)

levene_resultados

## Test de Wilcoxon
# Seleccionar solo los datos numericos
datos_num <- Tablas_unificadas |> 
  select(where(is.numeric))

resultados <- list()

for (col in names(datos_num)) {
  x <- datos_num[[col]][Tablas_unificadas$Fuente == "Cuestionario"]
  y <- datos_num[[col]][Tablas_unificadas$Fuente == "MEDI"]
  
  # Test de Wilcoxon pareado
  test <- wilcox.test(x, y, paired = TRUE)
  
  resultados[[col]] <- test$p.value}

# Convertir a data.frame y añadir columna de significancia
resultados_df <- data.frame(
  columna = names(resultados),
  p_valor = unlist(resultados))

resultados_df$significativo <- ifelse(resultados_df$p_valor < 0.05, "Sí", "No")

print(resultados_df)  

# Solo se observan diferencias significativas en el porcentaje de vegetales
# Graficar

Tablas_medias <- Tablas_unificadas |> 
  pivot_longer(cols = c("Vegetales_%", "Frutas_%", "Carnes_%", "Cereales_%"),
               names_to = "Grupo_alimento",
               values_to = "Porcentaje")

medias <- Tablas_medias |> 
  group_by(Fuente, Grupo_alimento) |> 
  summarise(
    Media = mean(Porcentaje),
    SD = sd(Porcentaje),
    n = n(),
    SE = SD / sqrt(n))

# Gráfico con barras y desviación estándar
g1 <- ggplot(medias, aes(x = Grupo_alimento, y = Media, fill = Fuente)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Media - SE, ymax = Media + SE),
                width = 0.2,
                position = position_dodge(0.9)) +
  labs(y = "", x = "",
       title = "Study group food intake",
       fill = "Source") +
  scale_fill_manual(
    values = c("Cuestionario" = "#F8766D", "MEDI" = "#00BFC4"),
    labels = c(
      "Cuestionario" = "Questionnaire",
      "MEDI" = "MEDI")) +
  scale_x_discrete(labels = c(
    "Carnes_%" = "Meat (%)",
    "Cereales_%" = "Cereals (%)",
    "Frutas_%" = "Fruits (%)",
    "Vegetales_%" = "Vegetables (%)")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 15))


print(g1)

## Correlación
# Automatizar el modelo para todos los nutrientes

# Organizar datos
cols_no_pivotar <- c("ID", "Estudio", "Visita", "Fuente")
Datos_pivotados <- Tablas_unificadas |> 
  pivot_wider(names_from = Fuente, values_from = setdiff(names(Tablas_unificadas), cols_no_pivotar))

# Dataframe para resultados
resultados_corr <- data.frame(
  Nutriente = character(),
  Pendiente = numeric(),
  R2 = numeric(),
  R2_adj = numeric(),
  Spearman_r = numeric(),
  p_value = numeric(),
  p_bonferroni = numeric(),
  p_fdr = numeric(),
  stringsAsFactors = FALSE)

# Bucle para todos los nutrientes
for (nut in cols_num) {
  
  # Extraer columnas MEDI y Cuestionario para este nutriente
  x <- Datos_pivotados[[paste0(nut, "_MEDI")]]
  y <- Datos_pivotados[[paste0(nut, "_Cuestionario")]]
  
  # Correlación de Spearman
  spearman_test <- cor.test(x, y, method = "spearman", exact = FALSE)
  
  # Guardar resultados en dataframe
  resultados_corr <- rbind(resultados_corr, data.frame(
    Nutriente = nut,
    Spearman_rho = spearman_test$estimate,
    p_value = spearman_test$p.value))}

# Ajuste de p-valores por múltiples comparaciones
resultados_corr$p_bonferroni <- p.adjust(resultados_corr$p_value, method = "bonferroni")
resultados_corr$p_fdr <- p.adjust(resultados_corr$p_value, method = "fdr")

# Ver resultados finales
resultados_corr
write.xlsx(resultados_corr, "Tablas_resultados/Corr_foods_celiacos.xlsx")

# No sale nada significativo (como era de esperar), pero en vegetales se acerca a significación y tiene la mejor r de spearman 

# Guardar medias y SE para tablas
medias_formateada <- medias |> 
  mutate(
    media_se = sprintf("%.2f ± %.2f", Media, SE))

tabla_final <- medias_formateada |> 
  select(Grupo_alimento, Fuente, media_se) |> 
  pivot_wider(
    names_from = Fuente,
    values_from = media_se) |> 
  arrange(Grupo_alimento)

write.xlsx(tabla_final, "Tablas_resultados/Tabla_medias_celiacos_alimentos.xlsx")
