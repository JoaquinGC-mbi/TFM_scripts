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
library(nortest) # test de Lilliefors (K-S corregido)
library(car) # Test de Levenne

# Carga de archivos de abundancia de alimentos y nutrientes
rm(list=ls()) # Clean global environment 

setwd("/home/vant/TFM/medi/")
getwd()

Datos_abundancia <- read_xlsx("Alimentos_controles_MEDI_unificado.xlsx")
Datos_DIAL <- read_xlsx("../Nutricion/Unificado_nutrición_indices.xlsx")

Datos_DIAL <- Datos_DIAL |> 
  select(ID, Estudio, Visita, `Cereales_g/día`, `Vegetales_g/día`, `Fruta_g/día`, `Lacteos_g/día`, `Carnes_g/día`, G_totales, Group_diet)

### ESTUDIO DE GRUPOS DE ALIMENTOS
# Exploración de datos
colnames(Datos_abundancia)

Total_grupos_alimentos <- Datos_abundancia |> 
  group_by(sample_id, food_group) |> 
  summarise(Abundancia_total = sum(relative_abundance), .groups = "drop") |> 
  pivot_wider(names_from = food_group, values_from = Abundancia_total, values_fill = 0)

# Crear nueva columna que sea suma de abundancias de todos los grupos de alimentos por voluntario
# IMPORTANTE: MEDI no detecta leche y productos variados ya que, aunque detecte ADN de vacuno, no puede diferenciar su procedencia y en la leche encontramos menos ADN que en la carne
Total_grupos_alimentos_3 <- Total_grupos_alimentos |> 
  mutate(Abundancia_total = rowSums(across(where(is.numeric))), 
         "Vegetales_%" = Vegetables + Gourds + `Herbs and Spices` + Teas, 
         "Cereales_%" = `Cereals and cereal products` + Pulses + Soy + Nuts, 
         "Carnes_%" = `Animal foods` + `Aquatic foods`,
         "Frutas_%" = Fruits + `Cocoa and cocoa products`) |> 
  select(sample_id, Abundancia_total, "Vegetales_%", "Frutas_%", "Carnes_%", "Cereales_%") |> 
  rename(ID = sample_id)

# Crear columna de visita y estudio en funcion del ID
# Poner el estudio al que corresponden
Total_grupos_alimentos_3 <- Total_grupos_alimentos_3 |> 
  mutate(
    Estudio = if_else(
      str_starts(ID, "M"),
      "Heitz-BuschartA_2016",
      "HansenLBS_2018"))

# Para sacar la visita del numero del estudio de Hansen
Total_grupos_alimentos_3 <- Total_grupos_alimentos_3 |> 
  mutate(
    Visita = if_else(
      str_starts(Estudio, "HansenLBS_2018"),
      str_extract(ID, "(?<=-).*"),  
      NA_character_),
    ID = if_else(
      str_starts(Estudio, "HansenLBS_2018"),
      str_extract(ID, "^[^-]+"),   
      ID))

# Ahora creamos porcentajes de consumo a partir de la abundancia total 
Total_grupos_alimentos_porc <- Total_grupos_alimentos_3 |> 
  mutate(across(-c(ID, Abundancia_total, Estudio, Visita), ~ .x / Abundancia_total * 100))

## Hacer lo mismo con los datos de los cuestionarios
Datos_DIAL_porc <- Datos_DIAL |>  
  mutate(across(-c(ID, G_totales, Visita, Estudio, Group_diet), ~ .x / G_totales * 100)) |> 
  filter(Estudio != "IMDEA")

Datos_DIAL_porc <- Datos_DIAL_porc |> 
  rename("Vegetales_%" = `Vegetales_g/día`,
         "Cereales_%" = `Cereales_g/día`,
         "Carnes_%" = `Carnes_g/día`,
         "Frutas_%" = `Fruta_g/día`,
         "Lacteos_%" = `Lacteos_g/día`)

# Unificar tablas
Total_grupos_alimentos_porc$Fuente <- "MEDI"
Datos_DIAL_porc$Fuente <- "Cuestionario"

# Introducir la columna de grupos de dieta en alimentos de MEDI
Total_grupos_alimentos_porc <- Total_grupos_alimentos_porc |> 
  left_join(Datos_DIAL_porc %>% select(ID, Group_diet, Visita), by = c("ID", "Visita"))

Tablas_unificadas <- bind_rows(Total_grupos_alimentos_porc, Datos_DIAL_porc) 
Tablas_unificadas <- Tablas_unificadas |> 
  select(-Abundancia_total, -`Lacteos_%`, -G_totales) |> 
  filter(!if_any(everything(), is.na))

# Retirar voluntarios que para la misma visita, no cuenten con cuestionario y datos de MEDI
Tablas_unificadas <- Tablas_unificadas |> 
  group_by(ID, Visita) |> 
  # contar cuántos tipos de estudio distintos tiene por visita
  filter(n_distinct(Fuente) == 2) |> 
  ungroup()
  
library(openxlsx)
write.xlsx(Tablas_unificadas, "../Nutricion/Alimentos_DIAL_MEDI_controles.xlsx") 

## BOXPLOT
# Hay datos bastante extremos, por lo que sería util llevar a cabo un boxplot para eliminar los outliers

# Pasar la tabla a formato largo
Tablas_long <- Tablas_unificadas |> 
  pivot_longer(cols = c("Vegetales_%", "Frutas_%", "Carnes_%", "Cereales_%"),
               names_to = "Grupo_alimento",
               values_to = "Porcentaje")

# Detectar outliers por Grupo_alimento y Fuente
outliers2 <- Tablas_long |> 
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
  geom_boxplot(outlier.shape = NA, fill = "lightblue") +  
  geom_text(data = outliers2, aes(label = ID), 
            position = position_jitter(width = 0.2), size = 3, color = "red") +
  labs(title = "Boxplot con IDs de outliers por Fuente",
       x = "Grupo de alimento",
       y = "Porcentaje de abundancia") +
  facet_wrap(~Fuente) +
  theme_minimal()

# En muchas muestras solo se detectan una variedad de alimentos, lo cual no puede estar bien

# La decisión de eliminar estos numeros es en consenso con el boxplot de nutrientes
outliers_eliminar <- data.frame(
  ID = c("5415", "5438", "5454"),
  Visita = c("1", "2", "4"))

# Eliminar esos registros respetando ID y Visita
Tablas_unificadas <- Tablas_unificadas |> 
  anti_join(outliers_eliminar, by = c("ID", "Visita"))

## NORMALIDAD
# Seleccionamos los nombres de solo las columnas numéricas
cols_num <- Tablas_unificadas |> 
  select(where(is.numeric)) |> 
  colnames()

# Función para variante de K-S
lillie_fun <- function(x) {
  x <- na.omit(x)
  if (length(x) < 3) return(NA)
  lillie.test(x)$p.value}

# Aplicar por grupo de fuente
normalidad_por_fuente <- Tablas_unificadas |>
  group_by(Fuente) |>
  summarise(across(all_of(cols_num), lillie_fun)) |>
  pivot_longer(-Fuente, names_to = "variable", values_to = "p_value") |>
  mutate(normal = p_value > 0.05)

normalidad_por_fuente

normalidad_todas <- Tablas_unificadas |> 
  summarise(across(all_of(cols_num), lillie_fun)) |> 
  pivot_longer(everything(), names_to = "variable", values_to = "p_value") |> 
  mutate(normal = p_value > 0.05)

normalidad_todas

# No siguen una distribución normal al ser porcentajes e ir de un rango del 0 al 100

## Test de levenne
# Test por nutriente diferenciando fuente

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
# Sale la homogeneidad no normal

## Test de Wilcoxon
# Seleccionar solo las columnas numéricas
datos_num <- Tablas_unificadas |> 
  select(where(is.numeric))

# Pivotar a formato largo
datos_largos <- Tablas_unificadas |> 
  pivot_longer(
    cols = all_of(names(datos_num)),
    names_to = "nutriente",
    values_to = "valor")

# Hacer test de Wilcoxon agrupando por group_diet y nutriente 
resultados_final <- datos_largos |> 
  group_by(Group_diet, nutriente) |> 
  nest() |>    
  mutate(
    p_valor = map_dbl(data, ~{
      x <- .x$valor[.x$Fuente == "Cuestionario"]
      y <- .x$valor[.x$Fuente == "MEDI"]
      if(length(x) == length(y) && length(x) > 0) {
        wilcox.test(x, y, paired = TRUE)$p.value
      } else {
        NA_real_}})) |> 
  unnest(cols = c(data)) |>   
  select(-valor, -Fuente) |> 
  mutate(significativo = ifelse(p_valor < 0.05, "Sí", "No")) |> 
  distinct(Group_diet, nutriente, p_valor, significativo) |> 
  arrange(Group_diet, nutriente)

print(resultados_final)

# Vegetales y frutas tienden a salir no significativos junto a cereales de Hansen, pero el resto sale significativo.

# Graficar resultados
# Datos y graficar resultados
medias <- Tablas_unificadas |> 
  select(Fuente, Group_diet, where(is.numeric)) |> 
  pivot_longer(cols = c(-Fuente, -Group_diet), names_to = "Nutriente", values_to = "Valor") |> 
  group_by(Nutriente, Fuente, Group_diet) |> 
  summarise(
    Promedio = mean(Valor, na.rm = TRUE),
    SD = sd(Valor, na.rm = TRUE),
    N = sum(!is.na(Valor)),
    SE = SD / sqrt(N),
    .groups = "drop")

# Gráfico 
g1 <- ggplot(medias, aes(x = Group_diet, y = Promedio, fill = Fuente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = Promedio - SE, ymax = Promedio + SE),
    width = 0.2,
    position = position_dodge(width = 0.8)) +
  facet_wrap(
    ~ Nutriente,
    scales = "free_y",
    strip.position = "top",
    labeller = labeller(
      Nutriente = c(
        "Carnes_%"    = "Meat (%)",
        "Cereales_%"  = "Cereals (%)",
        "Frutas_%"    = "Fruits (%)",
        "Vegetales_%" = "Vegetables (%)"))) +
  labs(
    x = "",
    y = "",
    title = "Control group food intake",
    fill = "Source") +
  theme_minimal(base_size = 13) +
  scale_fill_manual(
    values = c("Cuestionario" = "#F8766D", "MEDI" = "#00BFC4"),
    labels = c(
      "Cuestionario" = "Questionnaire",
      "MEDI"         = "MEDI")) +
  geom_text(
    data = medias |> dplyr::distinct(Group_diet),
    aes(x = Group_diet, y = -Inf, label = Group_diet),
    vjust = -0.5,
    inherit.aes = FALSE,
    size = 3.5) +
  theme(
    axis.text.x  = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 20, l = 10),
    strip.text = element_text(margin = margin(b = 20)))

print(g1)

## Correlación
# Automatizar el modelo  para todos los alimentos

# Función para ejecutar el análisis en un subconjunto
analizar_subconjunto <- function(df, cols_num) {
  resultados_corr <- data.frame(
    Nutriente = character(),
    Spearman_rho = numeric(),
    p_value = numeric(),
    Grupo_diet = character(),
    stringsAsFactors = FALSE)
  # Iterar sobre cada grupo de dieta
  for (grupo in unique(df$Group_diet)) {
    df_grupo <- df[df$Group_diet == grupo, ]
    # Pivotar MEDI y Cuestionario
    Datos_pivotados <- tidyr::pivot_wider(
      df_grupo,
      names_from = Fuente,
      values_from = all_of(cols_num),
      names_sep = "_")
    # Iterar sobre cada nutriente
    for (nut in cols_num) {
      x <- Datos_pivotados[[paste0(nut, "_MEDI")]]
      y <- Datos_pivotados[[paste0(nut, "_Cuestionario")]]

      if (all(is.na(x)) | all(is.na(y))) next
      # Correlación de Spearman
      spearman_test <- cor.test(x, y, method = "spearman", exact = FALSE)

      resultados_corr <- rbind(resultados_corr, data.frame(
        Nutriente = nut,
        Spearman_rho = spearman_test$estimate,
        p_value = spearman_test$p.value,
        Grupo_diet = grupo))}}
  
  # Ajustar p-valores por múltiples comparaciones
  resultados_corr$p_bonferroni <- p.adjust(resultados_corr$p_value, method = "bonferroni")
  resultados_corr$p_fdr <- p.adjust(resultados_corr$p_value, method = "fdr")
  return(resultados_corr)}

# Ejecutar el análisis en los tres subconjuntos
res_corr <- analizar_subconjunto(Tablas_unificadas, cols_num)

# Mostrar tabla final
print(res_corr)
write.xlsx(res_corr, "Tablas_resultados/Corr_foods_controles.xlsx")

# No sale nada significativo (como era de esperar), pero en vegetales se acerca a significación y tiene la mejor r de spearman 

# Guardar medias y SE para tablas

medias_formateada <- medias |> 
  mutate(
    media_se = sprintf("%.2f ± %.2f", Promedio, SE))

tabla_final <- medias_formateada |> 
  select(Nutriente, Fuente, Group_diet, media_se) |> 
  pivot_wider(
    names_from = Group_diet,
    values_from = media_se) |> 
  arrange(Nutriente)


write.xlsx(tabla_final, "Tablas_resultados/Tabla_medias_controles_alimentos.xlsx")
