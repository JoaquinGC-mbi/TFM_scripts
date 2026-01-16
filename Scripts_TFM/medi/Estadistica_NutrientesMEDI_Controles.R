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
library(psych) # para hacer confidencia
library(purrr)
library(nortest) # test de Lilliefors (K-S corregido)
library(car) # Test de Levenne

# Carga de archivos de abundancia de alimentos y nutrientes
rm(list=ls()) # Clean global environment 

setwd("/home/vant/TFM/")
getwd()

DIAL_mg_alimento <- read_xlsx("Nutricion/Cuestionarios_nutrientes_mg_100g.xlsx")
Nutrientes_MEDI <- read_xlsx("medi/Nutrientes_MEDI_controles_final.xlsx")

### ESTUDIO DE NUTRIENTES
# Preparar datos
colnames(DIAL_mg_alimento)
colnames(Nutrientes_MEDI)

DIAL_mg_alimento$Fuente <- "Cuestionario"

DIAL_mg_alimento$Visita <- as.character(DIAL_mg_alimento$Visita)
Nutrientes_MEDI$Visita <- as.character(Nutrientes_MEDI$Visita)  

# Unificar tablas
# Introducir la columna de grupos de dieta en nutrientes de MEDI
Nutrientes_MEDI <- Nutrientes_MEDI |> 
  left_join(DIAL_mg_alimento |>  
              select(ID, Group_diet, Visita), by = c("ID", "Visita"))

# Encontrar las columnas comunes
cols_comunes <- intersect(names(DIAL_mg_alimento), names(Nutrientes_MEDI))

# Combinar solo por las columnas comunes
Nutrientes_DIAL_MEDI <- bind_rows(
  select(DIAL_mg_alimento, all_of(cols_comunes), Fuente),
  select(Nutrientes_MEDI, all_of(cols_comunes), Fuente)) |> 
  filter(Estudio != "IMDEA") |> 
  relocate(Estudio, Fuente, .after = ID)

# Retirar voluntarios que para la misma visita, no cuenten con cuestionario y datos de MEDI
Nutrientes_DIAL_MEDI <- Nutrientes_DIAL_MEDI |> 
  group_by(ID, Visita) |> 
  # contar cuántos tipos de estudio distintos tiene por visita
  filter(n_distinct(Fuente) == 2) |> 
  ungroup()

# Quitar alcohol del analisis
Nutrientes_DIAL_MEDI <- Nutrientes_DIAL_MEDI |> 
  select(-`Alcohol (mg/100g)`)

library(openxlsx)
write.xlsx(Nutrientes_DIAL_MEDI, "Nutricion/Nutrientes_DIAL_MEDI_controles.xlsx") 

## BOXPLOT
# Hay datos bastante extremos, por lo que sería util llevar a cabo un boxplot para eliminar los outliers

# Pasar a formato largo
Tablas_long <- Nutrientes_DIAL_MEDI |> 
  pivot_longer(cols = c("Energía (Kcal/100g)", 
                        "Proteinas (mg/100g)", 
                        "Grasa total (mg/100g)", 
                        "Glucidos total (mg/100g)"),
               names_to = "Nutriente",
               values_to = "Nutriente_100g") |> 
  select(ID, Estudio, Fuente, Visita, Nutriente, Nutriente_100g)

# Detectar outliers por nutriente
outliers <- Tablas_long |> 
  group_by(Nutriente) |> 
  mutate(Q1 = quantile(Nutriente_100g, 0.25),
         Q3 = quantile(Nutriente_100g, 0.75),
         IQR = Q3 - Q1,
         Lower = Q1 - 1.5 * IQR,
         Upper = Q3 + 1.5 * IQR,
         Es_outlier = Nutriente_100g < Lower | Nutriente_100g > Upper) |> 
  filter(Es_outlier) |> 
  ungroup()

# Boxplot separado por fuente
ggplot(Tablas_long, aes(x = Fuente, y = Nutriente_100g)) +
  geom_boxplot(outlier.shape = NA, fill = "lightblue") +
  geom_text(data = outliers, aes(label = ID), 
            position = position_jitter(width = 0.1), size = 3, color = "red") +
  facet_wrap(~Nutriente, scales = "free_y") +  
  labs(title = "Boxplots de nutrientes con IDs de outliers",
       x = "",
       y = "Nutriente / 100g") +
  theme_minimal()

# Eliminar outliers: se tiene en cuenta datos ilogicos en el consumo de principales macronutrientes y grupos de alimentos obtenidos con MEDI
outliers_eliminar <- data.frame(
  ID = c("5415", "5438", "5454"),
  Visita = c("1", "2", "4"))

# Eliminar esos registros respetando ID y Visita
Tablas_unificadas <- Tablas_unificadas |> 
  anti_join(outliers_eliminar, by = c("ID", "Visita"))

## NORMALIDAD
# Seleccionamos solo las columnas numéricas
cols_num <- Nutrientes_DIAL_MEDI_boxplot |> 
  select(where(is.numeric)) |> 
  colnames()

# Función para variante de K-S
lillie_fun <- function(x) {
  x <- na.omit(x)
  if (length(x) < 3) return(NA)
  lillie.test(x)$p.value}

# Aplicar por grupo de fuente
normalidad_por_fuente <- Nutrientes_DIAL_MEDI_boxplot |>
  group_by(Fuente) |>
  summarise(across(all_of(cols_num), lillie_fun)) |>
  pivot_longer(-Fuente, names_to = "variable", values_to = "p_value") |>
  mutate(normal = p_value > 0.05)

normalidad_por_fuente

normalidad_todas <- Nutrientes_DIAL_MEDI_boxplot |> 
  summarise(across(all_of(cols_num), lillie_fun)) |> 
  pivot_longer(everything(), names_to = "variable", values_to = "p_value") |> 
  mutate(normal = p_value > 0.05)

normalidad_todas

# La gran mayoria de los datos no siguen una distribución normal, solo la energia

## Test de levenne
# Test por nutriente diferenciando fuente

levene_resultados <- Nutrientes_DIAL_MEDI_boxplot |>
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

# De nuevo, la mayoria de los paramatros no cumplen con la normalidad en la homogeneidad 
# Comprobamos si transformando los datos a logatmicos, siguen una distribución normal

## Aplicar transformación logarítmica
Datos_log <- Nutrientes_DIAL_MEDI_boxplot
Datos_log[cols_num] <- log(Datos_log[cols_num] + 1)

normalidad_por_fuente_log <- Datos_log |>
  group_by(Fuente) |>
  summarise(across(all_of(cols_num), lillie_fun)) |>
  pivot_longer(-Fuente, names_to = "variable", values_to = "p_value") |>
  mutate(normal = p_value > 0.05)

normalidad_por_fuente_log

normalidad_todas_log <- Datos_log |> 
  summarise(across(all_of(cols_num), lillie_fun)) |> 
  pivot_longer(everything(), names_to = "variable", values_to = "p_value") |> 
  mutate(normal = p_value > 0.05)

normalidad_todas_log

levene_log <- Datos_log |>
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

levene_log

# No mejora sustancialmente la normalidad de las muestras

## Test de Wilcoxon diferenciando en función del cuestionario
# Seleccionar solo las columnas numéricas
datos_num <- Nutrientes_DIAL_MEDI_boxplot |> 
  select(where(is.numeric))

# Pivotar a formato largo
datos_largos <- Nutrientes_DIAL_MEDI_boxplot |> 
  pivot_longer(
    cols = all_of(names(datos_num)),
    names_to = "nutriente",
    values_to = "valor")

# Hacer test de Wilcoxon agrupando por group_diet, nutriente y visita
resultados_final <- datos_largos |> 
  group_by(Group_diet, nutriente) |> 
  nest() |>    # dataframe anidado por grupo
  mutate(
    p_valor = map_dbl(data, ~{
      x <- .x$valor[.x$Fuente == "Cuestionario"]
      y <- .x$valor[.x$Fuente == "MEDI"]
      if(length(x) == length(y) && length(x) > 0) {
        wilcox.test(x, y, paired = TRUE)$p.value
      } else {
        NA_real_}})) |> 
  unnest(cols = c(data)) |>   # opcional, si quieres los datos originales también
  select(-valor, -Fuente) |> # limpiar columnas sobrantes
  mutate(significativo = ifelse(p_valor < 0.05, "Sí", "No")) |> 
  distinct(Group_diet, nutriente, p_valor, significativo) |> 
  arrange(Group_diet, nutriente)

print(resultados_final)
write.xlsx(resultados_final, "medi/Tablas_resultados/resultados_wilcoxon_control.xlsx")

# Los datos de folico no tienen ningun sentido en MEDI, son demasiado altos

# Datos y graficar resultados
medias <- Nutrientes_DIAL_MEDI_boxplot |> 
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
  facet_wrap(~Nutriente, scales = "free_y") +
  labs(
    x = "Grupo de dieta",
    y = "Promedio ± SE",
    title = "Promedios por Fuente y Grupo de dieta") +
  theme_minimal(base_size = 13)

print(g1)

### Indice de confidencia
# Inicializar lista de resultados
resultados_icc <- list()

# Iterar sobre columnas
for (col in names(datos_num)) {
  x <- datos_num[[col]][Nutrientes_DIAL_MEDI_boxplot$Fuente == "Cuestionario"]
  y <- datos_num[[col]][Nutrientes_DIAL_MEDI_boxplot$Fuente == "MEDI"]
  
  if (all(is.na(x)) | all(is.na(y))) next
  
  datos_icc <- cbind(Cuestionario = x, MEDI = y)
  icc <- psych::ICC(datos_icc)
  
  # Extraer ICC2 y ICC3
  icc2 <- icc$results$ICC[2]
  icc3 <- icc$results$ICC[3]
  
  # Guardar en lista
  resultados_icc[[col]] <- c(ICC2 = icc2, ICC3 = icc3)}

# Convertir a data.frame
icc_df <- do.call(rbind, lapply(names(resultados_icc), function(col) {
  data.frame(
    columna = col,
    ICC2 = resultados_icc[[col]]["ICC2"],
    ICC3 = resultados_icc[[col]]["ICC3"])}))

# Mostrar tabla
print(icc_df)
write.xlsx(icc_df, "medi/Tablas_resultados/ICC_nutrientes_controles.xlsx")

# No es necesario subdividir por Grupo de dieta para el ICC
# El ICC es bastante malo en practicamente todo

### Correlacion de Pearson
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
      
      # Saltar si todos los valores son NA
      if (all(is.na(x)) | all(is.na(y))) next
      
      # Correlación de Spearman
      spearman_test <- cor.test(x, y, method = "spearman", exact = FALSE)
      
      # Guardar resultados
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
res_corr <- analizar_subconjunto(Nutrientes_DIAL_MEDI_boxplot, cols_num)

# Mostrar tabla final
print(res_corr)
write.xlsx(res_corr, "medi/Tablas_resultados/Corr_nutrientes_controles.xlsx")

# Nada es significativo en macronutrientes y micronutrientes con respecto a correlación entre cuestionario y MEDI dependiendo del tipo de cuestionario. 

# Guardar medias y SE para tablas

medias_formateada <- medias |> 
  mutate(
    media_se = sprintf("%.5f ± %.5f", Promedio, SE))

tabla_final <- medias_formateada |> 
  select(Nutriente, Fuente, Group_diet, media_se) |> 
  pivot_wider(
    names_from = Group_diet,
    values_from = media_se) |> 
  arrange(Nutriente)


write.xlsx(tabla_final, "medi/Tablas_resultados/Tabla_medias_controles_nutricion.xlsx")

