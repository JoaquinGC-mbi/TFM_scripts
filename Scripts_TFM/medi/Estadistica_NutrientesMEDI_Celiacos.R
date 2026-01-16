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
library(car) # Test de Levenne

# Carga de archivos de abundancia de alimentos y nutrientes
rm(list=ls()) # Clean global environment 

setwd("/home/vant/TFM/")
getwd()

DIAL_mg_alimento <- read_xlsx("Nutricion/Cuestionarios_nutrientes_mg_100g.xlsx")
Nutrientes_MEDI <- read_xlsx("medi/Nutrientes_MEDI_celiacos.xlsx")

### ESTUDIO DE NUTRIENTES
# Preparar datos
colnames(DIAL_mg_alimento)
colnames(Nutrientes_MEDI)

DIAL_mg_alimento$Fuente <- "Cuestionario"
Nutrientes_MEDI$Fuente <- "MEDI"
Nutrientes_MEDI$Visita <- 1
Nutrientes_MEDI$Estudio <- "IMDEA"

DIAL_mg_alimento$Visita <- as.character(DIAL_mg_alimento$Visita)
Nutrientes_MEDI$Visita <- as.character(Nutrientes_MEDI$Visita)  

# Unificar tablas
# Encontrar las columnas comunes
cols_comunes <- intersect(names(DIAL_mg_alimento), names(Nutrientes_MEDI))

# Combinar solo por las columnas comunes
Nutrientes_DIAL_MEDI <- bind_rows(
  select(DIAL_mg_alimento, all_of(cols_comunes), Fuente),
  select(Nutrientes_MEDI, all_of(cols_comunes), Fuente)) |> 
  filter(Estudio == "IMDEA") |> 
  relocate(Estudio, Fuente, .after = ID)

library(openxlsx)
write.xlsx(Nutrientes_DIAL_MEDI, "Nutricion/Nutrientes_DIAL_MEDI_sinboxplot.xlsx") 

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

# Boxplot separado por nutriente
ggplot(Tablas_long, aes(x = "", y = Nutriente_100g)) +
  geom_boxplot(outlier.shape = NA, fill = "lightblue") +
  geom_text(data = outliers, aes(label = ID), 
            position = position_jitter(width = 0.1), size = 3, color = "red") +
  facet_wrap(~Nutriente, scales = "free_y") +  # gráfico separado por nutriente
  labs(title = "Boxplots de nutrientes con IDs de outliers",
       x = "",
       y = "Nutriente / 100g") +
  theme_minimal()

# Eliminar outliers: se tiene en cuenta datos ilogicos en el consumo de principales macronutrientes y grupos de alimentos obtenidos con MEDI
# A partir de los resultados del boxplot, determinamos que los voluntarios 1797, 1827, 1838, 1824
Nutrientes_DIAL_MEDI_boxplot <- Nutrientes_DIAL_MEDI |> 
  filter(!ID %in% c(1797, 1827, 1838, 1824))

# No se aprecia una mejora muy sustancial de los resultados, siendo que las conclusiones con boxplot son practicamente iguales a las de sin boxplot

## NORMALIDAD
# Seleccionamos solo las columnas numéricas
cols_num <- Nutrientes_DIAL_MEDI_boxplot |> 
  select(where(is.numeric)) |> 
  colnames()

# Función para Shapiro-Wilk
shapiro_fun <- function(x) {
  x <- na.omit(x)
  if(length(x) < 3) return(NA)
  shapiro.test(x)$p.value}

# Aplicar por grupo de fuente
normalidad_por_fuente <- Nutrientes_DIAL_MEDI_boxplot |>
  group_by(Fuente) |>
  summarise(across(all_of(cols_num), shapiro_fun)) |>
  pivot_longer(-Fuente, names_to = "variable", values_to = "p_value") |>
  mutate(normal = p_value > 0.05)

normalidad_por_fuente

normalidad_todas <- Nutrientes_DIAL_MEDI_boxplot |> 
  summarise(across(all_of(cols_num), shapiro_fun)) |> 
  pivot_longer(everything(), names_to = "variable", values_to = "p_value") |> 
  mutate(normal = p_value > 0.05)

normalidad_todas

# La gran mayoria de los datos no siguen una distribución normal, solo las proteinas
## Test de Levenne
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

# Comprobamos si transformando los datos a logatmicos, siguen una distribución normal

# Aplicar transformación logarítmica
Datos_log <- Nutrientes_DIAL_MEDI_boxplot
Datos_log[cols_num] <- log(Datos_log[cols_num] + 1)

normalidad_por_fuente_log <- Datos_log |>
  group_by(Fuente) |>
  summarise(across(all_of(cols_num), shapiro_fun)) |>
  pivot_longer(-Fuente, names_to = "variable", values_to = "p_value") |>
  mutate(normal = p_value > 0.05)

normalidad_por_fuente_log

normalidad_todas_log <- Datos_log |> 
  summarise(across(all_of(cols_num), shapiro_fun)) |> 
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

## Test de Wilcoxon
# Seleccionar solo los datos numericos
datos_num <- Nutrientes_DIAL_MEDI_boxplot |> 
  select(where(is.numeric))

resultados <- list()

for (col in names(datos_num)) {
  x <- datos_num[[col]][Nutrientes_DIAL_MEDI_boxplot$Fuente == "Cuestionario"]
  y <- datos_num[[col]][Nutrientes_DIAL_MEDI_boxplot$Fuente == "MEDI"]
  
  # Test de Wilcoxon pareado
  test <- wilcox.test(x, y, paired = TRUE)
  
  resultados[[col]] <- test$p.value}

# Convertir a data.frame y añadir columna de significancia
resultados_df <- data.frame(
  columna = names(resultados),
  p_valor = unlist(resultados))

resultados_df$significativo <- ifelse(resultados_df$p_valor < 0.05, "Sí", "No")

print(resultados_df)

# No se detectan diferencias significativas entre medias en Energía, Grasa total, Azucares simples y Vitamina D. 
# Revisar bien lo de la vitamina D, porque el grupo que presentaba una mayor sintomatología consumia menos vitamina D segun los cuestionarios. 

# Graficar resultados
# Calcular promedio y error estándar por columna y grupo
datos_num_fuente <- Nutrientes_DIAL_MEDI_boxplot |> 
  select(Fuente, where(is.numeric))

medias <- datos_num_fuente |> 
  pivot_longer(cols = -Fuente, names_to = "Nutriente", values_to = "Valor") |> 
  group_by(Nutriente, Fuente) |> 
  summarise(
    Promedio = mean(Valor, na.rm = TRUE),
    SD = sd(Valor, na.rm = TRUE),
    N = sum(!is.na(Valor)),
    SE = SD / sqrt(N),
    .groups = "drop")

# Grafica de barras
ggplot(medias, aes(x = Fuente, y = Promedio, fill = Fuente)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Promedio - SE, ymax = Promedio + SE), width = 0.2) +
  facet_wrap(~Nutriente, scales = "free_y") +
  labs(x = "", y = "Promedio ± SE", title = "Promedios por Fuente") +
  theme_minimal() +
  theme(legend.position = "none")

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
write.xlsx(icc_df, "medi/Tablas_resultados/ICC_nutrientes_celiacos.xlsx")

# Un ICC alto indicaría que Cuestionario y MEDI producen resultados muy parecidos sujeto por sujeto. Es decir, los métodos son intercambiables o uno puede validar al otro.
# Un ICC bajo (como el que obtuviste: ~0.0 a 0.2 en la mayoría de nutrientes) indica que los dos métodos no concuerdan bien: un mismo sujeto puede obtener valores muy distintos dependiendo del método.
# En nuestro caso, ha salido un ICC bajo. Esto junto con los resultados de Wilcoxon indican que los métodos, en promedio, no difieren, pero no se pueden usar indistintamente para el mismo individuo.

### Correlacion de Pearson
# Organizar las tablas y pivotar
cols_no_pivotar <- c("ID", "Estudio", "Visita", "Fuente")
Datos_pivotados <- Nutrientes_DIAL_MEDI_boxplot |> 
  pivot_wider(names_from = Fuente, values_from = setdiff(names(Nutrientes_DIAL_MEDI_boxplot), cols_no_pivotar))

# Automatizar el modelo lineal para todos los nutrientes

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
write.xlsx(resultados_corr, "medi/Tablas_resultados/Corr_nutrientes_celiacos.xlsx")

# Graficos de correlación de proteinas y fibra dietetica
nutrientes_corr <- c("Proteinas (mg/100g)", "Fibra dietetica (mg/100g)")

for (nut in nutrientes_corr) {
  x <- Datos_pivotados[[paste0(nut, "_MEDI")]]
  y <- Datos_pivotados[[paste0(nut, "_Cuestionario")]]
  df_plot <- data.frame(Cuestionario = y, MEDI = x)
  p <- ggplot(df_plot, aes(x = Cuestionario, y = MEDI)) +
    geom_point(color = "blue") +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    labs(title = paste("Regresión lineal -", nut),
         x = "Cuestionario",
         y = "MEDI") +
    theme_minimal()
  print(p)
  # Pausar entre gráficos si estás en consola interactiva
  readline(prompt = "Presiona [Enter] para continuar al siguiente gráfico...")}

# Nutrientes como Proteinas y Fibra dietetica muestran correlación positiva significativa.
# Nutrientes como Energía, Grasa total, Glucidos tienen pendientes muy pequeñas y no significativas, indicando que MEDI y DIAL no se correlacionan bien en estos casos.
# Para vitaminas y minerales, la relación suele ser débil, probablemente por variabilidad en la estimación.

## Grafico de Bland-Altman
# Esto tambien sirve para mirar la concordancia entre datos
# Iterar sobre cada nutriente
for (nut in cols_num) {
  # Extraer datos
  x <- Datos_pivotados[[paste0(nut, "_MEDI")]]
  y <- Datos_pivotados[[paste0(nut, "_Cuestionario")]]
  # Calcular promedio y diferencia
  promedio <- (x + y) / 2
  diferencia <- x - y
  # Estadísticas
  media_diff <- mean(diferencia, na.rm = TRUE)
  sd_diff <- sd(diferencia, na.rm = TRUE)
  limite_superior <- media_diff + 1.96 * sd_diff
  limite_inferior <- media_diff - 1.96 * sd_diff
  # Crear gráfico
  p <- ggplot(data = data.frame(promedio, diferencia), aes(x = promedio, y = diferencia)) +
    geom_point(color = "blue", alpha = 0.6) +
    geom_hline(yintercept = media_diff, color = "red", linetype = "dashed") +
    geom_hline(yintercept = limite_superior, color = "darkgreen", linetype = "dotted") +
    geom_hline(yintercept = limite_inferior, color = "darkgreen", linetype = "dotted") +
    labs(title = paste("Bland-Altman para", nut),
         x = "Promedio de los métodos",
         y = "Diferencia (MEDI - Cuestionario)") +
    theme_minimal()
  print(p)
  # Pausar entre gráficos si estás en consola interactiva
  readline(prompt = "Presiona [Enter] para continuar al siguiente gráfico...")}

# Guardar medias y SE para tablas
medias_formateada <- medias |> 
  mutate(
    media_se = sprintf("%.5f ± %.5f", Promedio, SE))

tabla_final <- medias_formateada |> 
  select(Nutriente, Fuente, media_se) |> 
  pivot_wider(
    names_from = Fuente,
    values_from = media_se) |> 
  arrange(Nutriente)


write.xlsx(tabla_final, "medi/Tablas_resultados/Tabla_medias_celiacos_nutrientes.xlsx")
