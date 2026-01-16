# Cargar librerías necesarias
library(dplyr)
library(tidyverse)
library(vegan)   # Diversidad y distancias
library(ggplot2) # Gráficas
library(readxl)
library(compositions)
library(limma)
library(ggplot2)
library(MASS)

# Carga de archivos de abundancia de alimentos y nutrientes
rm(list=ls()) # Clean global environment 

setwd("/home/vant/TFM/medi/")
getwd()

## COMPARACIÓN RESULTADOS FLAGS DE ALIMENTOS
# Carga de archivos 
Alimentos_celiacos <- read_csv("result_prueba_3_(buena)/food_abundance.csv")
Alimentos_completo <- read_xlsx("Alimentos_controles_MEDI_unificado.xlsx")

# Para combinar los datos de A/B
Alimentos_celiacos_corregido <- Alimentos_celiacos |> 
  # Crear ID sin la A/B final
  mutate(sample_id = str_replace(sample_id, "[AaBb]$", "")) |> 
  
  # Agrupar por el nuevo ID
  group_by(sample_id, wikipedia_id) |> 
  
  # Combinar datos
  summarise(
    # Promedio para columnas numéricas
    across(
      where(is.numeric),
      ~ mean(.x, na.rm = TRUE)),
    # Mantener valores de columnas no numéricas
    across(
      where(~ !is.numeric(.x)),
      ~ dplyr::first(.x)),
    .groups = "drop")

# Elimnados por boxplot obtenido del analisis de nutrientes

Alimentos_completo$Group <- "Control"
Alimentos_celiacos_corregido$Group <- "Celiac"
medi <- bind_rows(Alimentos_celiacos_corregido, Alimentos_completo) 

# Eliminación de muestras seleccionadas por boxplot
samples_to_remove <- c(
  "D1797", "D1827", "D1838", "D1824", "5415-1", "5438-2", "5454-4")

medi <- medi |> 
  dplyr::select(-any_of(samples_to_remove))

medi$Group[medi$Group == "Celiac"] <- "Study"

# Analisis de abundancias totales

total_abund <- medi |> 
  group_by(sample_id, Group) |> 
  summarise(total_abundancia = sum(relative_abundance, na.rm = TRUE))

ggplot(total_abund, aes(x = Group, y = total_abundancia)) +
  geom_boxplot() + geom_jitter(width = 0.2) +
  theme_minimal(base_size = 16) +
  geom_jitter(width = 0.2) +
  geom_signif(
    comparisons = list(c("Control", "Study")),
    annotations = "****",
    textsize = 5,
    y_position = max(total_abund$total_abundancia) * 1.1,
    tip_length = 0.02) +
  labs(title = "Proportion of reads assigned to foods",
       y = "Sum of relative abundance (foods)")

descriptivos_abund <- total_abund |>
  group_by(Group) |>
  summarise(
    across(
      where(is.numeric),
      list(
        media = ~mean(.x, na.rm = TRUE),
        sd    = ~sd(.x, na.rm = TRUE),
        n     = ~sum(!is.na(.x)),
        se    = ~ sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))))))

# Test estadístico (diferencia de abundancia global)
wilcox.test(total_abundancia ~ Group, data = total_abund)
# Las muestras de celiacos cuentan con una menor abundancia frente a las muesttras control

# Analisis de alimentos totales
total_alimentos <- medi |>  
  group_by(sample_id, Group) |> 
  summarise(n_alimentos_detectados = n_distinct(wikipedia_id))

library(ggsignif)

ggplot(total_alimentos, aes(x = Group, y = n_alimentos_detectados)) +
  geom_boxplot() +
  geom_jitter(width = 0.2) +
  geom_signif(
    comparisons = list(c("Control", "Study")),
    annotations = "n.s.",
    textsize = 5,
    y_position = max(total_alimentos$n_alimentos_detectados) * 1.1,
    tip_length = 0.02) +
  theme_minimal(base_size = 16) +
  labs(
    title = "Proportion of foods detected",
    y = "Richness (foods)")

descriptivos_alimentos <- total_alimentos |>
  group_by(Group) |>
  summarise(
    across(
      where(is.numeric),
      list(
        media = ~mean(.x, na.rm = TRUE),
        sd    = ~sd(.x, na.rm = TRUE),
        n     = ~sum(!is.na(.x)),
        se    = ~ sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))))))

wilcox.test(n_alimentos_detectados ~ Group, data = total_alimentos)

# Se detectan mas items con los nuevos flags
# Es decir, el cambio de flags no elimina detecciones, sino que baja el umbral de corte, aumentando la sensibilidad pero diluyendo la abundancia promedio.