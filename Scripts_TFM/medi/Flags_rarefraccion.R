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
library(phyloseq)
# install.packages("iNEXT")
library(iNEXT)

# Carga de archivos de abundancia de alimentos y nutrientes
rm(list=ls()) # Clean global environment 

setwd("/home/vant/TFM/medi/")
getwd()

### RAREFRACCION
## Rarefraccion de la microbiota (prueba)
# Quiero saber si estan en el mismo plato o batch
phylo <- readRDS("../Microbiota/Physeq_pseudo_counts.rds")
otu_table(phylo)
sample_data(phylo)

otu_rare <- as(otu_table(phylo), "matrix")
rownames(otu_rare) # las filas tienen que ser las muestras
otu_rare <- t(otu_rare)
meta <- as(sample_data(phylo), "data.frame")

# Ejecutar y graficar rarefraccion
sizes <- seq(1, min(rowSums(otu_rare)), by = 100)
richness_curves <- sapply(1:nrow(otu_rare), function(i) rarefy(otu_rare[i,], sample = sizes))
colnames(richness_curves) <- rownames(otu_rare)

df <- as.data.frame(richness_curves)
df$Size <- sizes
df_long <- pivot_longer(df, cols = -Size, names_to = "sample_id", values_to = "Richness")

df_long <- df_long |> 
  left_join(meta, by = "sample_id")

ggplot(df_long, aes(x = Size, y = Richness, color = study_name, group = sample_id)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(x = "Número de secuencias muestreadas",
       y = "Riqueza (OTUs)",
       title = "Curvas de rarefacción por estudio")

## Rarefracción alimentos
# Para ello, se puede construir una tabla OTU con alimentos en vez de especies bacterianas y los reads que MEDI detecta para cada alimento
make_food_matrix <- function(df) {
  df |> 
    # 1) Mantener solo sample, wikipedia_id y reads
    dplyr::select(sample_id, wikipedia_id, reads) |> 
    # 2) Sumar reads por muestra y alimento
    group_by(sample_id, wikipedia_id) |> 
    summarise(reads = sum(reads, na.rm = TRUE), .groups = "drop") |> 
    # 3) Convertir a matriz wide
    pivot_wider(
      names_from = sample_id,
      values_from = reads,
      values_fill = 0
    ) |> 
    # 4) Filas = alimentos
    column_to_rownames(var = "wikipedia_id")
}

Hansen_2018_V1 <- read.csv("result_Hansen_2018_V1/food_abundance.csv")
Hansen_2018_V2 <- read.csv("result_Hansen_2018_V2/food_abundance.csv")
Hansen_2018_V4 <- read.csv("result_Hansen_2018_V4/food_abundance.csv")
IMDEA <- read.csv("result_prueba_3_(buena)/food_abundance.csv")

Hansen_2018_V1_matrix <- make_food_matrix(Hansen_2018_V1)
Hansen_2018_V2_matrix <- make_food_matrix(Hansen_2018_V2)
Hansen_2018_V4_matrix <- make_food_matrix(Hansen_2018_V4)
IMDEA_matrix <- make_food_matrix(IMDEA)

# Quitar los A y B de IMDEA y sumar los reads
# Detectar columnas con A/B al final
cols_AB <- grep("[abAB]$", colnames(IMDEA_matrix), value = TRUE)

# Extraer la parte base (sin la letra)
bases <- sub("[abAB]$", "", cols_AB) |>  
  unique()

# Crear nuevas columnas sumando A + B
for(base in bases){
  cols_to_sum <- grep(paste0("^", base, "[abAB]$"), colnames(IMDEA_matrix), value = TRUE)
  IMDEA_matrix[[base]] <- rowSums(IMDEA_matrix[, cols_to_sum, drop = FALSE], na.rm = TRUE)
}

# Eliminar las columnas originales A/B
IMDEA_matrix <- IMDEA_matrix[, !colnames(IMDEA_matrix) %in% cols_AB]

# Combinar en un solo data.frame
# Crear la columna OTU y juntar
df1 <- cbind(OTU = rownames(Hansen_2018_V1_matrix), as.data.frame(Hansen_2018_V1_matrix))
df2 <- cbind(OTU = rownames(Hansen_2018_V2_matrix), as.data.frame(Hansen_2018_V2_matrix))
df3 <- cbind(OTU = rownames(Hansen_2018_V4_matrix), as.data.frame(Hansen_2018_V4_matrix))
df4 <- cbind(OTU = rownames(IMDEA_matrix), as.data.frame(IMDEA_matrix))

matrices_combinadas <- df1 |> 
  full_join(df2, by = "OTU") |> 
  full_join(df3, by = "OTU") |> 
  full_join(df4, by = "OTU")

# Poner 0 en los NA y eliminar la columna OTU
matrices_combinadas[is.na(matrices_combinadas)] <- 0

rownames(matrices_combinadas) <- matrices_combinadas$OTU
matrices_combinadas$OTU <- NULL

# Combertir los metadatos al formato de ID de la matrix
# Retirar el estudio Buscharta_2016
meta <- meta |> 
  filter(study_name != "Heitz-BuschartA_2016")

# Crear una nueva columna para los IDs normalizados
meta$ID_clean <- meta$sample_id

# eliminar prefijo SID → SID5420-2 → 5420-2
meta$ID_clean[meta$study_name == "HansenLBS_2018"] <- gsub(
  "^SID", "",
  meta$sample_id[meta$study_name == "HansenLBS_2018"]
)

# agregar "D" al principio de IDs numéricos
numeric_mask <- grepl("^[0-9]+$", meta$ID) & meta$study_name == "IMDEA"
meta$ID_clean[numeric_mask] <- paste0("D", meta$sample_id[numeric_mask])

# Eliminar sample_id y renombrar
meta_foods <- meta |> 
  dplyr::select(-sample_id) |> 
  rename(sample_id = ID_clean)
rownames(meta_foods) <- meta_foods$sample_id

# Eliminar outliers observados en foods y nutrientes
samples_to_remove <- c(
  "D1797", "D1827", "D1838", "D1824", "5415-1",
  "5438-2", "5454-4")

matrices_combinadas_corregida <- matrices_combinadas |> 
  dplyr::select(-any_of(samples_to_remove))

matrices_combinadas_corregida <- as.matrix(matrices_combinadas_corregida)
colSums(matrices_combinadas_corregida)

# Elimar los samples que suman 0 en reads
samples_zero <- colnames(matrices_combinadas_corregida)[colSums(matrices_combinadas_corregida) == 0]
matrices_combinadas_corregida <- matrices_combinadas_corregida[, colSums(matrices_combinadas_corregida) > 0]
meta_foods <- meta_foods[!meta_foods$sample_id %in% samples_zero, ]

## CURVA DE RAREFRACCION SIN SUBSAMPLING (Teorica)
# profundidad = número total de reads por muestra; riqueza = número de alimentos detectados
# Para obtener una matriz con el numero total de conteos por estudio
# tienen que ser filas alimentos y columnas muestras

Depth_rich <- sapply(unique(meta_foods$study_name), function(est){
  rowSums(matrices_combinadas_corregida[, meta_foods$study_name == est, drop=FALSE])
})

D_richness <- iNEXT(Depth_rich, q=0, datatype = "abundance")

ggiNEXT(D_richness) +
  labs(x = "Profundidad",
       y = "Riqueza")

## CURVA DE RAREFRACCION 
D_total <- iNEXT(matrices_combinadas_corregida, q=0, datatype = "abundance")

rare_total <- bind_rows(D_total$iNextEst)
rare_total$estudio <- meta_foods$study_name[match(rare_total$Assemblage, meta_foods$sample_id)]

ggplot(rare_total, aes(x = m, y = qD, color = estudio, group = Assemblage)) +
  geom_line() +
  scale_color_manual(
    name = "Groups",
    values = c("red", "blue"),
    labels = c(
      "HansenLBS_2018" = "Control",
      "IMDEA" = "Study"
    )
  ) +
  labs(x = "Depth (number of reads)", y = "Richness (Foods)") +
  theme_minimal() 
