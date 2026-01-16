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
library(openxlsx)

# BiocManager::install(c(
#  "mia",
#  "curatedMetagenomicData",
#  "TreeSummarizedExperiment",
#  "SingleCellExperiment",
#  "SummarizedExperiment",
#  "vegan"
# ), update = TRUE, ask = FALSE)

library(SingleCellExperiment)
library(SummarizedExperiment)
library(TreeSummarizedExperiment)
library(mia)
library(vegan, quietly = TRUE)
library(scater)
# BiocManager::install("curatedMetagenomicData")
library(curatedMetagenomicData)

rm(list=ls()) # Clean global environment 

# Establecer directorio de trabajo
setwd("/home/vant/TFM/")
getwd()

### ANÁLISIS 
# Exploracion de datos
# Estudio 2018
names(sampleMetadata)
sampleMetadata |>
  filter(study_name == "HansenLBS_2018") |>
  summarise(
    total_muestras = n(),
    n_individuos = n_distinct(subject_id),
    edad_promedio = mean(age, na.rm = TRUE),
    edad_sd = sd(age, na.rm = TRUE),
    visitas = n_distinct(visit_number),
    genero = paste(unique(gender), collapse = ", "),
    condiciones = paste(unique(study_condition), collapse = ", "),
    body_sites = paste(unique(body_site), collapse = ", "))

sampleMetadata |>
  filter(study_name == "HansenLBS_2018") |>
  pull(sample_id)
# Los datos de HansenLBS_2018 no estan bien separados, visit number no indica la visita, esta en el ID de la muestra
# De este datasheet solo necesitamos las muestras de las visitas 1, 2 y 4, que son las unicas que tienen datos de dieta

# Obtener la información de abundancia de los  estudios

combine_abundance <-
  sampleMetadata |>
  filter(body_site == "stool", study_name %in% "HansenLBS_2018") |> 
  filter(!(study_name == "HansenLBS_2018" & str_detect(sample_id, "-3$"))) |>  # solo para HansenLBS_2018 excluye los que terminan en -3
  select(where(~ !all(is.na(.x)))) |> 
  returnSamples("relative_abundance", rownames = "sample_id") 
matriz_abundancia <- assay(combine_abundance, "relative_abundance")

# Este script se puede usar en el caso de que se quiera trabajar con abundancia relativa, ya que la base de datos esta en porcentajes
matriz_proporcion <- matriz_abundancia / 100 # para transformar a proporciones los valores

# Extraer metadatos de muestras

HansenLBS_metadata <- sampleMetadata |>
  filter(body_site == "stool", study_name == "HansenLBS_2018") |> 
  filter(!str_detect(sample_id, "-3$")) |>   # excluir las que terminan en -3 
  select(where(~ !all(is.na(.x))))

HansenLBS_metadata$days_from_first_collection[is.na(HansenLBS_metadata$days_from_first_collection)] <- 0 # los valores NA se convierten en 0

# Retirar del estudio los voluntarios de menos de 18 años 
menores_id <- HansenLBS_metadata$sample_id[HansenLBS_metadata$age < 18]

metadata_total <- HansenLBS_metadata |> 
  filter(!(sample_id %in% menores_id))

matriz_proporcion <- matriz_proporcion[, !(colnames(matriz_proporcion) %in% menores_id)] 

## Emplear Phyloseq para extraer datos de las muestras de celiacos

library(phyloseq)

# Leer archivo phyloseq
Celiacos_Micro <- readRDS("Microbiota/physeq.tree.rds")
# Tenemos un archivo phyloseq con la siguiente información
otu_table(Celiacos_Micro)       # Tabla de abundancias, es una matriz de conteos absolutos
tax_table(Celiacos_Micro)       # Clasificación taxonómica
sample_data(Celiacos_Micro)     # Metadatos
phy_tree(Celiacos_Micro)        # Árbol filogenético

# Extraer los datos y convertir a abundancia relativa
Celiacos_por_millon <- tax_glom(Celiacos_Micro, taxrank = "Species")
Celiacos_abund <- transform_sample_counts(Celiacos_por_millon, function(x) x / sum(x))
Celiacos_abund <- as(otu_table(Celiacos_abund), "matrix")
Celiacos_meta <- as(sample_data(Celiacos_Micro), "data.frame")

## Combinar tablas en una 
# Comprobar que tienen los mismos nombres de taxones
all(rownames(matriz_proporcion) == rownames(Celiacos_abund))
setdiff(rownames(matriz_proporcion), rownames(Celiacos_abund))

# Eliminar los ID de celiacos outliers seleccionados por boxplot en nutrientes y alimentos
Celiacos_abund2 <- Celiacos_abund[ ,!colnames(Celiacos_abund) %in% c("1797","1827","1838","1824")]
matriz_proporcion2 <- matriz_proporcion[ ,!colnames(matriz_proporcion) %in% c("SID5415-1", "SID5438-2", "SID5454-4")]

# Comprobamos que el formato de los nombres es muy diferente, así que tenemos que adaptarlos 
# Eliminar "__" en el archivo phyloseq de celiacos y los corchetes
rownames(Celiacos_abund2) <- gsub("\\[|\\]", "", gsub("_", " ", gsub("^s__", "", rownames(Celiacos_abund2))))
rownames(Celiacos_abund2) <- gsub("(?<=[A-Za-z])\\s+(?=\\d)", "", rownames(Celiacos_abund2), perl = TRUE) # para quitar espacios entre letras y numeros (en ese orden)
Celiacos_abund2 <- Celiacos_abund2[order(rownames(Celiacos_abund2)), ]

# Eliminar species: al principio y los corchetes
rownames(matriz_proporcion2) <- gsub("^species:", "", gsub("\\[|\\]", "", rownames(matriz_proporcion2)))
rownames(matriz_proporcion2) <- gsub("-", "", gsub(":", "", gsub("\\.", "", rownames(matriz_proporcion2))))
rownames(matriz_proporcion2) <- gsub("(?<=[a-z])\\s+(?=\\d)", "", rownames(matriz_proporcion2), perl = TRUE) # para quitar espacios entre letras y numeros (en ese orden)
matriz_proporcion2 <- matriz_proporcion2[order(rownames(matriz_proporcion2)), ]

# Combinar ambas tablas
# Convertir matrices a data.frames manteniendo rownames
df1 <- as.data.frame(matriz_proporcion2)
df2 <- as.data.frame(Celiacos_abund2)

# Añadir columna con rownames para poder hacer merge
df1$species <- rownames(df1)
df2$species <- rownames(df2)

# Hacer merge con all=TRUE para incluir todas las especies
Abundancias_todo <- merge(df1, df2, by = "species", all = TRUE)

# Reemplazar NA por 0
Abundancias_todo[is.na(Abundancias_todo)] <- 0

# Poner la columna species como rownames
rownames(Abundancias_todo) <- Abundancias_todo$species
Abundancias_todo$species <- NULL

# Aun quedan especies sin juntar porque hay diferencias pequeñas 

Abundancias_todo$nombre <- rownames(Abundancias_todo)
# Normaliza nombres (quita espacios, símbolos, pone en minúsculas)
Abundancias_todo$nombre_limpio <- tolower(gsub("[^a-z0-9]", "", rownames(Abundancias_todo)))
# Agrupa y combina
Abundancias_todo <- Abundancias_todo |> 
  group_by(nombre_limpio) |> 
  summarise(nombre_real = dplyr::first(nombre), across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))

# hacemos que el nombre_real sea el nombre de las filas
Abundancias_todo <- as.data.frame(Abundancias_todo)
rownames(Abundancias_todo) <- Abundancias_todo$nombre_real
Abundancias_todo$nombre_real <- NULL
Abundancias_todo$nombre_limpio <- NULL
Abundancias_todo <- Abundancias_todo[order(rownames(Abundancias_todo)), ]

# Guardar la base de datos 
write.csv(Abundancias_todo, "Microbiota/matriz_abundancias_combinado.csv") 

## Juntar metadatos
Celiacos_meta$study_name <- "IMDEA"
# Insertar genero de los metadatos de nutrición (no esta incluido en los metadatos del phyloseq)
meta_nutri_celiacos <- read.xlsx("Nutricion/metadatados.xlsx")
Celiacos_meta$gender <-
  meta_nutri_celiacos$Sexo[
    match(Celiacos_meta$Sample_ID, meta_nutri_celiacos$Sample_ID)]

Celiacos_meta <- Celiacos_meta |> 
  dplyr::rename(sample_id = Sample_ID, BMI = IMC, age = Edad)
Celiacos_meta$sample_id <- as.character(Celiacos_meta$sample_id)

columnas_comunes2 <- intersect(names(Celiacos_meta), names(metadata_total))

metadata_combinado <- bind_rows(
  select(Celiacos_meta, all_of(columnas_comunes2)),
  select(metadata_total, all_of(columnas_comunes2)))

# Eliminar outliers 
metadata_combinado <- metadata_combinado |> 
  filter(!sample_id %in% c("1797", "1827", "1838", "1824", "SID5415-1", "SID5438-2", "SID5454-4"))

# Guardar metada combinado
write.xlsx(metadata_combinado, "Microbiota/Metadata_combinado.xlsx", rowNames = FALSE) 

### Correción de batch effect
# Se selecciona día de recogida debido a que otros posibles efectos, como el secuenciador o el centro, son comunes a todas las muestras
# Valorar correción de batch effect con Mmuphin para las muestras control (intraestudio)
# BiocManager::install("MMUPHin")
library(MMUPHin)

# Comenzamos ajuste de batch
rownames(metadata_combinado) <- metadata_combinado$sample_id
setdiff(colnames(Abundancias_todo), rownames(metadata_combinado)) # no faltan ID
identical(sort(colnames(Abundancias_todo)), sort(rownames(metadata_combinado))) # pero no estan ordenados
metadata_combinado <- metadata_combinado[colnames(Abundancias_todo), ] # para ordenar columnas de acuerdo con la matriz de abundancias

Batch_effect <- adjust_batch(
  feature_abd = Abundancias_todo,
  batch = "study_name",                      
  data = metadata_combinado,
  control = list(verbose = FALSE))

matriz_ajustada <- Batch_effect$feature_abd_adj

# Visualización antes y después de la corrección
D_before <- vegdist(t(Abundancias_todo))
D_after <- vegdist(t(matriz_ajustada))

set.seed(1)
fit_adonis_before <- adonis2(D_before ~ study_name, data = metadata_combinado)
fit_adonis_after <- adonis2(D_after ~ study_name, data = metadata_combinado)
print(fit_adonis_before)
print(fit_adonis_after)

# Representación grafica de datos
library(ggplot2)

# Realizamos un PCoA
# Calcular distancias y PCoA para antes y después
library(ape)

pcoa_before <- pcoa(D_before)
pcoa_after <- pcoa(D_after)

# Porcentaje de varianza explicada para cada PCoA
var_exp_before <- round(100 * pcoa_before$values$Relative_eig[1:2], 1)
var_exp_after  <- round(100 * pcoa_after$values$Relative_eig[1:2], 1)

# Crear etiquetas de ejes con varianza
xlab_before <- paste0("PCoA1 (", var_exp_before[1], "%)")
ylab_before <- paste0("PCoA2 (", var_exp_before[2], "%)")

xlab_after <- paste0("PCoA1 (", var_exp_after[1], "%)")
ylab_after <- paste0("PCoA2 (", var_exp_after[2], "%)")

# Dataframes con ejes y etiquetas
df_before <- data.frame(
  Axis1 = pcoa_before$vectors[,1],
  Axis2 = pcoa_before$vectors[,2],
  batch = metadata_combinado$study_name,
  estado = "Sin corregir",
  xlab = xlab_before,
  ylab = ylab_before)

df_after <- data.frame(
  Axis1 = pcoa_after$vectors[,1],
  Axis2 = pcoa_after$vectors[,2],
  batch = metadata_combinado$study_name,
  estado = "Corregido por MMUPHin",
  xlab = xlab_after,
  ylab = ylab_after)

# Unir ambos
df_combined <- bind_rows(df_before, df_after)

# Para poner las etiquetas a cada grafica
axis_labels <- c(
  "Sin corregir" = paste0("Without correction: PCoA1 (", var_exp_before[1], "%), PCoA2 (", var_exp_before[2], "%)"),
  "Corregido por MMUPHin" = paste0("Batch effect correction: PCoA1 (", var_exp_after[1], "%), PCoA2 (", var_exp_after[2], "%)")
)

ggplot(df_combined, aes(x = Axis1, y = Axis2, color = batch)) +
  geom_point(size = 2, alpha = 0.8) +
  facet_wrap(~estado, ncol = 1, labeller = labeller(estado = axis_labels)) +
  theme_minimal() +
  labs(x = "PCoA1", y = "PCoA2", color = "Group") +
  scale_color_manual(
    values = c("IMDEA" = "blue", "HansenLBS_2018" = "orange"),
    labels = c("IMDEA" = "Study", "HansenLBS_2018" = "Control")) +
  theme(strip.text = element_text(size = 14, face = "bold"),
        legend.position = "bottom")

# Se observa una mejora relativa de los datos procedentes de los celiacos del IMDEA
# Probamos a hacerlo con y sin batch effect para ver si hay diferencias grandes

### Analizar la alpha diversidad 
# para ello, es mejor usar la matriz de conteos, no la ajustada por batch effect
# los datos originales tanto de celiacos como curatedmetagenomicdata estan en abundancia relativa, así que hay que convertirlos a conteos (abundancia absoluta)

# Comprobar que metadatos y abundancias tienen las mismas muestras ...
Abundancias_todo <- as.matrix(Abundancias_todo)
setdiff(colnames(Abundancias_todo), rownames(metadata_combinado))
identical(sort(colnames(Abundancias_todo)), sort(rownames(metadata_combinado)))

# ... y el mismo orden 
# Solo las muestras comunes
muestras_comunes <- intersect(colnames(Abundancias_todo), rownames(metadata_combinado))
# Subconjuntos ordenados igual
Abundancias_todo <- Abundancias_todo[, muestras_comunes]
metadata_combinado <- metadata_combinado[muestras_comunes, ]

# Antes hay que eliminar las filas cuyos valores sean 0 (es decir, que no hay bacterias en ninguna muestra)
Abundancias_todo <- Abundancias_todo[rowSums(Abundancias_todo) > 0, ]

# Hay que crear la variable biologica de grupo (control o celiacos)
metadata_combinado <- metadata_combinado |> 
  mutate(Group = ifelse(study_name == "IMDEA", "celiac", "control"))

# Crear objeto phyloseq 
phylo_abund <- otu_table(Abundancias_todo, taxa_are_rows = TRUE)
sampledata_combinado <- sample_data(metadata_combinado) 
physeq_total <- phyloseq(phylo_abund, sampledata_combinado)

# Crear pseudo-conteos
physeq_pseudo_counts <- transform_sample_counts(physeq_total, function(x) round(1E6 * x))

# Guardar objeto
saveRDS(physeq_pseudo_counts, file = "Microbiota/Physeq_pseudo_counts.rds")

## Analizar alpha-diversidad
# BiocManager::install("microbiome")
library(microbiome)

sample_data(physeq_pseudo_counts)$study_name <- as.factor(sample_data(physeq_pseudo_counts)$study_name)
rich <- estimate_richness(physeq_pseudo_counts)

# Estadistica
rich$study_name <- sample_data(physeq_pseudo_counts)$study_name
pvals <- c(
  wilcox.test(Chao1 ~ study_name, data = rich)$p.value,
  wilcox.test(Shannon ~ study_name, data = rich)$p.value,
  wilcox.test(Simpson ~ study_name, data = rich)$p.value)

# Ajustar p-alores por Benjamini–Hochberg para corregir el FDR
pvals_adj <- p.adjust(pvals, method = "BH")

# Graficar
library(ggpubr)

plot_alpha <- plot_richness(physeq_pseudo_counts, 
                            color = "study_name", 
                            x = "study_name", 
                            measures = c("Chao1", "Simpson", "Shannon")) + 
  geom_boxplot(aes(fill = study_name), alpha = 0.7) + 
  scale_color_manual(values = c("blue", "red"), labels = c("Control", "Study")) + 
  scale_fill_manual(values = c("blue", "red"), labels = c("Control", "Study")) + 
  scale_x_discrete(labels = c("HansenLBS_2018" = "Control", "IMDEA" = "Study")) + 
  labs(x = "Group", y = "Alpha diversity", color = "Group", fill = "Group") + 
  theme_bw() +
  stat_compare_means(
    method = "wilcox.test", 
    comparisons = list(c("HansenLBS_2018", "IMDEA")),
    label = "p.signif",
    size = 6) + 
  theme(
    axis.title = element_text(size = 16, face = "bold"),      
    axis.text = element_text(size = 14),                     
    legend.title = element_text(size = 16, face = "bold"),   
    legend.text = element_text(size = 14),                  
    strip.text = element_text(size = 16, face = "bold"))      

print(plot_alpha)

# Chao1 (riqueza de especies estimada)
# HansenLBS_2018 muestra los valores más altos, indicando mayor riqueza de especies.
# IMDEA tiene los valores más bajos, sugiriendo menor riqueza de especies.

# Shannon (riqueza + equitatividad)
# HansenLBS_2018 tienen valores similares y más altos, lo que sugiere comunidades más diversas y equitativas.
# IMDEA presenta valores más bajos, reflejando menor diversidad y/o dominancia de ciertas especies.

#Simpson (dominio de especies)
# Los dos estudios muestran valores relativamente altos (>0.7), lo que indica baja dominancia de una sola especie y comunidades relativamente equitativas.
# IMDEA presenta más variación en los valores, con algunos casos de menor equitatividad.

# Conclusión general:
# HansenLBS_2018: alta riqueza (Chao1) y buena diversidad (Shannon).
# IMDEA: menor diversidad en todos los índices, indicando comunidades microbianas más pobres o dominadas.

### Analisis beta diversidad
# Emplear primero el modelo ajustado en datos de estudios.

## Comenzamos analisis 
# Antes hay que eliminar las filas cuyos valores sean 0 (es decir, que no hay bacterias en ninguna muestra)
Abundancias_filtrado <- matriz_ajustada[rowSums(matriz_ajustada) > 0, ]
# Vegdist espera que las columnas sean taxas y las filas sean samples
Abundancias_filtrado <- t(Abundancias_filtrado)
write.csv(Abundancias_filtrado, "Microbiota/matriz_Abundancias_ajustado.csv") 

# Bray-Curtis
dist_bc <- vegdist(Abundancias_filtrado, method = "bray")
permanova_bc <- adonis2(dist_bc ~ Group, data = metadata_combinado, permutations = 999)

# Para calcular la homogeneidad de dispersiones (comprobacion de que los datos son fiables)
betadisp_bc <- betadisper(dist_bc, metadata_combinado$Group)
anova(betadisp_bc)

# Jaccard (binaria)
dist_jac <- vegdist(Abundancias_filtrado, method = "jaccard", binary = TRUE)
permanova_jac <- adonis2(dist_jac ~ Group, data = metadata_combinado, permutations = 999)

betadisp_jac <- betadisper(dist_jac, metadata_combinado$Group)
anova(betadisp_jac)

# CLR + Euclidean
Abundancias_clr <- microbiome::transform(Abundancias_filtrado, "clr")
dist_clr <- dist(Abundancias_clr, method = "euclidean")
permanova_clr <- adonis2(dist_clr ~ Group, data = metadata_combinado, permutations = 999)

betadisp_clr <- betadisper(dist_clr, metadata_combinado$Group)
anova(betadisp_clr)

# Bray–Curtis y Jaccard:
# No hay diferencias significativas en la dispersión.
# Los grupos son comparables en términos de varianza interna.

# CLR (Centered Log-Ratio):
# Hay una diferencia fuerte en dispersión (p < 0.001).
# Los grupos tienen distinta variabilidad en el espacio CLR (composicional).
# Esto puede indicar que uno de los grupos tiene más heterogeneidad microbiana que el otro.

# Los resultados de la permanova arrojan que con todas las pruebas se observan diferencias significativas en la composición entre grupos

## Crear un PCoA y graficar para cada test 
library(ggrepel)

# Bray-Curtis
pcoa_bc <- pcoa(dist_bc)

coords_bc <- as.data.frame(pcoa_bc$vectors[,1:2])
coords_bc$Sample <- rownames(coords_bc)
coords_bc$Group <- metadata_combinado[coords_bc$Sample, "Group"]

coords_bc <- coords_bc |> 
  mutate(Group = ifelse(Group == "celiac", "study", Group))

p_bc <- ggplot(coords_bc, aes(x = Axis.1, y = Axis.2, color = Group)) +
  geom_point(size = 3) +
  xlab(paste0("PCoA1 (", round(pcoa_bc$values$Relative_eig[1]*100, 1), "%)")) +
  ylab(paste0("PCoA2 (", round(pcoa_bc$values$Relative_eig[2]*100, 1), "%)")) +
  ggtitle("PCoA - Bray-Curtis") +
  theme_minimal()

p_bc

# Jaccard
pcoa_jac <- pcoa(dist_jac)

coords_jac <- as.data.frame(pcoa_jac$vectors[,1:2])
coords_jac$Sample <- rownames(coords_jac)
coords_jac$Group <- metadata_combinado[coords_bc$Sample, "Group"]

coords_jac <- coords_jac |> 
  mutate(Group = ifelse(Group == "celiac", "study", Group))

p_jac <- ggplot(coords_jac, aes(x=Axis.1, y=Axis.2, color=Group)) +
  geom_point(size=3) +
  xlab(paste0("PCoA1 (", round(pcoa_jac$values$Relative_eig[1]*100,1), "%)")) +
  ylab(paste0("PCoA2 (", round(pcoa_jac$values$Relative_eig[2]*100,1), "%)")) +
  ggtitle("PCoA - Jaccard") +
  theme_minimal()

p_jac

# CLR + Euclidean
pcoa_clr <- pcoa(dist_clr)

coords_clr <- as.data.frame(pcoa_clr$vectors[,1:2])
coords_clr$Sample <- rownames(coords_clr)
coords_clr$Group <- metadata_combinado[coords_bc$Sample, "Group"]

coords_clr <- coords_clr |> 
  mutate(Group = ifelse(Group == "celiac", "study", Group))

p_clr <- ggplot(coords_clr, aes(x=Axis.1, y=Axis.2, color=Group)) +
  geom_point(size=3) +
  xlab(paste0("PCoA1 (", round(pcoa_clr$values$Relative_eig[1]*100,1), "%)")) +
  ylab(paste0("PCoA2 (", round(pcoa_clr$values$Relative_eig[2]*100,1), "%)")) +
  ggtitle("PCoA - CLR Euclidean") +
  theme_minimal()

p_clr

# Combinar todos los plots en uno
library(patchwork)

plot_beta_diver <- (p_bc | p_jac | p_clr) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 14),
    axis.title   = element_text(size = 16),
    axis.text    = element_text(size = 14),
    plot.title   = element_text(size = 18, face = "bold"))

plot_beta_diver

ggsave(filename = "Microbiota/beta_all_plots.png",
  plot = plot_beta_diver,
  width = 20,
  height = 12,
  dpi = 300)


# Observams una separación con Euclidean y Jaccard, pero no con Bray Curtis

## Probar con los datos sin ajustar

Abundancias_todo_2 <- t(Abundancias_todo)

# Bray-Curtis
dist_bc_no <- vegdist(Abundancias_todo_2, method = "bray")
permanova_bc_no <- adonis2(dist_bc_no ~ Group, data = metadata_combinado, permutations = 999)

# Para calcular la homogeneidad de dispersiones (comprobacion de que los datos son fiables)
betadisp_bc_no <- betadisper(dist_bc_no, metadata_combinado$Group)
anova(betadisp_bc_no)

# Jaccard (binaria)
dist_jac_no <- vegdist(Abundancias_todo_2, method = "jaccard", binary = TRUE)
permanova_jac_no <- adonis2(dist_jac_no ~ Group, data = metadata_combinado, permutations = 999)

betadisp_jac_no <- betadisper(dist_jac_no, metadata_combinado$Group)
anova(betadisp_jac_no)

# CLR + Euclidean
Abundancias_clr_no <- microbiome::transform(Abundancias_todo_2, "clr")
dist_clr_no <- dist(Abundancias_clr_no, method = "euclidean")
permanova_clr_no <- adonis2(dist_clr_no ~ Group, data = metadata_combinado, permutations = 999)

betadisp_clr_no <- betadisper(dist_clr_no, metadata_combinado$Group)
anova(betadisp_clr_no)

# En todas las pruebas, las diferencias entre el grupo control y celiacos es significativa, explicando entre un 5 y un 9% de la variabilidad (significativo en microbiología)
# En cuanto a la homogeneidad de dispersion, en todas las pruebas a salido que aceptamos la H0 (p-value < 0.05), por o que asumimos que la dispersion es homogenea y los resultados son confiables

## Crear un PCoA y graficar para cada test 

# Bray-Curtis
pcoa_bc <- pcoa(dist_bc_no)

coords_bc <- as.data.frame(pcoa_bc$vectors[,1:2])
coords_bc$Sample <- rownames(coords_bc)
coords_bc$Grupo <- metadata_combinado[coords_bc$Sample, "Group"]

p_bc_no <- ggplot(coords_bc, aes(x=Axis.1, y=Axis.2, color=Grupo)) +
  geom_point(size=3) +
  geom_text_repel(aes(label=Sample)) +
  xlab(paste0("PCoA1 (", round(pcoa_bc$values$Relative_eig[1]*100,1), "%)")) +
  ylab(paste0("PCoA2 (", round(pcoa_bc$values$Relative_eig[2]*100,1), "%)")) +
  ggtitle("PCoA - Bray-Curtis") +
  theme_minimal()

p_bc_no

# Jaccard
pcoa_jac <- pcoa(dist_jac_no)

coords_jac <- as.data.frame(pcoa_jac$vectors[,1:2])
coords_jac$Sample <- rownames(coords_jac)
coords_jac$Grupo <- metadata_combinado[coords_bc$Sample, "Group"]

p_jac_no <- ggplot(coords_jac, aes(x=Axis.1, y=Axis.2, color=Grupo)) +
  geom_point(size=3) +
  geom_text_repel(aes(label=Sample)) +
  xlab(paste0("PCoA1 (", round(pcoa_jac$values$Relative_eig[1]*100,1), "%)")) +
  ylab(paste0("PCoA2 (", round(pcoa_jac$values$Relative_eig[2]*100,1), "%)")) +
  ggtitle("PCoA - Jaccard") +
  theme_minimal()

p_jac_no

# CLR + Euclidean
pcoa_clr <- pcoa(dist_clr_no)

coords_clr <- as.data.frame(pcoa_clr$vectors[,1:2])
coords_clr$Sample <- rownames(coords_clr)
coords_clr$Grupo <- metadata_combinado[coords_bc$Sample, "Group"]

p_clr_no <- ggplot(coords_clr, aes(x=Axis.1, y=Axis.2, color=Grupo)) +
  geom_point(size=3) +
  geom_text_repel(aes(label=Sample)) +
  xlab(paste0("PCoA1 (", round(pcoa_clr$values$Relative_eig[1]*100,1), "%)")) +
  ylab(paste0("PCoA2 (", round(pcoa_clr$values$Relative_eig[2]*100,1), "%)")) +
  ggtitle("PCoA - CLR Euclidean") +
  theme_minimal()

p_clr_no

# Nos quedamos con los datos ajustados, ya que por lo menos parece que hemos eliminado algo del factor tecnico entre estudios
