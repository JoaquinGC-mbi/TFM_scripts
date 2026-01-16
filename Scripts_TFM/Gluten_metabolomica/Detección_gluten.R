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
library(car)
library(nortest) # test de Lilliefors (K-S corregido)

# Carga de archivos de abundancia de alimentos y nutrientes
rm(list=ls()) # Clean global environment 

setwd("/home/vant/TFM")
getwd()

Datos_abundancia <- read_csv("medi/result_prueba_3_(buena)/food_abundance.csv")
Datos_sintomas <- read_csv("Metabolomica/DATAmfa.cluster.csv")
Datos_metabolomica <- read_csv("Metabolomica/metabolitos_KEGG_HMDB.csv")

### DETECTOR DE MENTIRAS

# MEDI no detecta gluten en las muestras, pero si detecta la presencia de trigo, cebada u otros compuestos que si contienen gluten
# Importante mencionar que MEDI detecta la presencia de buckwheat o trigo sarraceno, una variedad que no contiene gluten
# Base de datos que solo contiene cereales

Cereales <- Datos_abundancia |> 
  filter(str_detect(food_subgroup, "Cereals")) |> 
  select(sample_id, wikipedia_id, food_group, food_subgroup, species, reads, relative_abundance, total_reads, bacteria_reads, human_reads)

# Vamos a agrupar los resultados de abundancia de cereales en cereales con gluten y sin gluten

# Vector de cereales con gluten
cereales_gluten <- c("Oriental wheat" ,"Triticale", "Barley", "Common wheat", "Wheat", "Hard_wheat", "Khorasan_wheat", "Rye")
# Vector de cereales sin gluten
cereales_sin_gluten <- c("Spelt" ,"Annual wild rice" ,"Red rice", "Common buckwheat", "Tartary buckwheat", "Buckwheat", "Corn", "Fagopyrum tataricum", "Millet", "Oryza_rufipogon", "Rice", "Sorghum", "Wild_rice", "Zizania_aquatica", "Oat")

cereales_agrupados <- Cereales |> 
  mutate(gluten_group = ifelse(tolower(wikipedia_id) %in% tolower(cereales_gluten), "Gluten",
                               ifelse(tolower(wikipedia_id) %in% tolower(cereales_sin_gluten), "Sin_gluten", NA)))

Voluntarios_gluten <- cereales_agrupados |> 
  filter(gluten_group == "Gluten")

print(Voluntarios_gluten)

Voluntarios_avena <- cereales_agrupados |> 
  filter(wikipedia_id == "Oat")

print(Voluntarios_avena)

# En los voluntarios D1801, 1807, 1815, 1831 y 1837 se detecta la presencia de alimentos que contienen gluten
# Tambien tener en cuenta que los voluntarios D1801, 1805 y 1844 consumen avena, y en las guias para celiacos advierten que en la avena es más facil la presencia de trazas de gluten por contaminacion

# Agrupar resultados en funcion de si consumen o no gluten
cereales_agrupados_filtro <- cereales_agrupados |> 
  mutate(sample_id = gsub("[^0-9]", "", sample_id)) 

cereales_agrupados_filtro2 <- cereales_agrupados_filtro |> 
  group_by(sample_id) |> 
  summarise(consume_gluten = ifelse(any(gluten_group == "Gluten"), 1, 0)) |> 
  rename(ID = sample_id)

### Detección de gluten en controles

detectar_gluten <- function(df) {
  # Filtrar solo cereales
  Cereales <- df |>
    filter(str_detect(food_subgroup, "Cereals"))
  
  # Clasificar gluten o sin gluten
  cereales_agrupados <- Cereales |>
    mutate(
      gluten_group = ifelse(
        tolower(wikipedia_id) %in% tolower(cereales_gluten), "Gluten",
        ifelse(
          tolower(wikipedia_id) %in% tolower(cereales_sin_gluten),
          "Sin_gluten", NA)))
  
  # Limpiar sample_id y resumir por paciente
  resultado <- cereales_agrupados |>
    mutate(sample_id = gsub("[^0-9]", "", sample_id)) |>
    group_by(sample_id) |>
    # Suma de abundancias de alimentos con gluten
    summarise(
      consume_gluten = ifelse(any(gluten_group == "Gluten"), 1, 0),
      abundancia_gluten = sum(relative_abundance[gluten_group == "Gluten"], na.rm = TRUE),
      .groups = "drop") |>
    rename(ID = sample_id)
  
  return(resultado)}

# Cargar datos y operar
Datos_abundancia_controles <- read_xlsx("medi/Alimentos_controles_MEDI_unificado.xlsx")
resultados_controles <- detectar_gluten(Datos_abundancia_controles)
# Corregir número de ID y crear visita
resultados_controles_2 <- resultados_controles |> 
  mutate(
    visit_number = str_extract(ID, "\\d$"),
    ID = str_remove(ID, "\\d$"))

# Contar el número de muestras en las que no detecta alimentos con gluten
table(resultados_controles_2$consume_gluten)

### ESTADISTICA
## Estadistica comparativa con los datos de sintomas y metabolomica
# Unificar tablas

Datos_sintomas$ID <- as.character(Datos_sintomas$ID)
Datos_sintomas <- Datos_sintomas |> 
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))

Gluten_cluster <- Datos_sintomas |> 
  left_join(cereales_agrupados_filtro2, by = "ID") |> 
  drop_na()

# Evaluar normalidad
shapiro.test(Gluten_cluster$Cluster[Gluten_cluster$consume_gluten == 1])
shapiro.test(Gluten_cluster$Cluster[Gluten_cluster$consume_gluten == 0])
# No son normales

# Comparar si hay relacion entre deteccion de gluten y cluster 
fisher.test(table(Gluten_cluster$Cluster, Gluten_cluster$consume_gluten))

# No encuentra asociacion entre el cluster 2 y el consumo de gluten. Normal teniendo en cuenta las pocas muestras que tenemos. 

## Comparación gluten con cuestionarios de consumo de gluten y pepetido inmunogenico del gluten
library(phyloseq)

# Leer archivo phyloseq
Celiacos_Micro <- readRDS("Microbiota/physeq.tree.rds")
Celiacos_meta <- as(sample_data(Celiacos_Micro), "data.frame")
Celiacos_meta$Sample_ID <- as.character(Celiacos_meta$Sample_ID)

Data_GIP <- Celiacos_meta |> 
  select(Sample_ID, GIP.heces, Puntuacion.Global.CDAT.V1) |> 
  rename(ID = Sample_ID) |> 
  left_join(cereales_agrupados_filtro2, by = "ID") |> 
  drop_na()

# Corregir datos erroneos
Data_GIP_2 <- Data_GIP |> 
  mutate(GIP.heces = if_else(ID %in% c(1808, 1834, 1835), GIP.heces / 1000, GIP.heces))

### Estadistica
# Evaluar normalidad
shapiro.test(Data_GIP_2$GIP.heces[Data_GIP_2$consume_gluten == 1])
shapiro.test(Data_GIP_2$GIP.heces[Data_GIP_2$consume_gluten == 0])
shapiro.test(Data_GIP_2$Puntuacion.Global.CDAT.V1[Data_GIP_2$consume_gluten == 1])
shapiro.test(Data_GIP_2$Puntuacion.Global.CDAT.V1[Data_GIP_2$consume_gluten == 0])
# No normal en GIP y normal en CDAT

# Homoceisticidad con el test de levene
cols_num_GIP <- Data_GIP_2 |> 
  select(where(is.numeric)) |> 
  select(-consume_gluten) |> 
  colnames()

levene_resultados_GIP <- Data_GIP_2 |> 
  pivot_longer(
    cols = all_of(cols_num_GIP),
    names_to = "Variable",
    values_to = "Valor") |> 
  group_by(Variable) |> 
  summarise(
    p_value = leveneTest(Valor ~ as.factor(consume_gluten))$`Pr(>F)`[1],
    .groups = "drop") |> 
  mutate(
    homocedastico = p_value > 0.05)

# Mostrar resultados
levene_resultados_GIP
# La varianza es aproximadamente constante en las dos variables 

# Test de Wilcoxon
wilcox.test(Data_GIP_2$GIP.heces ~ Data_GIP_2$consume_gluten)
t.test(Data_GIP_2$Puntuacion.Global.CDAT.V1 ~ Data_GIP_2$consume_gluten, var.equal = TRUE)

# Regresion lineal
modelo_GIP <- glm(GIP.heces ~ consume_gluten, data = Data_GIP_2, family = gaussian)
summary(modelo_GIP)
modelo_CDAT <- lm(Puntuacion.Global.CDAT.V1 ~ consume_gluten, data = Data_GIP_2)
summary(modelo_CDAT)

# Se necesitaría más muestra o control de confusores para confirmar la asociación.

# Graficar
ggplot(Data_GIP_2, aes(x = consume_gluten, y = GIP.heces)) +
  geom_jitter(width = 0.05, height = 0, alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Correlación entre consumo de gluten y niveles de GIP",
    x = "Consumo de gluten (0 = No, 1 = Sí)",
    y = "Nivel de GIP") +
  theme_minimal()

ggplot(Data_GIP_2, aes(x = consume_gluten, y = Puntuacion.Global.CDAT.V1)) +
  geom_jitter(width = 0.05, height = 0, alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Correlación entre consumo de gluten y CDAT",
    x = "Consumo de gluten (0 = No, 1 = Sí)",
    y = "Nivel de GIP") +
  theme_minimal()

resumen_GIP <- Data_GIP_2 |> 
  group_by(consume_gluten) |> 
  summarise(
    mean_GIP = mean(GIP.heces),
    sd_GIP = sd(GIP.heces),
    n = n(),
    se_GIP = sd_GIP / sqrt(n))

ggplot(resumen_GIP, aes(x = factor(consume_gluten), y = mean_GIP, fill = factor(consume_gluten))) +
  geom_bar(stat = "identity", width = 0.5, alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_GIP - se_GIP, ymax = mean_GIP + se_GIP), width = 0.2) +
  # Cambiar colores y labels de la leyenda
  scale_fill_manual(
    values = c("skyblue","salmon"),
    labels = c("No","Yes")  # labels de la leyenda
  ) +
  labs(
    x = "Detection of gluten",       
    y = "GIP in feces (ng/g)",  
    fill = "Gluten detection") +
  theme_classic(base_size = 14)

resumen_CDAT <- Data_GIP_2 |> 
  group_by(consume_gluten) |> 
  summarise(
    mean_GIP = mean(Puntuacion.Global.CDAT.V1),
    sd_GIP = sd(Puntuacion.Global.CDAT.V1),
    n = n(),
    se_GIP = sd_GIP / sqrt(n))

ggplot(resumen_CDAT, aes(x = factor(consume_gluten), y = mean_GIP, fill = factor(consume_gluten))) +
  geom_bar(stat = "identity", width = 0.5, alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_GIP - se_GIP, ymax = mean_GIP + se_GIP), width = 0.2) +
  # Cambiar colores y labels de la leyenda
  scale_fill_manual(
    values = c("skyblue","salmon"),
    labels = c("No","Yes")  # labels de la leyenda
  ) +
  labs(
    x = "Detection of gluten",       
    y = "CDAT Puntuation",  
    fill = "Gluten detection") +
  theme_classic(base_size = 14)

## Estadistica para deteccion de gluten en muestras control
# Comprobar si el consumo total de gluten reportado por el estudio se asocia con la detección
conc_gluten <- read_xlsx("Estudios_escogidos/HansenLBS_2018/Dietary_data.xlsx", sheet = 2) |> 
  select("ID", "Beh", "visit_number", "Gluten - total, mg/day")
conc_gluten$ID <- as.character(conc_gluten$ID)
conc_gluten$visit_number <- as.character(conc_gluten$visit_number)

resultados_controles_final <- resultados_controles_2 |> 
  left_join(conc_gluten |> select(ID, visit_number, `Gluten - total, mg/day`, Beh), by = c("ID", "visit_number")) |> 
  mutate(`Gluten - total, g/day` = `Gluten - total, mg/day` / 1000) |> 
  drop_na()

resultados_controles_final <- resultados_controles_final |> 
  rename(Group_diet = Beh) |> 
  mutate(Group_diet = case_when(
    Group_diet %in% c("Fstart", "Hstart") ~ "regular_diet",
    Group_diet == "Fend" ~ "low_gluten",
    Group_diet == "Hend" ~ "high_gluten"))

# Evaluar normalidad
cols_num <- resultados_controles_final |> 
  select(where(is.numeric)) |> 
  select(-consume_gluten) |> 
  colnames()

lillie_fun <- function(x) {
  x <- na.omit(x)
  if (length(x) < 3) return(NA)
  lillie.test(x)$p.value}

normalidad_GIP<- resultados_controles_final |>
  summarise(across(all_of(cols_num), lillie_fun)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "p_value") |>
  mutate(normal = p_value > 0.05)

normalidad_GIP
# Solo la abundancia sale normal

# Homoceisticidad con el test de levene
levene_resultados <- resultados_controles_final |> 
  pivot_longer(
    cols = all_of(cols_num),
    names_to = "Variable",
    values_to = "Valor") |> 
  group_by(Variable) |> 
  summarise(
    p_value = leveneTest(Valor ~ as.factor(consume_gluten))$`Pr(>F)`[1],
    .groups = "drop") |> 
  mutate(
    homocedastico = p_value > 0.05)

# Mostrar resultados
levene_resultados
# De nuevo sale que la abundancia de gluten es la que presenta varianzas iguales

# Comprobar si existe relación entre la detección o no de alimentos con gluten y el grupo (low, high or regular gluten consumption)

tabla <- table(resultados_controles_final$consume_gluten, resultados_controles_final$Group_diet)
# Residuos (para saber diferencias entre grupos)
res <- chisq.test(tabla)
res$stdres

# Si existe relación entre a detección o no de alimentos con gluten y el grupo, siendo más probable encontrar gluten en los grupos high-gluten y regular_diet

# Modelos
modelo_control_gluten <- glm(
  consume_gluten ~ `Gluten - total, g/day`,
  data = resultados_controles_final,
  family = binomial)

summary(modelo_control_gluten)

# Este modelo da significativo

# Para las abundancias, al incluir 0 es mejor hacer una transformación logaritmica
modelo_control_abundancias <- glm(
  `Gluten - total, g/day` ~ log1p(abundancia_gluten),
  data = resultados_controles_final, 
  family = gaussian)

summary(modelo_control_abundancias)

# Este no sale significativo, no hay relación clara entre la abundancia y las cantidades de gluten

# Graficar

resumen_group <- resultados_controles_final |> 
  group_by(Group_diet) |> 
  summarise(
    n_detected = sum(consume_gluten == 1),
    n_total = n(),
    prop_detected = n_detected / n_total)

ggplot(resumen_group, aes(x = Group_diet, y = prop_detected, fill = Group_diet)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Group diet", y = "Proportion of gluten containing foods detected", fill = "") +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") 

## Crear grafico de probabilidad de detección de gluten con los datos de la GLM
# Crear un rango de valores de gluten
gluten_range <- seq(min(resultados_controles_final$`Gluten - total, g/day`),
                    max(resultados_controles_final$`Gluten - total, g/day`),
                    length.out = 100)

# Predecir probabilidades
df_tmp <- resultados_controles_final
df_tmp$gluten_total <- df_tmp$`Gluten - total, g/day`

# Ajustar el modelo
modelo_control_gluten <- glm(consume_gluten ~ gluten_total, data = df_tmp, family = binomial)

# Crear rango de valores
gluten_range <- seq(min(df_tmp$gluten_total), max(df_tmp$gluten_total), length.out = 100)

# Operar y grafico
predicciones <- data.frame(
  gluten_total = gluten_range,
  prob = predict(modelo_control_gluten, newdata = data.frame(gluten_total = gluten_range), type = "response"))

ggplot(predicciones, aes(x = gluten_total, y = prob)) +
  geom_line(color = "salmon") +       
  labs(
    x = "Total gluten consumed (g/day)",
    y = "Probability of gluten detection") +
  theme_classic(base_size = 14)
