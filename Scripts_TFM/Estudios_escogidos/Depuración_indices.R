# Script para depuración de los indices para descargar archivos de ENA

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
library(curatedMetagenomicData)

rm(list=ls()) # Clean global environment 

setwd("/home/vant/TFM/Estudios_escogidos/")
getwd()

## Indice de Hanse_2018
Hansen_2018 <- read_tsv("Indice_Hansen2018.txt")

Hansen_2018_filtrado <- Hansen_2018 |> 
  filter(!grepl("3$", sample_alias))

write.table(Hansen_2018_filtrado, file = "Indice_meta_Hansen2018.txt", sep = "\t", row.names = FALSE)

## Indice de Buscharta_2016
Buscharta_2016 <- read_tsv("Indice_meta_Buscharta2016.txt")

Buscharta_2016_filtrado <- Buscharta_2016 |> 
  filter(grepl("stool$", sample_alias))

# Eliminar los voluntarios menores de edad 
# En este indice viene, junto a los url de descarga de F y R, la descarga para el fastq completo que hay que quitar antes de pasarle el programa de descarga
BuschartA_metadata <- sampleMetadata |>
  filter(body_site == "stool", study_name == "Heitz-BuschartA_2016") |>
  select(where(~ !all(is.na(.x))))

menores_id <- BuschartA_metadata$sample_id[BuschartA_metadata$age < 18]

Buscharta_2016_filtrado_2 <- Buscharta_2016_filtrado |> 
  filter(!sample_alias %in% menores_id) |> 
  mutate(fastq_ftp = sub("^[^;]*;", "", fastq_ftp),
         fastq_md5 = sub("^[^;]*;", "", fastq_md5))

# En este archivo hay más muestras que las que nos interesan, solo queremos las M01, M02, M03 y M04
# Cada muestra se secuencio dos veces, y solo nos interesa la primera
Buscharta_2016_filtrado_3 <- Buscharta_2016_filtrado_2 |> 
  filter(grepl("^M0", sample_alias)) |> 
  arrange(sample_alias, run_accession) |> 
  group_by(sample_alias) |> 
  filter(row_number() == 1) |> 
  ungroup()
  
write.table(Buscharta_2016_filtrado_3, file = "Indice_meta_Buscharta2016.txt", sep = "\t", row.names = FALSE)
