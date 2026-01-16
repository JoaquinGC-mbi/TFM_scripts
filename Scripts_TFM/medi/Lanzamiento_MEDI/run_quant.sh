#!/bin/bash
#SBATCH -J try_quant
#SBATCH -o /home/proyectos/imdeaalim/jgcorder/medi/logs/try_quant_%j.out
#SBATCH -e /home/proyectos/imdeaalim/jgcorder/medi/logs/try_quant_%j.err
#SBATCH -t 120:00:00 # Yo creo que con esto sería suficiente
#SBATCH -c 24
#SBATCH --mem=380G # Lo de los gigas habra que verlo.

set -euo pipefail

# Rutas
DBDIR=/home/proyectos/imdeaalim/nicodcroig/medi/medi_db
RESULTSDIR=/home/proyectos/imdeaalim/jgcorder/medi/result_Hansen_2018_prueba
DATADIR=/home/proyectos/imdeaalim/jgcorder/muestras_control/Hansen_2018_prueba
export NXF_WORK=/scratch/jgcorder/work
mkdir -p "$NXF_WORK"
mkdir -p "$RESULTSDIR"

module load medi/0.2.1

echo ">> Iniciaciando cuantificación..."
nextflow run quant.nf -resume \
  --db "$DBDIR" \
  --out_dir "$RESULTSDIR" \
  --data_dir "$DATADIR" \
  --confidence 0.1 \
  --consistency 0.7 \
  --entropy 0.7 \
  --multiplicity 1 \
  --threshold 2

echo ">> Cuantificación finalizada con exito!"

rm -rf "$NXF_WORK"/*
