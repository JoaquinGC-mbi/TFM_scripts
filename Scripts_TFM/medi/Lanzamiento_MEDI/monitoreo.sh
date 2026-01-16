#!/usr/bin/env bash
# Monitorea el número de archivos .k2 en work

# ==== CONFIGURA AQUÍ ====
OUTFILE="/home/proyectos/imdeaalim/jgcorder/medi/logs/monitor_k2.log"
INTERVAL=1200  # 20 minutos = 1200 segundos
WORK="/scratch/jgcorder/work"

# ==== Primer argumento como ID del JOB ====
if [ -z "$1" ]; then
  echo "Uso: $0 <JOB_ID>"
  exit 1
fi

JOB_ID="$1"

# ==== INICIO ====
mkdir -p "$(dirname "$OUTFILE")"
echo "Guardando resultados en $OUTFILE"
echo "----------------------------------------" >> "$OUTFILE"

while squeue -j "$JOB_ID" > /dev/null 2>&1; do
  if squeue -j "$JOB_ID" | grep -q "$JOB_ID"; then
    echo "===== $(date '+%Y-%m-%d %H:%M:%S') =====" | tee -a "$OUTFILE"
    ls "$WORK"/*/*/*.k2 2>/dev/null | wc -l | tee -a "$OUTFILE"
    echo "----------------------------------------" >> "$OUTFILE"
    sleep "$INTERVAL"
  else
    echo "El job $JOB_ID ha terminado. Saliendo..." | tee -a "$OUTFILE"
    break
  fi
done
