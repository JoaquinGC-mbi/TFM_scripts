#!/bin/bash
#SBATCH -p bioinfo                      #partition/queue name
#SBATCH --job-name=download_jgc                #Job name
#SBATCH -N 1                   #Un nodo
#SBATCH -n 3                   #Cores para usar
#SBATCH -t 20:00:00             #Time limit hrs:min:sec
#SBATCH --output=/home/jgcorder/controles/log-%j.o #Log de salida
#SBATCH --error=/home/jgcorder/controles/log-%j.e #Log de errores
#SBATCH --mail-user=joaquingcordero@gmail.com
#SBATCH --mail-type=END,FAIL

# Carga del modulo donde esta el programa de descarga aria2c
module load fungigut/fungigut

# Script para automatizar la descarga de fastq de los estudios control desde ENA

INDICE=$1
SALIDA=$2

# Para asegurar que se pasan dos argumentos
if [ $# -lt 2 ]
then
    echo "Uso: $0 <archivo_report_ena.txt> <directorio_salida>"
    exit 1
fi

# Crea directorio de salida si no existe
mkdir -p "${SALIDA}"

echo ">>> Extrayendo URLs y datos del indice..."

# Este bucle esta preparado para extraer la información necesaria para que aria2c descargue los archivos
# seleccion de columnas que nos interesan
tail -n +2 "$INDICE" | cut -f1,7,8,11 | while IFS=$'\t' read -r run md5s urls sample; do
    # separar URLs y MD5s por ";"
    IFS=';' read -r url_f url_r <<<"$urls"
    IFS=';' read -r md5_f md5_r <<<"$md5s"

    # limpiar URLs (quitar comillas, espacios, pasar ftp->https)
    for u in url_f url_r; do
        eval val=\$$u
        val=$(echo "$val" | sed -E 's/^[[:space:]"“”]+//; s/[[:space:]"“”]+$//; s/^ftp\./https:\/\/ftp./')
        eval $u=\$val
    done

    # Limpiar en el resto de valores comillas y espacios
    sample=$(echo "$sample" | sed -E 's/^[[:space:]"“”]+//; s/[[:space:]"“”]+$//')
    md5_f=$(echo "$md5_f" | sed -E 's/^[[:space:]"“”]+//; s/[[:space:]"“”]+$//')
    md5_r=$(echo "$md5_r" | sed -E 's/^[[:space:]"“”]+//; s/[[:space:]"“”]+$//')
    # nombres de salida con SAMPLE + _1/_2
    out_f="${sample}_1.fastq.gz"
    out_r="${sample}_2.fastq.gz"

    # escribir al archivo urls.txt en formato aria2c
    echo "$url_f"
    echo "  out=$out_f"
    echo "  checksum=md5=$md5_f"
    echo
    echo "$url_r"
    echo "  out=$out_r"
    echo "  checksum=md5=$md5_r"
    echo
done > urls.txt

echo ">>> Descargando archivos con aria2c..."
# -x 3: hasta 3 conexiones por archivo
# -j 3: hasta 3 descargas concurrentes
# -c: reanuda si el archivo estaba incompleto
# puede reintentar tres veces descargar el archivo y entre intentos hay un tiempo de espera de 15 segundos
aria2c -x 3 -j 3 -c --max-tries=3 --retry-wait=15 --auto-file-renaming=false -d "${SALIDA}" -i urls.txt

# Borrar el archivo generado de urls
# rm -f urls.txt

echo "Descarga completada"


