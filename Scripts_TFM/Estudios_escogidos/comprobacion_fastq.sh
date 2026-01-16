#!/bin/bash
#SBATCH -p bioinfo                      #partition/queue name
#SBATCH --job-name=check_fastq_jgc                #Job name
#SBATCH -N 1                   #Un nodo
#SBATCH -n 3                   #Cores para usar
#SBATCH -t 15:00:00             #Time limit hrs:min:sec
#SBATCH --output=/home/jgcorder/check_fastq/log-%j.o #Log de salida
#SBATCH --error=/home/jgcorder/check_fastq/log-%j.e #Log de errores
#SBATCH --mail-user=joaquingcordero@gmail.com
#SBATCH --mail-type=END,FAIL

# Carga del modulo donde esta el programa de descarga aria2c
module load fungigut/fungigut

# Comprobar que se pasa un directorio
if [ $# -lt 1 ]; then
    echo "Uso: $0 <directorio_fastq>"
    exit 1
fi

DIR="$1"

# Verificar que el directorio existe
if [ ! -d "$DIR" ]; then
    echo "El directorio $DIR no existe."
    exit 1
fi

# Funcion para descargar archivos corruptos
re_download() {
    local file="$1"
    local base=$(basename "$file")        # Ej: SRR7989811_1.fastq.gz
    local srr=$(echo "$base" | cut -d'_' -f1)  # Ej: SRR7989811
    # Construir ruta ENA
    local prefix=${srr:0:6}               # Ej: SRR798
    local suffix=${srr:9:3}               # Ej: 811 → subcarpeta
    local url="https://ftp.sra.ebi.ac.uk/vol1/fastq/${prefix}/00${suffix}/${srr}/${base}"
    echo ">>> Descargando $base desde $url ..."
    aria2c -x 3 -s 3 --retry-wait=15 --max-tries=3 -d "$(dirname "$file")" "$url"
    # Verificar integridad de nuevo
    if gunzip -t "$file" 2>/dev/null; then
        echo "✅ $file OK tras descarga"
    else
        echo "❌ $file sigue corrupto tras reintento" >&2
    fi
}

export -f re_download

echo ">>> Comprobando integridad de archivos .fastq.gz en $DIR ..."

# Recorre todos los fastq.gz de a cuatro
find "$DIR" -name "*.fastq.gz" | parallel -j 3 '
    if gunzip -t {} 2>/dev/null; then
        echo "✅ {} OK"
    else
        echo "❌ {} CORRUPTO"
        re_download {}
    fi
'
echo ">>> Comprobación completada."
