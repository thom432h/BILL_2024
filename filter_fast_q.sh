#!/bin/bash

# Get the directory where the script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

# Directory where your fastq files are located (assuming it's the same as the script directory)
READS_DIR="${SCRIPT_DIR}"

# Directory where you want to save the filtered fastq files (creating a new directory here)
FILTERED_DIR="${SCRIPT_DIR}/filtered"

# Create the filtered directory if it doesn't exist
mkdir -p "${FILTERED_DIR}"

# Quality score threshold
QUALITY_THRESHOLD=10
# Minimum read length
MIN_LENGTH=1000

# Loop through each fastq file in the directory
for FILE in ${READS_DIR}/*.fastq; do
  # Extract the base name of the file without the path and extension
  BASENAME=$(basename "${FILE}" .fastq)

  # Filter the reads using NanoFilt and save as a new fastq file
  NanoFilt -q ${QUALITY_THRESHOLD} -l ${MIN_LENGTH} < "${FILE}" > "${FILTERED_DIR}/${BASENAME}_filtered.fastq"

  echo "Filtering completed for ${BASENAME}"
done

echo "All files have been filtered."