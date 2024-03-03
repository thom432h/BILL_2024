#!/bin/bash

# Optimized and resource-aware Flye assembler script for .fastq files

# Configuration
WORKING_DIR=$(pwd)
ASSEMBLY_DIR="${WORKING_DIR}/assembly_output"
GENOME_SIZE="300k"
ITERATIONS=3
THREADS=6

echo "Starting assembly process..."

# Create assembly output directory
mkdir -p "${ASSEMBLY_DIR}"

# Array to hold .fastq.gz files
FASTQ_FILES=(${WORKING_DIR}/*.fastq)
TOTAL_FILES=${#FASTQ_FILES[@]}

# Exit if no files found
if [ ${TOTAL_FILES} -eq 0 ]; then
    echo "No .fastq files found in the working directory. Exiting."
    exit 1
fi

# Main loop to process each .fastq.gz file
for READS_PATH in "${FASTQ_FILES[@]}"; do
    SAMPLE_NAME=$(basename "${READS_PATH}" .fastq)
    SAMPLE_ASSEMBLY_DIR="${ASSEMBLY_DIR}/${SAMPLE_NAME}"

    echo "Processing ${SAMPLE_NAME} (${READS_PATH})"

    mkdir -p "${SAMPLE_ASSEMBLY_DIR}" && \
    flye --nano-raw "${READS_PATH}" \
         --genome-size ${GENOME_SIZE} \
         --out-dir "${SAMPLE_ASSEMBLY_DIR}" \
         --threads ${THREADS} \
         --iterations ${ITERATIONS} \
         &> "${SAMPLE_ASSEMBLY_DIR}/flye.log"

    # Check for successful Flye run and output results
    if [ $? -eq 0 ]; then
        echo "Flye assembly completed successfully for sample ${SAMPLE_NAME}."
    else
        echo "Flye assembly failed for sample ${SAMPLE_NAME}. Check ${SAMPLE_ASSEMBLY_DIR}/flye.log for details."
        continue # Skip to next file in case of failure
    fi

    # Save metadata for this sample
    printf "Sample Name: %s\nReads File: %s\nGenome Size: %s\nThreads: %d\nIterations: %d\n" \
    "${SAMPLE_NAME}" "${READS_PATH}" "${GENOME_SIZE}" "${THREADS}" "${ITERATIONS}" > "${SAMPLE_ASSEMBLY_DIR}/metadata.txt"
done

echo "Assembly process completed."
