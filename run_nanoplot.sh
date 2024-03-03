#!/bin/bash

# Définir le nombre de threads à utiliser
number_of_threads=4 # Modifiez cette valeur en fonction de vos besoins

# Créer un répertoire pour les résultats de NanoPlot
mkdir -p NanoPlot_results_combined

# Boucle sur chaque fichier FASTQ dans le répertoire courant
for fastq_file in *.fastq; do
    # Exécuter NanoPlot sur le fichier
    NanoPlot --fastq "$fastq_file" -o NanoPlot_results_combined/"$(basename "$fastq_file" .fastq)"_NanoPlot -t "$number_of_threads" -f pdf
done