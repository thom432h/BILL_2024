Importation de la bibliothèque : Utilise readODS pour lire des fichiers ODS.
Lecture des données : Charge les données depuis un fichier ODS spécifique situé sur l'ordinateur de l'utilisateur.
Statistiques descriptives : Calcule la moyenne et l'écart type pour les insertions, les délétions, et les indels totaux dans les données.
Réorganisation et visualisation des données : Réorganise les données dans un nouveau format et crée un barplot pour comparer les fréquences des insertions et des délétions entre différents échantillons.
Basé sur cette analyse, je vais rédiger un README qui décrit le but du script, comment l'exécuter, et les dépendances nécessaires. ​​

README pour le Script d'Analyse Statistique des Données Génétiques
Objectif du Script
Ce script R est conçu pour effectuer une analyse statistique de données génétiques, spécifiquement les insertions (INS) et les délétions (DEL) dans des séquences d'ADN. Il lit les données depuis un fichier ODS, calcule des statistiques descriptives (moyenne et écart type) pour les insertions, les délétions, et les indels totaux, et visualise ces données sous forme de barplots groupés pour faciliter la comparaison entre différents échantillons.

Prérequis
R et RStudio installés sur votre ordinateur.
Installation du package readODS. Vous pouvez l'installer en exécutant install.packages("readODS") dans la console R.
Données
Le script est configuré pour lire un fichier de données spécifique nommé Nombres indels P65.ods situé sur le bureau de l'utilisateur. Assurez-vous que votre fichier de données est correctement nommé et placé, ou modifiez le chemin d'accès dans le script pour correspondre à l'emplacement de votre fichier.

Exécution du Script
Ouvrez RStudio et chargez le script.
Assurez-vous que le package readODS est installé.
Exécutez le script. Les résultats des analyses statistiques s'afficheront dans la console, et les barplots seront générés automatiquement.
Structure du Script
Importation de la bibliothèque : Le script commence par charger la bibliothèque readODS.
Lecture des données : Les données sont lues à partir d'un fichier ODS spécifié.
Analyse statistique : Calcul des moyennes et des écarts types pour les insertions, les délétions, et les indels totaux.
Visualisation : Les données sont réorganisées et visualisées à l'aide de barplots groupés, montrant les comparaisons entre les insertions et les délétions pour différents échantillons.
