library(readxl)
data <- read_excel("C:/Users/Vitre/Desktop/Cours/S2/BILL/Nombre_variation_bill.xlsx")
data$Passage <- as.numeric(sub("P", "", sub("-.*", "", data$Échantillons)))
library(dplyr)
donnee <- data %>%
  mutate(Passage = as.numeric(gsub("P", "", gsub("-.*", "", Échantillons)))) %>%
  group_by(Passage) %>%
  summarise(
    Moyenne_des_variations = mean(Nombre_de_variations, na.rm = TRUE),
    Écart_type_des_variations = sd(Nombre_de_variations, na.rm = TRUE)
  )
print(donnee)

donnee_ins <- data %>%
  mutate(Passage = as.numeric(gsub("P", "", gsub("-.*", "", Échantillons)))) %>%
  group_by(Passage) %>%
  summarise(
    Moyenne_des_insertions = mean(Nombre_insertions, na.rm = TRUE),
    Écart_type_des_insertions = sd(Nombre_insertions, na.rm = TRUE)
  )
print(donnee_ins)

donnee_del <- data %>%
  mutate(Passage = as.numeric(gsub("P", "", gsub("-.*", "", Échantillons)))) %>%
  group_by(Passage) %>%
  summarise(
    Moyenne_des_délétions = mean(Nombre_deletions, na.rm = TRUE),
    Écart_type_des_délétions = sd(Nombre_deletions, na.rm = TRUE)
  )
print(donnee_del)

install.packages("ggplot2")
library(ggplot2)
ggplot(donnee, aes(x = Passage, y = Moyenne_des_variations)) +
  geom_line() +
  geom_errorbar(aes(ymin = Moyenne_des_variations - Écart_type_des_variations, 
                    ymax = Moyenne_des_variations + Écart_type_des_variations), 
                width = 0.1) +
  scale_x_continuous(breaks = c(15, 30, 50, 65), labels = c("P15", "P30", "P50", "P65")) +
  labs(title = "Moyenne des variations par passage",
       x = "Passage",
       y = "Moyenne des variations") +
  theme_minimal()

install.packages("tidyverse")
library(tidyverse)

# Ajoutons une colonne Type à chaque dataframe
donnee_ins <- donnee_ins %>%
  mutate(Type = 'Insertion',
         Moyenne = Moyenne_des_insertions,
         Ecart_type = Écart_type_des_insertions)

donnee_del <- donnee_del %>%
  mutate(Type = 'Deletion',
         Moyenne = Moyenne_des_délétions,
         Ecart_type = Écart_type_des_délétions)

# Fusion des dataframes
donnees_longues <- bind_rows(donnee_ins, donnee_del) %>%
  select(Passage, Type, Moyenne, Ecart_type)

# Création du graphique
ggplot(donnees_longues, aes(x = Passage, y = Moyenne, color = Type)) +
  geom_line() +
  geom_errorbar(aes(ymin = Moyenne - Ecart_type, ymax = Moyenne + Ecart_type),
                width = 0.1, position = position_dodge(0.1)) +
  scale_x_continuous(breaks = c(15, 30, 50, 65), labels = c("P15", "P30", "P50", "P65")) +
  labs(title = "Moyenne des insertions et délétions par passage",
       x = "Passage",
       y = "Moyenne des variations") +
  theme_minimal() +
  scale_color_manual(values = c("Insertion" = "blue", "Deletion" = "orange"))

# Lire les données (remplacer cela avec le chemin vers votre fichier de données)
data <- read.csv("/chemin/vers/votre/fichier.csv")

# Calculer les moyennes et les écarts-types pour chaque condition
resultats <- data %>%
  group_by(Température) %>%
  summarise(
    Moyenne_des_variations = mean(Nombre_de_variations, na.rm = TRUE),
    Ecart_type_des_variations = sd(Nombre_de_variations, na.rm = TRUE)
  ) %>%
  mutate(Condition = if_else(Température == "F", "Froid", "Chaud"))

# Créer le graphique en barres avec barres d'erreur
ggplot(resultats, aes(x = Condition, y = Moyenne_des_variations, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.5) +
  geom_errorbar(aes(ymin = Moyenne_des_variations - Ecart_type_des_variations, 
                    ymax = Moyenne_des_variations + Ecart_type_des_variations),
                width = 0.25, position = position_dodge(0.5)) +
  labs(title = "Moyenne des variations en fonction du stress chaud / froid",
       x = "Condition",
       y = "Moyenne des variations") +
  theme_minimal() +
  scale_fill_manual(values = c("Froid" = "blue", "Chaud" = "red"))

# Afficher le graphique
ggsave("Moyenne_Variations_Chaud_Froid.png", width = 10, height = 6)


resultats_ins <- data %>%
  group_by(Temperature) %>%
  summarise(
    Moyenne_des_insertions = mean(Nombre_insertions, na.rm = TRUE),
    Ecart_type_des_insertions = sd(Nombre_insertions, na.rm = TRUE)
  ) %>%
  mutate(Condition = if_else(Temperature == "F", "Froid", "Chaud"))

# Créer le graphique en barres avec barres d'erreur
ggplot(resultats_ins, aes(x = Condition, y = Moyenne_des_insertions, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.5) +
  geom_errorbar(aes(ymin = Moyenne_des_insertions - Ecart_type_des_insertions, 
                    ymax = Moyenne_des_insertions + Ecart_type_des_insertions),
                width = 0.25, position = position_dodge(0.5)) +
  labs(title = "Moyenne des insertions en fonction du stress chaud / froid",
       x = "Condition",
       y = "Moyenne des insertions") +
  theme_minimal() +
  scale_fill_manual(values = c("Froid" = "blue", "Chaud" = "red"))

# Afficher le graphique
ggsave("Moyenne_des_instertions_Chaud_Froid.png", width = 10, height = 6)

resultats_del <- data %>%
  group_by(Temperature) %>%
  summarise(
    Moyenne_des_deletions = mean(Nombre_deletions, na.rm = TRUE),
    Ecart_type_des_deletions = sd(Nombre_deletions, na.rm = TRUE)
  ) %>%
  mutate(Condition = if_else(Temperature == "F", "Froid", "Chaud"))

# Créer le graphique en barres avec barres d'erreur
ggplot(resultats_del, aes(x = Condition, y = Moyenne_des_deletions, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.5) +
  geom_errorbar(aes(ymin = Moyenne_des_deletions - Ecart_type_des_deletions, 
                    ymax = Moyenne_des_deletions + Ecart_type_des_deletions),
                width = 0.25, position = position_dodge(0.5)) +
  labs(title = "Moyenne des délétions en fonction du stress chaud / froid",
       x = "Condition",
       y = "Moyenne des délétions") +
  theme_minimal() +
  scale_fill_manual(values = c("Froid" = "blue", "Chaud" = "red"))

# Afficher le graphique
ggsave("Moyenne_des_délétions_Chaud_Froid.png", width = 10, height = 6)




data_long <- data %>%
  gather(key = "Type", value = "Nombre", -Échantillons, -Temperature, -Passage)

# Créer le graphique
ggplot(data_long, aes(x = Passage, y = Nombre, color = Type, group = Type)) +
  geom_line() +
  geom_point() +
  labs(title = "Nombre de variations / insertions / délétions par passage",
       x = "Passage",
       y = "Nombre") +
  theme_minimal() +
  scale_color_manual(values = c("Nombre_de_variations" = "blue", 
                                "Nombre_insertions" = "green", 
                                "Nombre_deletions" = "red"))



# Calculer la somme des variations, insertions, et délétions pour chaque passage
data_somme <- data %>%
  group_by(Passage) %>%
  summarise(
    Somme_variations = sum(Nombre_de_variations),
    Somme_insertions = sum(Nombre_insertions),
    Somme_deletions = sum(Nombre_deletions)
  ) %>%
  pivot_longer(
    cols = starts_with("Somme_"),
    names_to = "Type",
    values_to = "Total"
  )

# Créer l'histogramme avec des étiquettes de passage spécifiques
ggplot(data_somme, aes(x = Passage, y = Total, fill = Type)) +
  geom_col(position = "dodge") +
  scale_x_continuous(breaks = c(15, 30, 50, 65), 
                     labels = c("P15", "P30", "P50", "P65")) +
  scale_fill_manual(values = c("Somme_variations" = "blue", 
                               "Somme_insertions" = "green", 
                               "Somme_deletions" = "red")) +
  labs(title = "Somme des variations / insertions / délétions par passage",
       x = "Passage",
       y = "Total") +
  theme_minimal()

analyse_desc <- data %>%
  summarise(
    Moyenne = mean({{nom_colonne}}, na.rm = TRUE),
    Médiane = median({{nom_colonne}}, na.rm = TRUE),
    Mode = as.numeric(names(sort(table({{nom_colonne}}), decreasing = TRUE)[1])),
    Écart_type = sd({{nom_colonne}}, na.rm = TRUE),
    Variance = var({{nom_colonne}}, na.rm = TRUE),
    Q1 = quantile({{nom_colonne}}, 0.25, na.rm = TRUE),
    Q3 = quantile({{nom_colonne}}, 0.75, na.rm = TRUE),
    Étendue = max({{nom_colonne}}, na.rm = TRUE) - min({{nom_colonne}}, na.rm = TRUE),
    Skewness = skewness({{nom_colonne}}, na.rm = TRUE),
    Kurtosis = kurtosis({{nom_colonne}}, na.rm = TRUE),
    Coef_variation = sd({{nom_colonne}}, na.rm = TRUE) / mean({{nom_colonne}}, na.rm = TRUE)
  )

# Afficher les résultats
print(analyse_desc)


# Test de Shapiro-Wilk pour la normalité des variations
shapiro_variations <- data %>%
  group_by(Passage) %>%
  summarise(p_value_variations = shapiro.test(Nombre_de_variations)$p.value)

# Test de Shapiro-Wilk pour la normalité des insertions
shapiro_insertions <- data %>%
  group_by(Passage) %>%
  summarise(p_value_insertions = shapiro.test(Nombre_insertions)$p.value)

# Test de Shapiro-Wilk pour la normalité des délétions
shapiro_deletions <- data %>%
  group_by(Passage) %>%
  summarise(p_value_deletions = shapiro.test(Nombre_deletions)$p.value)

# Afficher les résultats
print(shapiro_variations)
print(shapiro_insertions)
print(shapiro_deletions)

library(dplyr)

table(data$Temperature)

data$Temperature <- factor(data$Temperature, levels = c("F", "C"))

test_variations <- wilcox.test(na.omit(data$Nombre_de_variations[data$Temperature == "F"]),
                               na.omit(data$Nombre_de_variations[data$Temperature == "C"]),
                               exact = FALSE)

test_insertions <- wilcox.test(na.omit(data$Nombre_insertions[data$Temperature == "F"]),
                               na.omit(data$Nombre_insertions[data$Temperature == "C"]),
                               exact = FALSE)

test_deletions <- wilcox.test(na.omit(data$Nombre_deletions[data$Temperature == "F"]),
                              na.omit(data$Nombre_deletions[data$Temperature == "C"]),
                              exact = FALSE)


print(test_variations)
print(test_insertions)
print(test_deletions)

# Pour comparer les médianes entre les insertions et les délétions
test_insertions_vs_deletions <- wilcox.test(data$Nombre_insertions, 
                                            data$Nombre_deletions,
                                            exact = FALSE)

# Afficher les résultats
print(test_insertions_vs_deletions)


passages <- unique(data$Passage)

# Initialiser un vecteur pour stocker les résultats des p-values
p_values <- numeric(length(passages))


for (i in 1:length(passages)) {
  passage_data <- filter(data, Passage == passages[i])
  
  test_result <- wilcox.test(passage_data$Nombre_insertions, 
                             passage_data$Nombre_deletions,
                             exact = FALSE)
  
  p_values[i] <- test_result$p.value
}

# Créer un dataframe pour afficher les passages et les p-values correspondantes
resultats_tests <- data.frame(Passage = passages, P_value = p_values)

# Afficher les résultats
print(resultats_tests)



# Liste unique de passages
passages <- unique(data$Passage)

# Initialiser une liste pour stocker les résultats
results_list <- list()

# Boucle pour effectuer le test de Mann-Whitney pour chaque type de mutation par passage
for (passage in passages) {
  for (mutation_type in c("Nombre_de_variations", "Nombre_insertions", "Nombre_deletions")) {
    
    # Filtrer les données par passage et par type de mutation
    passage_data <- filter(data, Passage == passage)
    
    # Séparer les données en groupes chaud et froid
    chaud <- passage_data %>% filter(Temperature == "C") %>% pull(!!sym(mutation_type))
    froid <- passage_data %>% filter(Temperature == "F") %>% pull(!!sym(mutation_type))
    
    # Effectuer le test de Mann-Whitney
    test_result <- wilcox.test(chaud, froid, exact = FALSE)
    
    # Stocker les résultats
    results_list[[paste(passage, mutation_type, sep = "_")]] <- test_result$p.value
  }
}

# Transformer la liste des résultats en dataframe pour une meilleure lisibilité
results_df <- do.call(rbind, lapply(names(results_list), function(x) {
  data.frame(Passage_Mutation = x, P_value = results_list[[x]], row.names = NULL)
}))
results_df$Passage <- gsub("_.*", "", results_df$Passage_Mutation)
results_df$Mutation_Type <- gsub(".*_", "", results_df$Passage_Mutation)
results_df <- results_df[ , -1]

# Afficher les résultats
print(results_df)


data <- data %>%
  mutate(Total_mutations = Nombre_de_variations + Nombre_insertions + Nombre_deletions)

# Liste unique de passages
passages <- unique(data$Passage)

# Initialiser un dataframe pour stocker les résultats des p-values
results_df <- data.frame(Passage = integer(), P_value = numeric())

# Boucle pour effectuer le test de Mann-Whitney par passage
for (passage in passages) {
  # Filtrer les données pour le passage courant
  passage_data <- filter(data, Passage == passage)
  
  # Effectuer le test de Mann-Whitney U
  test_result <- wilcox.test(Total_mutations ~ Temperature, data = passage_data, exact = FALSE)
  
  # Ajouter le résultat au dataframe
  results_df <- rbind(results_df, data.frame(Passage = passage, P_value = test_result$p.value))
}

# Afficher les résultats
print(results_df)
update.packages(ask = FALSE, checkBuilt = TRUE)

install.packages("lme4")
library(lme4)
install.packages("Matrix")

# Charger le package nécessaire
if (!requireNamespace("lme4", quietly = TRUE)) install.packages("lme4")
library(lme4)

# Votre dataframe s'appelle 'data'. Nous allons créer une nouvelle colonne pour identifier la lignée
data$Lignee <- sub(".*-", "", data$Échantillons)  # cela retire tout ce qui précède et inclut le tiret

# Convertir les colonnes nécessaires en facteurs
data$Passage <- as.factor(data$Passage)
data$Temperature <- as.factor(data$Temperature)
data$Lignee <- as.factor(data$Lignee)

modele <- lmer(Total_mutations ~ Passage + Temperature + (1|Lignee), data = data)
summary(modele)


# Afficher le résumé du modèle
summary(modele)

# Assurez-vous d'avoir le package 'lmerTest' installé pour obtenir des p-values
if (!requireNamespace("lmerTest", quietly = TRUE)) install.packages("lmerTest")
library(lmerTest)

# Votre modèle est déjà ajusté avec 'lmer'
# Utilisez 'anova' du package 'lmerTest' pour obtenir des p-values pour les effets fixes
anova(modele)


friedman_data <- reshape2::melt(data, id.vars = c("Lignee", "Temperature"),
                                measure.vars = c("P15", "P30", "P50", "P65"))

# Application du test de Friedman
friedman_result <- with(friedman_data, friedman.test(value ~ variable | Lignee))
print(friedman_result)

install.packages("reshape2")  # pour la fonction melt()
library(reshape2)
library(ggplot2)
library(reshape2)
library(ggplot2)

# Transformer le dataframe pour avoir des variables en colonnes et des valeurs en lignes
data_long <- melt(data, id.vars = c("Passage", "Temperature"),
                  measure.vars = c("Nombre_de_variations", "Nombre_insertions", "Nombre_deletions"),
                  variable.name = "Mutation_Type", value.name = "Count")

# Créer des boxplots pour les données de mutations
ggplot(data_long, aes(x = Passage, y = Count, fill = Temperature)) + 
  geom_boxplot() +
  facet_wrap(~Mutation_Type, scales = "free_y") +
  labs(title = "Boxplot of Genetic Mutations Across Passages and Temperatures",
       x = "Passage",
       y = "Nombre de mutations") +
  theme_minimal()

# Créer des violin plots pour les mêmes données
ggplot(data_long, aes(x = Passage, y = Count, fill = Temperature)) + 
  geom_violin() +
  facet_wrap(~Mutation_Type, scales = "free_y") +
  labs(title = "Violin Plot of Genetic Mutations Across Passages and Temperatures",
       x = "Passage",
       y = "Nombre de mutations") +
  theme_minimal()

library(reshape2)
library(ggplot2)

# Transformer le dataframe pour avoir des variables en colonnes et des valeurs en lignes
data_long <- melt(data, id.vars = c("Passage", "Échantillons"), 
                  measure.vars = c("Nombre_de_variations", "Nombre_insertions", "Nombre_deletions"))


# Créer la heatmap avec une couleur différente pour chaque type de mutation
ggplot(data_long, aes(x = Passage, y = variable, fill = value)) + 
  geom_tile() +
  scale_fill_gradientn(colors = c("Yellow", "green", "pink", "Orange", "Red", "brown", "Purple", "Blue", "Black")) +
  labs(title = "Heatmap of Genetic Mutations Across Passages", x = "Passage", y = "Type de mutation", fill = "Nombre") +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 0.5))  # Pour une meilleure lisibilité des labels


library(ggplot2)
library(reshape2)

# Transformer le dataframe en format long pour les boxplots
data_long <- melt(data, id.vars = c("Passage", "Temperature"),
                  measure.vars = c("Nombre_de_variations", "Nombre_insertions", "Nombre_deletions"),
                  variable.name = "Type_de_mutation", value.name = "Nombre")

# Convertir 'Passage' en un facteur pour l'ordonner correctement dans le graphique
data_long$Passage <- factor(data_long$Passage, levels = c("15", "30", "50", "65"))

# Créer le boxplot
ggplot(data_long, aes(x = interaction(Passage, Type_de_mutation, lex.order = TRUE), y = Nombre, fill = Type_de_mutation)) + 
  geom_boxplot() +
  scale_x_discrete(labels = c("P15 Variations", "P15 Insertions", "P15 Deletions", "P30 Variations", "P30 Insertions", "P30 Deletions",
                              "P50 Variations", "P50 Insertions", "P50 Deletions", "P65 Variations", "P65 Insertions", "P65 Deletions")) +
  labs(title = "Répartition des mutations par passage", x = "Passage and Mutation Type", y = "Nombre de mutations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_fill_manual(values = c("Nombre_de_variations" = "red", "Nombre_insertions" = "green", "Nombre_deletions" = "blue"))



library(ggplot2)

# Assurez-vous que les données sont dans le format long approprié pour les graphiques à barres empilées
data_long <- reshape2::melt(data, id.vars = c("Passage"), 
                            measure.vars = c("Nombre_insertions", "Nombre_deletions"),
                            variable.name = "Type_de_mutation", value.name = "Nombre")

# Convertissez 'Passage' en facteur pour s'assurer qu'il est ordonné correctement dans le graphique
data_long$Passage <- factor(data_long$Passage, levels = c("15", "30", "50", "65"), labels = c("P15", "P30", "P50", "P65"))

# Créez le graphique à barres empilées
ggplot(data_long, aes(x = Passage, y = Nombre, fill = Type_de_mutation)) + 
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Nombre_insertions" = "green", "Nombre_deletions" = "red")) +
  labs(title = "Répartition des insertions et délétions par passage", 
       x = "Passage", 
       y = "Nombre total de mutations") +
  theme_minimal()


library(ggplot2)

# Si 'data' est votre dataframe, nous devons d'abord le mettre dans le format long pour les graphiques en aires
data_long <- reshape2::melt(data, id.vars = c("Passage"),
                            measure.vars = c("Nombre_insertions", "Nombre_deletions"),
                            variable.name = "Type_de_mutation", value.name = "Nombre")

# Convertir 'Passage' en un facteur ordonné pour s'assurer que les passages sont dans l'ordre correct
data_long$Passage <- factor(data_long$Passage, levels = c("15", "30", "50", "65"))

# Nous créons des couches séparées pour chaque type de mutation et les empilons les unes sur les autres
ggplot(data_long, aes(x = Passage, y = Nombre, fill = Type_de_mutation, group = Type_de_mutation)) +
  geom_area(position = 'stack', alpha = 0.5) +
  scale_fill_manual(values = c("Nombre_insertions" = "green", "Nombre_deletions" = "red")) +
  labs(title = "Évolution des mutations au fil des passages", 
       x = "Passage", 
       y = "Nombre total de mutations") +
  theme_minimal()
install.packages("ggraph")
install.packages("igraph")
library(igraph)
library(ggraph)
# Supposons que 'co_occurrence_matrix' est une matrice ou un dataframe où les lignes et les colonnes représentent les types de mutations, et les valeurs représentent la force de la co-occurrence entre ces mutations.

# Créer un graphe à partir de la matrice de co-occurrence
graph <- graph_from_adjacency_matrix(co_occurrence_matrix, mode = "undirected", weighted = TRUE, diag = FALSE)

# Visualiser le graphe réseau
ggraph(graph, layout = "fr") +
  geom_edge_link(aes(edge_width = weight), alpha = 0.5) +  # Utilisez 'weight' si vos liens ont des poids
  geom_node_point(size = 5, aes(color = name)) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_graph() +
  labs(title = "Réseau de co-occurrence des mutations")


# Supposons que votre dataframe 'data' a des colonnes pour différentes mutations, par exemple, 'Mutation_A', 'Mutation_B', etc.
# Vous souhaitez calculer combien de fois chaque paire de mutations se produit ensemble.

# Convertir les données en présence/absence (1 pour présence, 0 pour absence de mutation)
data_binary <- data
data_binary[data_binary > 0] <- 1

# Préparation des données pour la matrice de co-occurrence
# Convertissez les données en format binaire (0 pour absence de mutation, 1 pour présence)
data_binary <- as.data.frame(lapply(data[, c("Nombre_insertions", "Nombre_deletions")], function(x) as.integer(x > 0)))

# Calculer la matrice de co-occurrence
co_occurrence_matrix <- as.matrix(data_binary) %*% t(data_binary)

# Créez un objet graphe à partir de la matrice de co-occurrence
graph <- graph_from_adjacency_matrix(co_occurrence_matrix, mode = "undirected", weighted = TRUE, diag = FALSE)

# Nommez les nœuds selon les colonnes de votre matrice binaire (ou une autre méthode si nécessaire)
V(graph)$name <- colnames(data_binary)

# Visualisez le graphe réseau
ggraph(graph, layout = "fr") +
  geom_edge_link(aes(edge_width = weight), alpha = 0.5) +
  geom_node_point(size = 5, aes(color = name)) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_graph() +
  labs(title = "Réseau de co-occurrence des mutations")


# Créer une nouvelle colonne 'Passage' en extrayant les informations à partir des noms des échantillons
data$Passage <- sub("-.*", "", data$Échantillons)

# Convertir la colonne 'Passage' en facteur avec les niveaux appropriés
data$Passage <- factor(data$Passage, levels = c("P15", "P30", "P50", "P65"))

# Vérifier à nouveau la table des passages
table(data$Passage)


# Assurez-vous que les packages nécessaires sont installés et chargés
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

# Sélectionnez les données pour les passages P15 et P30
P15_data <- filter(data, Passage == "P15")$Nombre_de_variations
P30_data <- filter(data, Passage == "P30")$Nombre_de_variations

# Effectuez le test de Mann-Whitney U pour P15 vs P30
test_P15_P30 <- wilcox.test(P15_data, P30_data, exact = FALSE)

# Affichez les résultats du test
test_P15_P30

# Répétez cette procédure pour les autres paires de passages

# Assurez-vous que les packages nécessaires sont installés et chargés
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

# Sélectionnez les données pour les passages P15 et P30
P30_data <- filter(data, Passage == "P30")$Nombre_de_variations
P50_data <- filter(data, Passage == "P50")$Nombre_de_variations

# Effectuez le test de Mann-Whitney U pour P15 vs P30
test_P30_P50 <- wilcox.test(P30_data, P50_data, exact = FALSE)

# Affichez les résultats du test
test_P30_P50


# Sélectionnez les données pour les passages P15 et P30
P50_data <- filter(data, Passage == "P50")$Nombre_de_variations
P65_data <- filter(data, Passage == "P65")$Nombre_de_variations
# Effectuez le test de Mann-Whitney U pour P15 vs P30
test_P50_P65 <- wilcox.test(P50_data, P65_data, exact = FALSE)

# Affichez les résultats du test
test_P50_P65


# Sélectionnez les données pour les passages P15 et P30
P65_data <- filter(data, Passage == "P65")$Nombre_de_variations
P15_data <- filter(data, Passage == "P15")$Nombre_de_variations
# Effectuez le test de Mann-Whitney U pour P15 vs P30
test_P65_P15 <- wilcox.test(P65_data, P15_data, exact = FALSE)

# Affichez les résultats du test
test_P65_P15



# Installer le package e1071 si nécessaire
if (!require(e1071)) install.packages("e1071")

# Charger le package e1071
library(e1071)

# Votre code, après avoir chargé le package
stats_descriptives <- data %>%
  group_by(Passage) %>%
  summarise(
    Moyenne = mean(Nombre_de_variations, na.rm = TRUE),
    Ecart_Type = sd(Nombre_de_variations, na.rm = TRUE),
    Médiane = median(Nombre_de_variations, na.rm = TRUE),
    Premier_Quartile = quantile(Nombre_de_variations, 0.25, na.rm = TRUE),
    Troisieme_Quartile = quantile(Nombre_de_variations, 0.75, na.rm = TRUE),
    Variance = var(Nombre_de_variations, na.rm = TRUE),
    Skewness = skewness(Nombre_de_variations, na.rm = TRUE),
    Kurtosis = kurtosis(Nombre_de_variations, na.rm = TRUE)
  )

# Afficher les résultats
print(stats_descriptives)
table_stat_variation <- (stats_descriptives)




# Votre code, après avoir chargé le package
stats_descriptives <- data %>%
  group_by(Temperature) %>%
  summarise(
    Moyenne = mean(Nombre_insertions, na.rm = TRUE),
    Ecart_Type = sd(Nombre_insertions, na.rm = TRUE),
    Médiane = median(Nombre_insertions, na.rm = TRUE),
    Premier_Quartile = quantile(Nombre_de_variations, 0.25, na.rm = TRUE),
    Troisieme_Quartile = quantile(Nombre_insertions, 0.75, na.rm = TRUE),
    Variance = var(Nombre_insertions, na.rm = TRUE),
    Skewness = skewness(Nombre_insertions, na.rm = TRUE),
    Kurtosis = kurtosis(Nombre_insertions, na.rm = TRUE)
  )

# Afficher les résultats
print(stats_descriptives)
table_stat_insertions_temps <- (stats_descriptives)

