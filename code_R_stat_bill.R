#importation de la library
library(readODS)
#indication du chemin des données dans la machine
data <- read_ods(path = "C:/Users/Vitre/Desktop/Cours/S2/BILL/Nombres indels P65.ods", sheet = 1)
#Statistique descriptiv
mean(data$INS)
mean(data$DEL)
mean(data$INDELS)
sd(data$INS)
sd(data$DEL)
sd(data$INDELS)

#comparaison des féquences // tableau de fréquence en barplot
#Réorganisation des données
data <- read.table(header=TRUE, text='
Echantillon INS DEL
P65-1 29 29
P65-2 37 32
P65-3 39 37
P65-4 31 35
P65-5 48 46
P65-6 29 37
P65-7 36 34
P65-8 56 56
P65-9 56 56
P65-10 47 52
')

# Utilisez barplot pour créer des barres groupées et stockez les positions des barres
bar_positions <- barplot(t(as.matrix(data[, c("INS", "DEL")])),
                         beside = TRUE, # Mettre les barres côte à côte
                         col = c("blue", "red"),
                         legend.text = c("Insertions", "Deletions"),
                         args.legend = list(x = "topright"),
                         names.arg = data$Echantillon,
                         las = 1, # Orientation des étiquettes de l'axe des abscisses
                         xlab = "Echantillons",
                         ylab = "Fréquence",
                         main = "Barplot des Insertions et Déletions",
                         ylim = c(0, 80) # Définir les limites de l'axe y
)

# Ajouter des étiquettes de fréquence au-dessus des barres pour Insertions
text(x = bar_positions[1,] - 0.225, y = data$INS + 2, labels = data$INS, cex = 0.7)

# Ajouter des étiquettes de fréquence au-dessus des barres pour Deletions
text(x = bar_positions[2,] + 0.25, y = data$DEL + 2, labels = data$DEL, cex = 0.7)

#Test statistique comparaison des moyennes Del et Ins pas Student 
# Analyse descriptive
mean_ins <- mean(data$INS)
mean_del <- mean(data$DEL)
sd_ins <- sd(data$INS)
sd_del <- sd(data$DEL)

# Test de normalité
shapiro.test(data$INS)
shapiro.test(data$DEL)

# Test statistique
# Si les données sont normalement distribuées :
t.test(data$INS, data$DEL)

# Visualisation
boxplot(data$INS, data$DEL, names = c("Insertions", "Deletions"), main = "Insertions vs Deletions")