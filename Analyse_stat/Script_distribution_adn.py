import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

# Charger les données depuis un fichier Excel
fichier_excel = r'C:\Users\Vitre\Desktop\Cours\S2\BILL\Quantification.xlsx'  # Remplacez par le chemin de votre fichier
df = pd.read_excel(fichier_excel)

# Supposons que vos données sont dans la colonne 'Concentration'
concentrations = df['Concentration']  # Remplacez 'Concentration' par le nom réel de votre colonne

# Calculer la moyenne et l'écart-type des données
moyenne = np.mean(concentrations)
ecart_type = np.std(concentrations)

# Générer des points pour la courbe de distribution normale
valeurs_x = np.linspace(min(concentrations), max(concentrations), 100)
valeurs_y = norm.pdf(valeurs_x, moyenne, ecart_type)

# Créer le graphique
plt.figure(figsize=(10, 6))
plt.plot(valeurs_x, valeurs_y, label='Distribution Normale')
plt.hist(concentrations, bins=20, density=True, alpha=0.6, color='g', label='Données Réelles')
plt.title('Distribution Normale des Concentrations d\'ADN')
plt.xlabel('Concentration')
plt.ylabel('Densité')
plt.legend()
plt.show()