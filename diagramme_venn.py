import pandas as pd
from venny4py.venny4py import venny4py
import matplotlib.pyplot as plt

# Fonction pour charger les données et filtrer pour les variants communs
def charger_et_filtrer(fichier):
    df = pd.read_excel(fichier)
    return df

# Chemins vers les fichiers Excel pour chaque passage
chemins_fichiers = [
    'c:/Users/Vitre/Desktop/Cours/S2/BILL/Variant commun/p15.xlsx',
    'c:/Users/Vitre/Desktop/Cours/S2/BILL/Variant commun/p30.xlsx',
    'c:/Users/Vitre/Desktop/Cours/S2/BILL/Variant commun/p50.xlsx',
    'c:/Users/Vitre/Desktop/Cours/S2/BILL/Variant commun/p65.xlsx'
]

# Charger et filtrer les données pour chaque passage
donnees = [charger_et_filtrer(fichier) for fichier in chemins_fichiers]

# Initialiser les variants communs avec les données du premier fichier
variants_communs = donnees[0][['CHROM', 'POS', 'REF', 'ALT']]

# Fusionner les données en utilisant des suffixes uniques pour éviter les conflits
for i, donnee in enumerate(donnees[1:], start=1):
    variants_communs = pd.merge(variants_communs, donnee[['CHROM', 'POS', 'REF', 'ALT']], 
                                how='inner', on=['CHROM', 'POS', 'REF', 'ALT'],
                                suffixes=(False, f'_dup{i}'))

# Extraction des identifiants uniques (POS) pour chaque passage
ids_p15 = set(donnees[0]['POS'].unique())
ids_p30 = set(donnees[1]['POS'].unique())
ids_p50 = set(donnees[2]['POS'].unique())
ids_p65 = set(donnees[3]['POS'].unique())

# Préparation des ensembles pour le diagramme de Venn à 4 variables
sets = {
    'P15': set(donnees[0].apply(lambda row: (row['POS'], row['REF'], row['ALT']), axis=1)),
    'P30': set(donnees[1].apply(lambda row: (row['POS'], row['REF'], row['ALT']), axis=1)),
    'P50': set(donnees[2].apply(lambda row: (row['POS'], row['REF'], row['ALT']), axis=1)),
    'P65': set(donnees[3].apply(lambda row: (row['POS'], row['REF'], row['ALT']), axis=1)),
}


# Création du diagramme de Venn à 4 variables
venny4py(sets=sets)
plt.show()