import pandas as pd

# Fonction pour charger les données d'un fichier Excel dans un DataFrame
def charger_donnees(fichier):
    return pd.read_excel(fichier)

# Charger les données des fichiers p15, p30, p50 et p65
chemins_fichiers = [
    'c:/Users/Vitre/Desktop/Cours/S2/BILL/Variant commun/p15.xlsx',
    'c:/Users/Vitre/Desktop/Cours/S2/BILL/Variant commun/p30.xlsx',
    'c:/Users/Vitre/Desktop/Cours/S2/BILL/Variant commun/p50.xlsx',
    'c:/Users/Vitre/Desktop/Cours/S2/BILL/Variant commun/p65.xlsx'
]

donnees = [charger_donnees(fichier) for fichier in chemins_fichiers]

# Trouver les variants communs entre tous les fichiers
variants_communs_tous = donnees[0]
for donnee in donnees[1:]:
    variants_communs_tous = pd.merge(variants_communs_tous, donnee[['CHROM', 'POS', 'REF', 'ALT']], how='inner', on=['CHROM', 'POS', 'REF', 'ALT'])

# Trouver les variants communs entre p30, p50 et p65
variants_communs_p30_p50_p65 = donnees[1]
for donnee in donnees[2:]:
    variants_communs_p30_p50_p65 = pd.merge(variants_communs_p30_p50_p65, donnee[['CHROM', 'POS', 'REF', 'ALT']], how='inner', on=['CHROM', 'POS', 'REF', 'ALT'])

# Pour chaque fichier p, trouver la référence (REF) et l'allèle alternatif (ALT) les plus longs
resultats_ref_alt_longues = pd.DataFrame(columns=['Fichier', 'REF_long', 'ALT_long'])
for i, fichier in enumerate(chemins_fichiers[1:]):  # Ignorer le fichier p15
    ref_long = donnees[i+1]['REF'].str.len().max()
    alt_long = donnees[i+1]['ALT'].str.len().max()
    resultats_ref_alt_longues.loc[i] = {'Fichier': fichier.split('/')[-1], 'REF_long': ref_long, 'ALT_long': alt_long}

# Enregistrer les résultats dans des fichiers Excel
chemin_fichier_sortie_communs_tous = 'c:/Users/Vitre/Desktop/Cours/S2/BILL/Variant commun/variants_communs_tous.xlsx'
variants_communs_tous.to_excel(chemin_fichier_sortie_communs_tous, index=False)

chemin_fichier_sortie_communs_p30_p50_p65 = 'c:/Users/Vitre/Desktop/Cours/S2/BILL/Variant commun/variants_communs_p30_p50_p65.xlsx'
variants_communs_p30_p50_p65.to_excel(chemin_fichier_sortie_communs_p30_p50_p65, index=False)

chemin_fichier_sortie_ref_alt_longues = 'c:/Users/Vitre/Desktop/Cours/S2/BILL/Variant commun/variantes_ref_alt_longues_par_p.xlsx'
resultats_ref_alt_longues.to_excel(chemin_fichier_sortie_ref_alt_longues, index=False)