# Atelier d'intro à l'analyse quantitative de données d'éducation
# Vignette: Évaluation d'une mesure d'aide avec méthode des
#           moyennes marginales.
#
# Par G. Loignon
# loignon.guillaume@uqam.ca
#
# Printemps 2023
#

library(tidyverse)
library(marginaleffects)
options(scipen = 99)  # pour afficher les p-valeurs avec plus de décimales

df_simul <- read_csv("data/simul_mesure_cegep_1.csv")

# Modélisation de la note
lm_1 <- lm(note_francais ~ mgs + statut + age + genre + situation_handicap, data = df_simul)
marginalmeans(lm_1, variables = "statut", hypothesis = "pairwise")

# Nous avons utilisé la méthode des moyennes marginales pour investiguer l'effet de deux mesures 
  # d'aide sur la note en français. Les variables indépendantes étaient la moyenne au secondaire, 
  # l'âge, le genre, la situation de handicap (codée dichotomiquement), et le statut (mesure 1, 
  # mesure 2 ou pas de mesure d'aide).  
  # En maintenant constantes les autres variables, les élèves ayant reçu la mesure 2 différaient
  # des élèves n'ayant pas reçu de mesures, p < 0,001. La différence était d'environ 6,6 points
  # de pourcentage, avec un intervalle de confiance à 95% allant de 4,0 à 9,1. Par ailleurs,
  # il y avait une différence significative entre les élèves ayant reçu la mesure 1 et la mesure 2,
  # p < 0,001, ces derniers ayant une note en français plus élevée par environ 5,5 points de 
  # pourcentage, avec un intervalle de confiance à 95% de 2,9 à 8,1.

# et l'effet sur la réussite ?
  # Je peux tester le scénario où les élèves n'auraient pas eu le gain de 5,5 points

  # Calcul de la proportion de réussite de mes élèves ayant reçu la mesure 2
  # mais en leur retirant les 5,5 points attribuables à la mesure.
df_simul %>% 
  filter(statut == "mesure_2") %>% 
  summarise(prop_reussi_contrefactuel = sum((note_francais - 5.5) >= 60) / n(),
            prop_reussi_reel = sum(note_francais >= 60) / n()
            )  # augmente la réussite par environ 17%
