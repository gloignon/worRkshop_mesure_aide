# Atelier d'intro à l'analyse quantitative de données d'éducation
# Vignette: Évaluation d'une mesure d'aide par méthode de pairage
#           (scores de propension)
#
# Par G. Loignon
# loignon.guillaume@uqam.ca
#
# Printemps 2023
#
# Note: le fichier utilisé est plus volumineux (N = 1181) afin que
# l'algo de pairage puisse trouver des élèves qui ont des
# caractéristiques similaires à notre groupe expérimental (mesure 2)

library(tidyverse)

df_mega_simul <- read_csv("data/mega_simul_1.csv")  # données simulées 

# créer des variables dichotomiques (dummy) pour le statut
df_mega_simul$est_mesure_1 <- df_mega_simul$statut == "mesure_1"
df_mega_simul$est_mesure_2 <- df_mega_simul$statut == "mesure_2"

# petites vérifications
table(df_mega_simul$statut)
table(df_mega_simul$est_mesure_1)
table(df_mega_simul$est_mesure_2)

# matching pour la mesure 2 ----
match_mesure_2 <-
  MatchIt::matchit(
    data = df_mega_simul,
    est_mesure_2 ~ mgs + age + genre + situation_handicap,
    exact = ~ est_mesure_1,  # variable qui doit être pairée exactement, dans ce cas on ne veut pas que le "match" soit avec des élèves du groupe mesure_1
    ratio = 2 # va pairer chaque élève du groupe mesure_2 à deux élèves du groupe pas_mesure
  )
match_mesure_2

# va reconstituer un data frame pairé à partir du résultat de matchit
  # on utilise ici le score de propension pour aller chercher les élèves qui ressemblent le plus
  # aux élèves de notre groupe expérimental
df_match_mesure_2 <-
  MatchIt::get_matches(match_mesure_2, data = df_mega_simul, distance = "prop.score")

table(df_match_mesure_2$est_mesure_2)  # vérification: FALSE devrait être un ratio de TRUE
                                       # tel que spécifié dans le paramètre ratio de 
                                       # la fonction matchit()

# validation du pairage
ggplot(data = df_match_mesure_2, aes(x = est_mesure_2, y = mgs)) +
  geom_boxplot(notch = T) +
  labs(
    title = "Comparaison de la MGS par groupe",
    x = "Groupe",
    y = "Moy. au secondaire (%)") +
  scale_x_discrete(labels = c("FALSE" = "Témoin", "TRUE" = "Intervention"))+
  ggpubr::stat_compare_means(method = "t.test", label.x = 1.5) +
  ggpubr::theme_pubclean(base_size = 16)

# évaluation de la mesure d'aide 2
ggplot(data = df_match_mesure_2, aes(x = est_mesure_2, y = note_francais)) +
  geom_boxplot(notch = T) +
  labs(
    title = "Effet de la mesure d'aide #2",
    x = "Groupe",
    y = "Note (%)") +
  scale_x_discrete(labels = c("FALSE" = "Témoin", "TRUE" = "Intervention")) +
  ggpubr::stat_compare_means(method = "t.test", label.x = 1) +
  ggpubr::theme_pubclean(base_size = 16)
