# Atelier d'intro à l'analyse quantitative de données d'éducation
# Vignette: Évaluation d'une mesure d'aide avec le value added model
#
# Par G. Loignon
# loignon.guillaume@uqam.ca
#
# Printemps 2023
#

library(tidyverse)  # utilisé pour des opérations courantes sur les données

df_simul <- read_csv("data/simul_mesure_cegep_1.csv")
df_simul$statut <- as.factor(df_simul$statut) 

# graphique exploratoire
df_simul %>% ggplot(aes(x = mgs, y = note_francais)) +
  geom_point(aes(x = mgs, y = note_francais, color = statut), alpha = .7) +
  geom_smooth(color = "darkgrey", method = "lm") +
  scale_color_discrete(
    labels = c(
      "mesure_1" = "Mesure d'aide 1",
      "mesure_2" = "Mesure d'aide 2",
      "pas_mesure" = "Pas de mesure d'aide"
    )
  ) +
  scale_shape_discrete(labels = c("TRUE" = "Oui", "FALSE" = "Non")) +
  labs(
    y = "Note en français (%)",
    x = "Moy. au secondaire (%)",
    title = "Note en français selon la MGS et le statut",
    subtitle = "Les élèves ayant eu la mesure 2 semblent mieux performer",
    color = "Statut",
    shape = "Situation de handicap"
  )

# modéliser la note en français

  # j'utilise les facteurs d'intérêt, sauf le statut
lm_1 <- lm(note_francais ~ mgs + situation_handicap + genre + age, data = df_simul)
summary(lm_1)  # j'ai un effet de mgs

  # prédiction d'une note à partir du modèle
df_simul$pred <- predict(lm_1)

# comparer la note prédite et la note réelle
df_simul$diff <- df_simul$note_francais - df_simul$pred
  # Ainsi une difference positive indique que la note obtenue était plus élevée que la note
  # prédite pas le modèle. Ces différences sont parfois désignées comme résidus ou erreurs 
  # du modèle.

# histogramme des différences
hist(df_simul$diff)  # à peu près réparti autour de 0
summary(df_simul$diff)

# visualiser les différences par statut
df_simul %>% ggplot(aes(x = statut, y = diff)) +
  geom_boxplot() +
  geom_abline(intercept = 0, slope = 0, color = "grey")

  # on dirait que mes mesure_2 sont anormalement élevés!

# est-ce statistiquement significatif ?
rstatix::anova_test(data = df_simul, diff ~ statut)  # oui, p = 0.046
rstatix::pairwise_t_test(data = df_simul, diff ~ statut)  # mesure 2 diffère de pas de mesure

# et en points à donne quoi ?
df_simul %>% group_by(statut) %>% 
  summarise(diff_m = mean(diff))  # Différence moyenne d'env. 5 points de pourcentage
                                  # relativement au modèle, ou env. 6 points de plus que
                                  # le groupe n'ayant pas eu de mesure d'aide.

# et la taille d'effet associée?
  # le d de Cohen est la différence de moyennes divisée par l'écart-type.
  # Je sais que ma différence est d'environ 6 points.
6 / sd(df_simul$note_francais)  # d = 0.4
effectsize::interpret_cohens_d(0.4)  # pour interpréter

# Nous avons utilisé une appproche de type de modélisation de type "valeur ajoutée"
# (value added modeling) pour évaluer l'effet de deux mesures d'aide sur la note
# en français, en contrôlant pour l'effet de variables confondantes.
# Nous avons utilisé une régression linéaire multivariable dont la variable dépendante
# était la note en français, et les variables indépendantes étaient la moyenne au
# secondaire, l'âge, le genre et la situation de handicap. Nous avons ensuite analysé
# les résidus du modèle (différence entre la note réelle et la note prédite) selon
# le statut de l'élève (mesure d'aide 1, mesure d'aide 2 ou pas de mesure d'aide). 
# Les résultats indiquent que, après avoir contrôlé pour l'effet des variables
# confondantes, les élèves ayant reçu la mesure d'aide 2 avaient une note
# en français qui était généralement supérieure aux élèves n'ayant pas reçu de mesure
# d'aide. La différence était d'environ 6 points de pourcentage, l'écart type de la note
# en français étant d'env. 14,5, cet avantage correspond à une taille d'effet d'env. 0,4
# ce qui est un effet de petite taille selon les barèmes proposés par Cohen (1988), 
# et un effet de taille moyenne selon les barèmes plus spécifiques à l'éducation
# suggérés par Hattie (2009).

