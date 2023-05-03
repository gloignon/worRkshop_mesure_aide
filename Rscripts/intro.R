# Atelier d'intro à l'analyse quantitative de données d'éducation
# Vignette: Évaluation d'une mesure d'aide, méthode "cours d'intro aux stats"
#
# Par G. Loignon
# loignon.guillaume@uqam.ca
#
# Printemps 2023
#

library(tidyverse)  # utilisé pour des opérations courantes sur les données
options(scipen = 10)  # pour réduire l'affichage en notation scientifique

# ouverture du fichier de données
df_simul <- read_csv("data/simul_mesure_cegep_1.csv")

# démo - recodage simple de variables chaine de caractère vers facteur
df_simul$genre <- as.factor(df_simul$genre) 
df_simul$statut <- as.factor(df_simul$statut) 

# démo - skimr pour un aperçu rapide de nos statistiques descriptives
skimr::skim(df_simul)  # la notation package::fonction va aller chercher la fonction directement dans le package
                       # c'est utile pour indiquer d'où provient la fonction (dans ce cas, c'est la fonction
                       # skim du package skimr)

  # ça "skim" aussi en notation tidyverse
df_simul %>% skimr::skim()

  # plus avancé, skim par groupe
df_simul %>% 
  group_by(statut) %>%
  skimr::skim()

# Créer un tableau de statistiques descriptives ----
  # Démo de la fonction summarise du tidyverse
tab_sommaire <- df_simul %>% 
  group_by(statut) %>%
  summarise(n = length(note_francais),
            m_fr = mean(note_francais),
            sd_fr = sd(note_francais),
            mgs = mean(mgs),
            prop_reussi = mean(note_francais >= 60)
  )
tab_sommaire

# Comparaison de proportions ----
tab_comp <- table(statut = df_simul$statut, reussi = df_simul$note_francais >= 60)
tab_comp
rstatix::chisq_test(tab_comp)  # test du chi-2 sur l'ensemble des proportions
rstatix::pairwise_prop_test(tab_comp)  # tests de proportions pairés

# 44% des élèves ayant reçu la mesure 1 ont réussi leur cours de français, contre
# 72% des élèves ayant reçu la mesure 2 et 85% des élèves n'ayant pas eu de mesure
# d'aide. Un test du chi-2 a révélé que la proportions d'élèves ayant réussi leur cours
# de français variait significativement selon le parcours (pas de mesure d'aide,
# mesure d'aide 1, mesure d'aide 2), x2(2) = 28.3, p < 0,001.
# Les résultats de tests de proportion pairés montrent que l'écart entre mesure 1 et 
# mesure 2 était statistiquement significatif, p = 0,0196, de même que l'écart entre
# mesure 1 et pas de mesure, p < 0,001. Aucune différence significative n'a été 
# observée entre mesure 2 et pas de de mesure, p = 0,243.
 

# comparer plus de deux groupes avec ANOVA ----

# petit graphique pour voir de quoi il est question
df_simul %>% ggplot(aes(x = statut, y = mgs)) +
  geom_boxplot()

  # ANOVA de base
anova_1 <- aov(mgs ~ statut, data = df_simul)
summary(anova_1)
rstatix::anova_summary(anova_1)  # plus beau sommaire avec le package rstatix

# Note: R fait une ANOVA de type 1 par défaut. SPSS fait une ANOVA de type 3.
car::Anova(anova_1, type = 3)  # ... la fonction Anova du package car me donner les
                               # tests de type 3 si je lui demande (même résultats que SPSS)

  # variante plus avancée, va interpréter la taille d'effet et formater la p-valeur
df_simul %>% 
  rstatix::anova_test(mgs ~ statut, type = 3) %>%
  data.frame %>%
  mutate(interp_ges = effectsize::interpret_eta_squared(ges),
         p = format.pval(p, eps = .001))

# post-tests d'une ANOVA
df_simul %>% rstatix::pairwise_t_test(mgs ~ statut)  # bref, tout est différent


## ANOVA multifacteurs ----
  # avec interactions
anova_3 <-
  aov(note_francais ~ mgs * (statut * situation_handicap + age + genre) ,
      data = df_simul)
car::Anova(anova_3, type = 3)
  
# sans interactions
anova_4 <-
  aov(note_francais ~ mgs + statut + age + genre + situation_handicap,
      data = df_simul)
car::Anova(anova_4, type = 3)

# on va réutiliser rstatix pour avoir un beau tableau de résultats
df_simul %>% 
  rstatix::anova_test(note_francais ~ mgs + statut + situation_handicap + age + genre,
                      type = 3) %>%
  data.frame %>%
  mutate(interp_ges = effectsize::interpret_eta_squared(ges),
         p = format.pval(p, eps = .001),
         ges = round(ges, 2))

  # Nous avons fait une ANOVA de type 3 ayant comme variable dépendante la note en français
  # et comme variables indépendantes la MGS, le statut (pas de mesure d'aide, mesure d'aide 1
  # ou mesure d'aide 2), la situation de handicap (dichotomique), l'âge et la variable de genre.
  # Les résultat indiquent un effet de la moyenne au secondaire, p < 0.001; la taille d'effet était 
  # eta2 = 0.78 soit une grande magnitude selon les barèmes proposés par Field (2013). L'effet du
  # parcours en français était également significatif, p = 0.13 avec une taille d'effet associée
  # de eta2 = 0.13 soit une magnitude moyenne. Il n'y avait pas d'effet significatif des autres
  # prédicteurs.