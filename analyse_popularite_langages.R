# ==============================================================================
# Projet : Analyse de la popularité des langages de programmation
# Source des données : Stack Exchange Data Explorer (via DataCamp DataLab)
# Auteur : Mustakeem Adémola Arèmou LATOUNDJI (Mtk)
# ==============================================================================

# ---- 1. Chargement des packages nécessaires ----
library(readr)
library(dplyr)
library(ggplot2)

# ---- 2. Chargement du jeu de données ----
data <- read_csv("stack_overflow_data.csv")

# Aperçu du jeu de données
head(data)

# ==============================================================================
# Question 1 : Pourcentage de questions sur R en 2020
# ==============================================================================
r_2020 <- data %>%
  filter(tag == "r", year == 2020) %>%
  mutate(percentage = num_questions / year_total * 100) %>%
  select(year, tag, num_questions, year_total, percentage)

r_2020

# ==============================================================================
# Question 2 : Les 5 tags avec le plus de questions entre 2015 et 2020 (inclus)
# ==============================================================================
highest_tags <- data %>%
  filter(year >= 2015, year <= 2020) %>%
  group_by(tag) %>%
  summarize(total_questions = sum(num_questions)) %>%
  arrange(desc(total_questions)) %>%
  slice_head(n = 5) %>%
  select(tag)

highest_tags

# ==============================================================================
# Bonus : Évolution de la popularité de R au fil du temps (2008-2020)
# ==============================================================================
r_over_time <- data %>%
  filter(tag == "r") %>%
  mutate(percentage = num_questions / year_total * 100)

ggplot(r_over_time, aes(x = year, y = percentage)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  labs(
    title = "Popularité relative de R sur Stack Overflow (2008-2020)",
    x = "Année",
    y = "Pourcentage des questions (%)"
  ) +
  theme_minimal()

# Pour exporter le graphique en image (à exécuter dans DataCamp DataLab) :
# ggsave("evolution_popularite_R.png", width = 8, height = 5, dpi = 300)
