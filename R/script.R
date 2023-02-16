# Gestion de l'environnement ----

devtools::install_deps(upgrade = "never")

library(tidyverse)
library(dplyr)
library(ggplot2)

yaml::read_yaml("secrets.yaml")[["password"]][["jeton_api"]]

# Fonctions ----

source("R/fonctions.R", encoding = "UTF-8")

# Import des donnees ----

df <- readr::read_csv2(
  "data/individu_reg.csv",
  col_select = c(
    "region", "aemm", "aged", "anai", "catl", "cs1", "cs2", "cs3",
    "couple", "na38", "naf08", "pnai12", "sexe", "surf", "tp", "trans", "ur"
  )
)
summarise(group_by(df, aged), n())

df <- arrow::read_parquet(
  "data/individu_reg.parquet",
  col_select = c(
    "region", "aemm", "aged", "anai", "catl", "cs1", "cs2", "cs3",
    "couple", "na38", "naf08", "pnai12", "sexe", "surf", "tp", "trans", "ur"
  )
)


# Retraitement des donnees ----

df$sexe <- df$sexe %>%
  as.character() %>%
  forcats::fct_recode(Homme = "1", Femme = "2")

# Statistiques descriptives -----

calcul_stats_desc(df %>%
                           filter(sexe == "Homme") %>%
                           mutate(aged = as.numeric(aged)) %>%
                           pull(aged))
calcul_stats_desc(df %>%
                           filter(sexe == "Femme") %>%
                           mutate(aged = as.numeric(aged)) %>%
                           pull(aged))

tats_age <- df %>%
  mutate(age = as.numeric(aged))
group_by(decennie = decennie_a_partir_annee(age)) %>%
  summarise(n())

table_age <- gt::gt(stats_age) %>%
  gt::tab_header(
    title = "Distribution des âges dans notre population"
  ) %>%
  gt::fmt_number(
    columns = `n()`,
    sep_mark = " ",
    decimals = 0
  ) %>%
  gt::cols_label(
    decennie = "Tranche d'âge",
    `n()` = "Population"
  )


# Graphiques ----

ggplot(df) +
  geom_histogram(aes(x = 5 * floor(as.numeric(aged) / 5)),
                 stat = "count"
  )

# part d'hommes dans chaque cohorte
df %>%
  group_by(aged, sexe) %>%
  summarise(SH_sexe = n()) %>%
  group_by(aged) %>%
  mutate(SH_sexe = SH_sexe / sum(SH_sexe)) %>%
  filter(sexe == "Homme") %>%
  ggplot() +
  geom_bar(aes(x = as.numeric(aged), y = SH_sexe),
           stat = "identity"
  ) +
  geom_point(aes(x = as.numeric(aged), y = SH_sexe),
             stat = "identity", color = "blue"
  ) +
  coord_cartesian(c(0, 100))

# stat trans par statut
p <- tibble(df %>%
              group_by(couple, trans) %>%
              summarise(x = n()) %>%
              group_by(couple) %>%
              mutate(y = 100 * x / sum(x))) %>%
  ggplot() +
  geom_bar(aes(x = trans, y = y, color = couple),
           stat = "identity",
           position = "dodge")

ggsave("output/p.png", p)



# Modelisation ----

df3 <- df %>%
  select(surf, cs1, ur, couple, aged) %>%
  filter(surf != "Z")
df3[, 1] <- factor(df3$surf, ordered = TRUE)
df3[, "cs1"] <- factor(df3$cs1)
df3 %>%
  filter(couple == "2" & aged > 40 & aged < 60)

MASS::polr(surf ~ cs1 + factor(ur), df3)
