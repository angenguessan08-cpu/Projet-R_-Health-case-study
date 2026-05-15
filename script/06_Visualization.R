library(ggplot2)

# Création de  graphiques

# Les diagrammes en barres (Comparaisons de catégories)

ggplot(
  consultations_region,
  aes(
    x = region,
    y = total_consultations
  )
) +
  geom_col()



ggplot(
  consultations_region, 
  aes(
    x = reorder(region, total_consultations), 
    y = total_consultations)
  ) +
  geom_col(fill = "steelblue") +
  coord_flip()


# Les Histogrammes (Distribution d'une variable numérique)

ggplot(health_case_study, 
       aes(x = patient_age)
       ) +
      geom_histogram()


ggplot(health_case_study, 
       aes(x = patient_age)
       ) + 
  geom_histogram(bins = 20)



#Les Boîtes à moustaches / Boxplots (Repérer les variations et les prix)

ggplot(health_case_study, 
       aes(
         x = diagnosis, 
         y = treatment_cost)
       ) +
  geom_boxplot()



# Les graphiques linéaires (Évolution dans le temps)
ggplot(
  monthly_trend,
  aes(
    x = month,
    y = consultations
  )
) +
  geom_line()


ggplot(
  monthly_trend,
  aes(
    x = month,
    y = consultations
  )
) +
  geom_line() +
  geom_point()


# Le nuage de points / Scatter plot (Relations entre deux mesures)

ggplot(
  health_case_study,
  aes(
    x = patient_age,
    y = treatment_cost
  )
) +
  geom_point()


# Les graphiques à barres empilées  (Croiser deux catégories)

ggplot(
  health_case_study,
  aes(
    x = region,
    fill = gender
  )
) +
  geom_bar(position = "dodge")

ggplot(
  health_case_study,
  aes(
    x = region,
    fill = gender
  )
) +
  geom_bar()


# le decouparge

ggplot(
  health_case_study,
  aes(
    x = diagnosis,
    y = treatment_cost
  )
) +
  geom_boxplot() +
  facet_wrap(~ region)


#Le diagramme circulaire / Camembert (Parts de marché)

health_case_study %>%
  count(insurance_status) %>%
  ggplot(
    aes(
      x = "",
      y = n,
      fill = insurance_status
    )
  ) +
  geom_col() +
  coord_polar("y")


ggplot(
  consultations_region,
  aes(
    x = region,
    y = total_consultations
  )
) +
  geom_col() +
  labs(
    title = "Consultations par région",
    x = "Région",
    y = "Nombre"
  )

ggsave(
  filename = "outputs/consultations_region.png",
  width = 8,      # Largeur de 8 pouces
  height = 5,     # Hauteur de 5 pouces
  units = "in",   # Unité : "in" pour inches (pouces), ou "cm"
  dpi = 300       # Haute résolution pour impression ou présentation
)
