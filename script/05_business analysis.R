# Key performance indicators (KPIs) to provide:

# Territorial Activity → Ranking of regions by consultation volume.
# Pathological Burden → Identification of the most frequent diseases.
# Financial Analysis → Highest cost categories.
# Social Protection → Rate and quality of insurance coverage (public / private / none).
# Facility Performance → Top most active health centers.
# Temporal Trends → Monthly evolution of consultations (seasonality, peaks, trend).

# Expected Deliverable: Concise, visual, and actionable insights for each question.


# Final variables preparation

health_case_study <- health_case_study %>%
  mutate(
    age_group = case_when(
      patient_age < 18 ~ "Child",
      patient_age >= 18 & patient_age < 60 ~ "Adult",
      patient_age >= 60 ~ "Senior"
    )
  )

table(health_case_study$age_group)

health_case_study <- health_case_study %>%
  mutate(
    treatment_cost_usd = treatment_cost / 600
  )

nrow(health_case_study)



# Ranking of regions by consultation volume

consultations_region <- health_case_study %>%
  group_by(region) %>%
  summarise(
    total_consultations = n()
  ) %>%
  arrange(desc(total_consultations))

View(consultations_region)


# Identification of the most frequent diseases

top_diagnosis <- health_case_study %>%
  group_by(diagnosis) %>%
  summarise(
    total_cases = n()
  ) %>%
  arrange(desc(total_cases))

top_diagnosis


# Highest cost categories

cost_by_diagnosis <- health_case_study %>%
  group_by(diagnosis) %>%
  summarise(
    avg_cost = mean(treatment_cost)
  ) %>%
  arrange(desc(avg_cost))

health_case_study %>% summarise(avg_cost = mean(treatment_cost))
health_case_study %>% summarise(median_cost = median(treatment_cost))


# Rate and quality of insurance coverage

health_case_study  %>%
  group_by(insurance_status) %>%
  summarise(
    total = n()
  )


# Facility performance → Top most active health centers

health_case_study %>%
  group_by(facility_name) %>%
  summarise(
    total_consultations = n()
  ) %>%
  arrange(desc(total_consultations))


# Monthly evolution of consultations
monthly_trend <- health_case_study %>%
  group_by(month) %>%
  summarise(
    consultations = n()
  )

# Save the first deliverable

write_csv(
  consultations_region,
  "outputs/kpi_region.csv"
)