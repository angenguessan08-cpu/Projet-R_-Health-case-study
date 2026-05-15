# Data cleaning

sum(duplicated(health_case_study))

health_case_study <- distinct(health_case_study)

dim(health_case_study)

# Calculate mean cost
mean(health_case_study$treatment_cost)

# Remove NAs
colSums(is.na(health_case_study))

health_case_study <- health_case_study %>%
  drop_na(diagnosis, region)

View(health_case_study)


# Second verification 

colSums(is.na(health_case_study))

colSums(is.na(health_case_study) | health_case_study == "")



# Calculate safety median

median(health_case_study$treatment_cost, na.rm = TRUE)



# Replace missing costs with median

health_case_study <- health_case_study %>%
  mutate(
    treatment_cost = ifelse(
      is.na(treatment_cost),
      median(treatment_cost, na.rm = TRUE),
      treatment_cost
    )
  )


# Handle missing genders

health_case_study <- health_case_study %>%
  mutate(
    gender = replace_na(gender, "Unknown")
  )


# Handle missing insurance statuses

health_case_study <- health_case_study %>%
  mutate(
    insurance_status = replace_na(
      insurance_status,
      "Unknown"
    )
  )


# Final verifications

colSums(is.na(health_case_study))

glimpse(health_case_study)


# Save cleaned file

write_csv(
  health_case_study,
  "outputs/health_case_study_clean1.csv"
)


# Text harmonization (Regions, Gender, Insurance)


table(health_case_study$region)

unique(health_case_study$region)


health_case_study <- health_case_study %>%
  mutate(
    region = case_when(
      region %in% c("centre", "CENTER", "Ctr") ~ "Centre",
      region %in% c("litoral", "LITTORAL") ~ "Littoral",
      region %in% c("sud ouest", "SW") ~ "Sud-Ouest",
      TRUE ~ region
    )
  )


table(health_case_study$region)


unique(health_case_study$gender)


health_case_study <- health_case_study %>%
  mutate(
    gender = case_when(
      gender %in% c("male", "M", "Masculin") ~ "Male",
      gender %in% c("female", "F", "Feminin") ~ "Female",
      gender %in% c("", " ") ~ "Unknown",
      TRUE ~ gender
    )
  )

table(health_case_study$gender)


unique(health_case_study$insurance_status)

health_case_study <- health_case_study %>%
  mutate(
    insurance_status = case_when(
      insurance_status %in% c("insured", "INSURED") ~ "Insured",
      insurance_status %in% c("uninsured", "UNINSURED", "") ~ "Uninsured",
      TRUE ~ insurance_status
    )
  )

table(health_case_study$insurance_status)

unique(health_case_study$consultation_type)

unique(health_case_study$diagnosis)

table(health_case_study$gender)
table(health_case_study$region)

# Save categories
write_csv(
  health_case_study,
  "outputs/health_case_study_categories_clean2.csv"
)

# Date cleaning

install.packages("lubridate")
library(lubridate)

health_case_study <-  health_case_study %>%
  mutate(
    consultation_date = parse_date_time(
      consultation_date,
      orders = c("ymd", "dmy")
    )
  )



library(dplyr)
library(lubridate)

health_case_study <- health_case_study %>%
  mutate(
    consultation_date = parse_date_time(
      consultation_date,
      orders = c("ymd", "dmy")
    )
  )

class(health_case_study$consultation_date)


health_case_study$consultation_date <- as.Date(
  health_case_study$consultation_date
)

class(health_case_study$consultation_date)


head(health_case_study$consultation_date)

head(health_case_study$consultation_date, 100)


# Outlier removal

health_case_study <- health_case_study %>%
  filter(
    patient_age >= 0,
    patient_age <= 120
  )


summary(health$patient_age)


summary(health$treatment_cost)

health_case_study <- health_case_study %>%
  filter(
    treatment_cost >= 0,
    treatment_cost <= 500
  )

boxplot(health_case_study$treatment_cost)


health_case_study <- health_case_study %>%
  mutate(
    month = month(consultation_date),
    year = year(consultation_date)
  )

# Save final files
write_csv(
  health_case_study,
  "outputs/health_case_study_clean_final.csv"
)


# Create age groups (case_when)

health_case_study <- health_case_study %>%
  mutate(
    age_group = case_when(
      patient_age < 18 ~ "Child",
      patient_age >= 18 & patient_age < 60 ~ "Adult",
      patient_age >= 60 ~ "Senior"
    )
  )
table(health_case_study$age_group)


# Currency conversion (Conversion to USD)

health_case_study <- health_case_study %>%
  mutate(
    treatment_cost_usd = treatment_cost / 600
  )
















