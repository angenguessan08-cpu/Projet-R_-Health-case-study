# ==============================================================================
# PUBLIC HEALTH PROJECT: DESCRIPTIVE STATISTICS
# ==============================================================================

# 1. Base statistics on patient age
mean_age   <- mean(health_case_study$patient_age, na.rm = TRUE)   # Mean age
median_age <- median(health_case_study$patient_age, na.rm = TRUE) # Median age

# 2. Treatment cost analysis
min_cost  <- min(health_case_study$treatment_cost, na.rm = TRUE)
max_cost  <- max(health_case_study$treatment_cost, na.rm = TRUE)
var_cost  <- var(health_case_study$treatment_cost, na.rm = TRUE)  # Variance
sd_cost   <- sd(health_case_study$treatment_cost, na.rm = TRUE)   # Standard deviation

# Full summaries and quantiles of costs
summary(health_case_study$treatment_cost)
quantile(health_case_study$treatment_cost, na.rm = TRUE)

# 3. Frequency tables (Diagnosis and Gender)
table(health_case_study$diagnosis)                       # Total counts per pathology
prop.table(table(health_case_study$gender))              # Proportions by gender (F/M)

# 4. Group analyses using Tidyverse (dplyr)
library(dplyr)

# Mean and median costs by region
cost_by_region <- health_case_study %>%
  group_by(region) %>%
  summarise(
    avg_cost    = mean(treatment_cost, na.rm = TRUE),
    median_cost = median(treatment_cost, na.rm = TRUE)
  )

# Mean age and cost by diagnosis type (Pathology)
metrics_by_diagnosis <- health_case_study %>%
  group_by(diagnosis) %>%
  summarise(
    avg_age  = mean(patient_age, na.rm = TRUE),
    avg_cost = mean(treatment_cost, na.rm = TRUE)
  )

# 5. Correlation between patient age and treatment cost
correlation_age_cost <- cor(
  health_case_study$patient_age,
  health_case_study$treatment_cost,
  use = "complete.obs" # Handles missing values if any exist
)

# 6. Quick overview of the entire dataset
# (Requires library(skimr))
skimr::skim(health_case_study)


# ==============================================================================
# EXPORTING THE RESULTS
# ==============================================================================
library(readr)

# Creating the global summary table
stats_summary <- health_case_study %>%
  summarise(
    avg_age    = mean(patient_age, na.rm = TRUE),
    median_age = median(patient_age, na.rm = TRUE),
    avg_cost   = mean(treatment_cost, na.rm = TRUE)
  )

# Saving as CSV format in your outputs folder
write_csv(
  stats_summary,
  "outputs/stats_summary.csv"
)
