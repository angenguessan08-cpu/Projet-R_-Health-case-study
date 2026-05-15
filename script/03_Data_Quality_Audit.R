# Data_Quality_Audit


# Number of rows and columns
dim(health_case_study) 


# View last rows
tail(health_case_study)


# Detect duplicates
sum(duplicated(health_case_study))


# Total number of missing values
sum(is.na(health))


# Detect missing values (Number of NAs)
colSums(is.na(health_case_study))


# View dataset structure
glimpse(health_case_study)


# Statistical summary
summary(health_case_study) 


# Variable types
str(health_case_study) 


# Automated report
skim(health_case_study)


# Missing values visualization
vis_miss(health_case_study) 


# Check unique values
unique(health_case_study$gender) 

unique(health_case_study$diagnosis)

unique(health_case_study$region)


# Complete statistical summary
summary(health_case_study$patient_age)

summary(health_case_study$treatment_cost)


# Date diagnosis
head(health_case_study$consultation_date, 100)


# Frequency tables
table(health_case_study$diagnosis)

table(health_case_study$region)


