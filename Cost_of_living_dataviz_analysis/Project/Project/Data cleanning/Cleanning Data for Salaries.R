
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)

data <- read_csv("Data cleanning/data.csv")

cat("Initial data structure:\n")
str(data)
cat("\nFirst few rows:\n")
head(data)

cleaned_data <- data %>%
  filter(!is.na(Position) | !is.na(Salary) | !is.na(Company)) %>%

  mutate(
    Date = mdy_hms(Date),
    Year = year(Date),
    Month = month(Date),

    Company = case_when(
      is.na(Company) | Company == "" ~ "Not Specified",
      str_trim(Company) == "" ~ "Not Specified",
      TRUE ~ str_trim(Company)
    ),

    Position = case_when(
      is.na(Position) | Position == "" ~ "Not Specified",
      str_trim(Position) == "" ~ "Not Specified",
      TRUE ~ str_trim(Position)
    ),


    Salary_Clean = str_remove_all(Salary, ","),
    Salary_Numeric = as.numeric(Salary_Clean),

    Experience_Years = case_when(
      is.na(`Years of experience`) ~ 0,
      `Years of experience` == "" ~ 0,
      TRUE ~ as.numeric(`Years of experience`)
    )
  ) %>%

  filter(!is.na(Salary_Numeric), Salary_Numeric > 0) %>%

  mutate(
    Salary_Category = case_when(
      Salary_Numeric < 200000 ~ "Under 200K",
      Salary_Numeric >= 200000 & Salary_Numeric < 500000 ~ "200K-500K",
      Salary_Numeric >= 500000 & Salary_Numeric < 1000000 ~ "500K-1M",
      Salary_Numeric >= 1000000 & Salary_Numeric < 2000000 ~ "1M-2M",
      Salary_Numeric >= 2000000 ~ "Above 2M"
    ),

    Experience_Level = case_when(
      Experience_Years == 0 ~ "Entry Level (0 years)",
      Experience_Years > 0 & Experience_Years <= 2 ~ "Junior (0-2 years)",
      Experience_Years > 2 & Experience_Years <= 5 ~ "Mid-Level (2-5 years)",
      Experience_Years > 5 & Experience_Years <= 10 ~ "Senior (5-10 years)",
      Experience_Years > 10 ~ "Expert (10+ years)",
      TRUE ~ "Unknown"
    ),

    Position_Standardized = case_when(
      str_detect(str_to_lower(Position), "software engineer|software developer|developer") ~ "Software Engineer/Developer",
      str_detect(str_to_lower(Position), "qa|quality assurance|test") ~ "QA/Testing",
      str_detect(str_to_lower(Position), "project manager|pm") ~ "Project Manager",
      str_detect(str_to_lower(Position), "product manager|product owner") ~ "Product Manager/Owner",
      str_detect(str_to_lower(Position), "data scientist|data analyst") ~ "Data Scientist/Analyst",
      str_detect(str_to_lower(Position), "ui/ux|designer") ~ "UI/UX Designer",
      str_detect(str_to_lower(Position), "devops|system administrator") ~ "DevOps/System Admin",
      str_detect(str_to_lower(Position), "android|ios|mobile") ~ "Mobile Developer",
      str_detect(str_to_lower(Position), "frontend|front-end|front end") ~ "Frontend Developer",
      str_detect(str_to_lower(Position), "backend|back-end|back end") ~ "Backend Developer",
      str_detect(str_to_lower(Position), "full stack|fullstack") ~ "Full Stack Developer",
      str_detect(str_to_lower(Position), "team lead|tech lead|lead") ~ "Team/Tech Lead",
      TRUE ~ "Other"
    ),

    Company_Clean = case_when(
      str_detect(str_to_lower(Company), "epam") ~ "EPAM Systems",
      str_detect(str_to_lower(Company), "picsart") ~ "PicsArt",
      str_detect(str_to_lower(Company), "vmware") ~ "VMware",
      str_detect(str_to_lower(Company), "synopsys") ~ "Synopsys",
      str_detect(str_to_lower(Company), "betconstruct|bet construct") ~ "BetConstruct",
      str_detect(str_to_lower(Company), "digitain") ~ "Digitain",
      str_detect(str_to_lower(Company), "freelance") ~ "Freelance",
      str_detect(str_to_lower(Company), "not specified") ~ "Not Specified",
      TRUE ~ Company
    )
  ) %>%

  select(
    Date,
    Year,
    Month,
    Company = Company_Clean,
    Position = Position,
    Position_Standardized,
    Salary_AMD = Salary_Numeric,
    Experience_Years,
    Experience_Level,
    Salary_Category
  ) %>%

  filter(Salary_AMD <= 15000000) %>%

  arrange(Date)

write_csv(cleaned_data, "Data cleanning/cleaned_salary_data.csv")
cat("\nCleaned data saved to: Data cleanning/cleaned_salary_data.csv\n")

