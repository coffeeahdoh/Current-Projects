################################################################################
# Title: Simulated FQHC
# Author: MichaelAngelo Confiado
# Description: Creation of a simulated FQHC data set with 1000 patients
# Tables -> Columns: patient_list -> pt first name, pt last name, 
#                                     provider (last, first), gender, ethnicity,
#                                     date of birth
#                    visits -> visit date, comments, cpt codes, dx codes
# Stats used to build distributions: - age distribution in FQHCs (Michas, 2022)
#                                    - 0.32% PLWH (AIDSVu, 2020)
#                                    - 23% PLWH & bacterial co-infection
################################################################################

install.packages(c("randomNames", "ds4psy"))
library(randomNames)
library(lubridate)
library(ds4psy)
library(dplyr)

# setting seed for reproducibility
set.seed(1028)

# randomly generated patient names w/ demographic info
## Ethnicity 
### 1. American Indian or Native Alaskan
### 2. Asian or Pacific Islander
### 3. Black (not Hispanic)
### 4. Hispanic
### 5. White (not Hispanic)
### 6. Middle-Eastern, Arabic
##
## Gender
### 0 = Female
### 1 = Male
patients <- data.frame(id = 1:1000, randomNames(n = 1000, 
                                    gender = 1000, 
                                    ethnicity = 1000,
                                    return.complete.data = TRUE))

# randomly generated provider names
## 10 providers for simulated FQHC
providers <- data.frame(id = 1:10, provider = randomNames(n = 10))

# function to randomly select birthdates
birthdates <- function(from, to, size) {
  from_date <- Sys.Date() - years(from)
  to_date <- Sys.Date() - years(to)
  birthdates <- sample_date(from = from_date, to = to_date, size = size)
  return(birthdates)
}

# age distribution of pts in FQHCs (Michas, 2022)
### note: max age 99
birthday_under5 <- birthdates(4, 0, 80)    # under 5 -> 8%
birthday_5to12 <- birthdates(12, 5, 120)   # 5 - 12  -> 12%
birthday_13to17 <- birthdates(17, 13, 80)  # 13 - 17 -> 8%
birthday_18to19 <- birthdates(19, 18, 30)  # 18 - 19 -> 3% 
birthday_20to44 <- birthdates(44, 20, 340) # 20 - 44 -> 34%
birthday_45to64 <- birthdates(64, 45, 250) # 45 - 64 -> 25%
birthday_65plus <- birthdates(99, 65, 100) # 65+     -> 10%

# combined birthdates
dob <- c(birthday_under5, birthday_5to12, birthday_13to17, birthday_18to19,
         birthday_20to44, birthday_45to64, birthday_65plus)

# patient ages 
age <- function(dob, on.day=today()) {
  intvl <- interval(dob, on.day)
  prd <- as.period(intvl)
  return(prd@year)
}

# 0.77% of total patients at an FQHC w/ symptomatic or asymptomatic HIV/AIDS
## (data.hrsa.gov, 2022)
# According to ICD-10, B20 is used when the patient has confirmed AIDS. 
# Following ICD-10 guidelines, if a patient has or has had an HIV related 
# condition, use **B20** AIDS. If the patient has a positive HIV status, without 
# symptoms or related conditions, use **Z21**. See the excerpts from the ICD-10-CM 
# General Guidelines, below.
## (codingintel.com)
# percentages multiplied by 10 to simulate an HIV-focused care center

dx_codes <- c("Z21", "B20", "ZZZ")
probs_dx <- c((0.0077 * 100)/2, (0.0077 * 100)/2, 1 - (0.0077 * 100))
dx <- sample(dx_codes, size = 1000, replace = TRUE, prob = probs_dx)

# combined patient info (patients, providers, date_of_birth)
demographics <- data.frame(patient_id = as.integer(patients[,1]), 
                              first_name = as.character(patients[,4]),
                              last_name = as.character(patients[,5]),
                              gender = as.integer(patients[,2]),
                              ethnicity = as.integer(patients[,3]),
                              dob = as.Date(dob),
                              age = as.integer(age(dob)),
                              provider_id = as.integer(sample(providers$id, 
                                                        size = 1000, 
                                                        replace = TRUE, 
                                                        prob = c(0.19, 0.18, 0.15, 0.20,
                                                                 0.08, 0.05, 0.01, 0.02,
                                                                 0.08, 0.04))),
                              diagnoses = dx)
patient_list[patient_list$diagnoses != "ZZZ",]
