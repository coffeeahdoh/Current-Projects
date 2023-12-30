library(ds4psy) # sample_date f(x)
library(lubridate)
library(dplyr)

# setting seed for reproducibility
set.seed(41090)

# visit dates
visits <- data.frame(visit_date = sample_date(from = Sys.Date() - years(2), to = Sys.Date(), 
                                              size = 12000, 
                                              replace = TRUE))

visits$patient_id <- sample(patient_list$patient_id, size = 12000, replace = T)

# The Clinic no-show rate ranged between 30% and 35%, 
# average taken to represent no-show rate
## (Adams et al, 2020)
cpt_codes <- c("99202", "99203", "99204", "99205", "99211", "99212", "99213", 
               "99214", "99215", "00000")
probs_cpt <- c(rep(0.01, 9), 0.325)
cpt <- sample(cpt_codes, size = 12000, replace = T, prob = probs_cpt)

visits$billing_codes <- cpt

# Randomly adding notes of documented Chlamydia, Gonnorhea, Syphilis lab results
# to patients that showed for their appt
notes <- c("Full panel completed - awaiting results", 
           "Full panel completed - results documented",
           "Full panel scheduled",
           "Chalmydia & Gonorrhea tested - Syphilis test missing",
           "Syphilis tested - Chlamydia & Gonorrhea scheduled",
           "Chlamydia, Gonorrhea, Syphilis tests refused")

visits <- visits |> 
            mutate(notes = ifelse(billing_codes != "00000",
                                  sample(notes, 
                                         size = 2609,
                                         replace = TRUE),
                                  NA))

visits <- visits |> 
            arrange(visit_date) |>
            mutate(visit_id = 1:12000)

visits <- visits[, c(5, 1, 2, 3, 4)] |> arrange(patient_id)

# Quality Checks
visits[visits$billing_codes != "00000",]

visits[visits$patient_id %in% c(268, 392, 414, 596, 610),] |> arrange(patient_id)
