# MIPS -- #205 (https://mdinteractive.com/files/uploaded/file/CMS2023/2023_Measure_205_MIPSCQM.pdf)
## cpt codes 
# 99202, 99203, 99204, 99205, 99211, 99212, 99213, 99214, 99215

# dx codes
# Z21, B20

# exclusion
# G9725

library(sqldf)

query <- function(x) {
  sqldf(drv = "RSQLite", x = x)
}

## DENOMINATOR ##
query("WITH date_difference AS (
           SELECT visit_id
                  , DATE('1970-01-01', LAG(visit_date) OVER(PARTITION BY patient_id 
                                         ORDER BY visit_date) || ' day') AS following_visit_date
                  , JULIANDAY(DATE('1970-01-01', visit_date || ' day')) -
                       JULIANDAY(DATE('1970-01-01', LAG(visit_date) OVER(PARTITION BY patient_id 
                                         ORDER BY visit_date) || ' day')) AS day_difference
           FROM visits )
           SELECT DISTINCT vts.visit_id
                           , vts.visit_date
                           , pts.patient_id
                           , pts.diagnoses
                           , pts.gender
                           , pts.ethnicity
                           , vts.billing_codes
                           , vts.notes
                           , pts.provider_id
           FROM visits vts
              LEFT JOIN patient_list pts 
                ON vts.patient_id = pts.patient_id
              LEFT JOIN providers pvdr
                ON pts.provider_id = pvdr.id
              LEFT JOIN date_difference date
                ON vts.visit_id = date.visit_id
           WHERE age >= 13
                 AND diagnoses IN ('Z21', 'B20')
                 AND billing_codes IN (99202, 99203, 99204, 
                                       99205, 99211, 99212, 
                                       99213, 99214, 99215)
                 AND day_difference >= 90;")

## NUMERATOR ##
                                         