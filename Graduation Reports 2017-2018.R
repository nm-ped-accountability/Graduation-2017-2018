
### Produce various graduation reports

# EdFacts student-level reports
# consolidated student-level reports for SOAP
# CCRB student-level reports
# LFC student-level reports
# student-level reports for calculating grad rates

library(Hmisc)
library(tidyverse)
options(digits = 5)

################################################################################
## retrieve data files
################################################################################
# 4yr cohort of 2018
rm(list = ls())

dat <- read.csv("Corrected 4yr cohort of 2018 added demo.csv", 
                header = TRUE, stringsAsFactors = FALSE)
nrow(dat) #39718

# Richard added commas when matching
dat$schnumb <- gsub(",", "", dat$schnumb)
dat$schnumb <- as.numeric(dat$schnumb)

# homeless
homeless <- read.csv("grad cohort add_homeless updated.csv",
                     header = TRUE, stringsAsFactors = FALSE)

################################################################################
# 5 yr cohort of 2017
rm(list = ls())

dat <- read.csv("Corrected 5yr cohort of 2017.csv",
                header = TRUE, stringsAsFactors = FALSE)
nrow(dat) #40803

################################################################################
# 6 yr cohort of 2016
rm(list = ls())

dat <- read.csv("Corrected 6yr cohort of 2016.csv",
                header = TRUE, stringsAsFactors = FALSE)
nrow(dat) #40046

################################################################################



################################################################################
## pre-processing variables
################################################################################
# graduated
dat$graduated[dat$outcome == "WG"] <- 1
dat$graduated[is.na(dat$graduated)] <- 0
table(dat$graduated)

# excused
dat$graduated[dat$outcome == "FX"] <- 3
dat$graduated[dat$outcome == "W6"] <- 3
dat$graduated[dat$outcome == "W8"] <- 3
dat$graduated[dat$outcome == "W10"] <- 3
dat$graduated[dat$outcome == "W81"] <- 3
dat$graduated[dat$outcome == "WD"] <- 3
dat$graduated[dat$outcome == "D1"] <- 3
dat$graduated[dat$outcome == "D2"] <- 3

table(dat$outcome, dat$graduated)
table(dat$graduated)

# fractions
dat$fraction <- dat$numsnapshots / dat$totalsnapshots
range(dat$fraction)



################################################################################
## EdFacts reports
################################################################################
# excused students are included in the EdFacts reports
edfacts <- dat

##########
# save outputs
current_date <- Sys.Date()
file.name <- paste0("EdFacts 4yr Cohort of 2018 ", current_date, ".csv")
write.csv(edfacts, file = file.name, row.names = FALSE, na = "")
nrow(edfacts)
# 4-year cohort of 2018: 39718



################################################################################
## consolidated outcome report published in SOAP
################################################################################
# excused students are removed from this report
consolidated <- dat
consolidated <- consolidated[consolidated$graduated != 3, ]

names(consolidated) <- tolower(names(consolidated))
consolidated <- consolidated[c("districtcode", "locationid", "studentid", 
                               "lastname", "firstname", "numsnapshots", 
                               "totalsnapshots", "outcome", "ethnicity", 
                               "gender", "frl", "everell", "everiep", 
                               "active_duty", "foster_care", "migrant_added", 
                               "fraction", "graduated", "schnumb")]

# recover district codes
consolidated$districtcode <- floor(consolidated$schnumb / 1000)
consolidated$schnumb <- NULL

# add additional columns and format names
consolidated$SchoolNumerator <- consolidated$graduated*consolidated$fraction
consolidated$SchoolDenominator <- consolidated$fraction
consolidated$graduated <- NULL
names(consolidated) <- c("DistrictCode", "SchoolCode", "StudentID", 
                         "LastName", "FirstName", "SchoolSnaps", "StateSnaps", 
                         "Outcome", "Ethnicity", "Gender", "FRL", "ELL", "SWD", 
                         "ActiveDuty", "FosterCare", "Migrant", "SchoolFraction", 
                         "SchoolNumerator", "SchoolDenominator")

consolidated$LastName <- paste(toupper(substr(consolidated$LastName, 1, 1)), 
                               substr(consolidated$LastName, 2, 
                                      nchar(consolidated$LastName)), sep = "")

consolidated$FirstName <- paste(toupper(substr(consolidated$FirstName, 1, 1)), 
                                substr(consolidated$FirstName, 2, 
                                       nchar(consolidated$FirstName)), sep = "")

##########
# save outputs
# 4-year: 36584
nrow(consolidated)
current_date <- Sys.Date()
file_name <- paste0("SOAP 4 Year 2018 Consolidated Outcome Report ", 
                    current_date, ".csv")
write.csv(consolidated, file = file_name, row.names = FALSE, na = "")

# 5-year: 37112
nrow(consolidated)
current_date <- Sys.Date()
file_name <- paste0("SOAP 5 Year 2017 Consolidated Outcome Report ", 
                    current_date, ".csv")
write.csv(consolidated, file = file_name, row.names = FALSE, na = "")


# 6-year: 35727
nrow(consolidated)
current_date <- Sys.Date()
file_name <- paste0("SOAP 6 Year 2016 Consolidated Outcome Report ", 
                    current_date, ".csv")
write.csv(consolidated, file = file_name, row.names = FALSE, na = "")



################################################################################
## CCRB student-level reports
################################################################################
# excused students are removed from this report
ccrb <- dat
ccrb <- ccrb[ccrb$graduated != 3, ]

ccrb <- ccrb %>%
    group_by(studentid, districtcode) %>%
    mutate(numsnapshots_dist = sum(numsnapshots))

ccrb$Cohort <- "2017-2018"
ccrb$District_Code <- ccrb$districtcode
ccrb$Location_ID <- ccrb$locationid
ccrb$Student_ID <- ccrb$studentid
ccrb$Number_of_Snapshots_School <- ccrb$numsnapshots
ccrb$Number_of_Snapshots_District <- ccrb$numsnapshots_dist
ccrb$Number_of_Snapshots_Total <- ccrb$totalsnapshots
ccrb$School_Numerator <- ccrb$Number_of_Snapshots_School * ccrb$graduated
ccrb$School_Denominator <- ccrb$totalsnapshots
ccrb$District_Numerator <- ccrb$Number_of_Snapshots_District * ccrb$graduated
ccrb$District_Denominator <- ccrb$totalsnapshots
ccrb$Outcome <- ccrb$outcome
ccrb$Outcome_Description <- ccrb$outcomedesc
ccrb$Graduated <- ccrb$graduated
ccrb$Use_In_Cohort_Calculation <- "Y"
names(ccrb)
ccrb <- ccrb[c(34:48)]

# update outcome descriptions
table(ccrb$Outcome)
table(ccrb$Outcome_Description)
ccrb$Outcome_Description <- NA
ccrb$Outcome_Description[ccrb$Outcome == "E1"] <- "Still Enrolled"
ccrb$Outcome_Description[ccrb$Outcome == "E2"] <- "Still Enrolled"
ccrb$Outcome_Description[ccrb$Outcome == "E3"] <- "Still Enrolled"
ccrb$Outcome_Description[ccrb$Outcome == "R1"] <- "Still Enrolled"
ccrb$Outcome_Description[ccrb$Outcome == "R2"] <- "Still Enrolled"
ccrb$Outcome_Description[ccrb$Outcome == "R3"] <- "Still Enrolled"
ccrb$Outcome_Description[ccrb$Outcome == "W1"] <- "Withdrawn"
ccrb$Outcome_Description[ccrb$Outcome == "W2"] <- "Withdrawn - Whereabouts Unknown"
ccrb$Outcome_Description[ccrb$Outcome == "W3"] <- "Withdrawn - No Show"
ccrb$Outcome_Description[ccrb$Outcome == "W4"] <- "Withdrawn - GED"
ccrb$Outcome_Description[ccrb$Outcome == "W5"] <- "Withdrawn - Court ordered"
ccrb$Outcome_Description[ccrb$Outcome == "W7"] <- "Withdrawn - Pregnant"
ccrb$Outcome_Description[ccrb$Outcome == "W9"] <- "Withdrawn - Failed Immunization"
ccrb$Outcome_Description[ccrb$Outcome == "W11"] <- "Withdrawn - Lack of interest"
ccrb$Outcome_Description[ccrb$Outcome == "W12"] <- "Withdrawn - Unable to adjust"
ccrb$Outcome_Description[ccrb$Outcome == "W13"] <- "Withdrawn -  Left school to work"
ccrb$Outcome_Description[ccrb$Outcome == "W14"] <- "Withdrawn - Failing"
ccrb$Outcome_Description[ccrb$Outcome == "W15"] <- "Withdrawn - Parental request"
ccrb$Outcome_Description[ccrb$Outcome == "W16"] <- "Withdrawn - Child care problems"
ccrb$Outcome_Description[ccrb$Outcome == "W17"] <- "Withdrawn - Runaway"
ccrb$Outcome_Description[ccrb$Outcome == "W18"] <- "Withdrawn - Married and left school"
ccrb$Outcome_Description[ccrb$Outcome == "W21"] <- "Withdrawn - Suspension"
ccrb$Outcome_Description[ccrb$Outcome == "W23"] <- "Withdrawn - Illness"
ccrb$Outcome_Description[ccrb$Outcome == "W24"] <- "Withdrawn - Illness not verified"
ccrb$Outcome_Description[ccrb$Outcome == "WC"] <- "Withdrawn - Certificate of Completion"
ccrb$Outcome_Description[ccrb$Outcome == "WG"] <- "Graduated"
table(ccrb$Outcome_Description)
nrow(ccrb[is.na(ccrb$Outcome_Description), ]) #17 missing observations

##########
# save outputs
# 4-year: 36584
nrow(ccrb)
current_date <- Sys.Date()
file_name <- paste0("CCRB 4 Year Cohort of 2018 ", current_date, ".csv")
write.csv(ccrb, file = file_name, row.names = FALSE, na = "")



################################################################################
## LFC reports
################################################################################
# excused students are removed from this report
lfc <- dat
lfc <- lfc[lfc$graduated != 3, ]

# LFC requests the 4-year cohort with student IDs only
lfc <- lfc$studentid

##########
# save outputs
current_date <- Sys.Date()
file_name <- paste0("LFC 4-Year Cohort of 2018 ", current_date, ".csv")
write.csv(lfc, file = file_name, row.names = FALSE, na = "")



################################################################################
## student-level reports for calculating grad rates
################################################################################
# excused students are removed from this report
dat <- dat[dat$graduated != 3, ]

names(dat) <- tolower(names(dat))

# demographics
table(dat$everell)
dat$everell[dat$everell == "Y"] <- "English Learners"
dat$everell[dat$everell == "N"] <- "Non ELs"

table(dat$everiep)
dat$everiep[dat$everiep == "Y"] <- "Students with Disabilities"
dat$everiep[dat$everiep == "N"] <- "Non SWD"

table(dat$frl)
dat$frl[dat$frl == "Y"] <- "Economically Disadvantaged"
dat$frl[dat$frl == "N"] <- "Non ED"

table(dat$gender)
dat$gender[dat$gender == "F"] <- "Female"
dat$gender[dat$gender == "M"] <- "Male"

table(dat$ethnicity)
dat$ethnicity[dat$ethnicity == "A"] <- "Asian"
dat$ethnicity[dat$ethnicity == "B"] <- "African American"
dat$ethnicity[dat$ethnicity == "C"] <- "Caucasian"
dat$ethnicity[dat$ethnicity == "H"] <- "Hispanic"
dat$ethnicity[dat$ethnicity == "I"] <- "Native American"
dat$hispanic[dat$ethnicity == "Hispanic"] <- "Hispanic2"
dat$hispanic[is.na(dat$hispanic)] <- "Non Hispanic"

# homeless
homeless[duplicated(homeless$ï..studentid), ] #two ID duplicates
#neither record was homeless
table(homeless$cohort) #2016, 2017, 2018
table(homeless$homeless) #both HS and HNS mean homeless

dat$homeless <- homeless$homeless[match(dat$studentid, homeless$ï..studentid)]
table(dat$homeless)
dat$homeless[dat$homeless == ""] <- "Non Homeless"
dat$homeless[dat$homeless == "HS" | dat$homeless == "HNS"] <- "Homeless"
table(dat$homeless)

# active duty
table(dat$active_duty)
dat$active_duty[dat$active_duty == "Y"] <- "Active Duty" #698
dat$active_duty[dat$active_duty == ""] <- "Not Active Duty" #35886

# foster care
table(dat$foster_care)
dat$foster_care[dat$foster_care == "Y"] <- "Foster Care" #230
dat$foster_care[dat$foster_care == ""] <- "Not Foster Care" #36354

# migrant
table(dat$migrant_added)
dat$migrant_added[dat$migrant_added == "Y"] <- "Migrant" #122
dat$migrant_added[dat$migrant_added == ""] <- "Not Migrant" #36472

##########
# save output
# 4-year: 36584
nrow(dat)
current_date <- Sys.Date()
file_name <- paste0("Cleaned 4-Year Cohort of 2018 Student-Level Report ", 
                    current_date, ".csv")
write.csv(dat, file = file_name, row.names = FALSE, na = "")

# 5-year: 37112
nrow(dat)
current_date <- Sys.Date()
file_name <- paste0("Cleaned 5-Year Cohort of 2017 Student-Level Report ", 
                    current_date, ".csv")
write.csv(dat, file = file_name, row.names = FALSE, na = "")

# 6-year: 35727
nrow(dat)
current_date <- Sys.Date()
file_name <- paste0("Cleaned 6-Year Cohort of 2016 Student-Level Report ", 
                    current_date, ".csv")
write.csv(dat, file = file_name, row.names = FALSE, na = "")
