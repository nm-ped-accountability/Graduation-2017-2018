
### Grad Rate Calculations

library(Hmisc)
library(tidyverse)

################################################################################
## open data files
################################################################################
# 4yr cohort of 2018
rm(list = ls())

dat <- read.csv("Cleaned 4-Year Cohort of 2018 Student-Level Report 2019-07-22.csv", 
                header = TRUE, stringsAsFactors = FALSE)
nrow(dat) #36584

mastersch <- read.csv("MasterSchoolsAllYearsCorrected2018V10.csv", 
                      header = TRUE, stringsAsFactors = FALSE)
mastersch <- mastersch[mastersch$ï..SY == 2018, ]
mastersch <- mastersch[mastersch$open != "CLOSED" | mastersch$open != "CLOSEDGT1Y", ]
nrow(mastersch) #1789

################################################################################
# 5 yr cohort of 2017
rm(list = ls())

dat <- read.csv("Corrected 5yr cohort of 2017.csv",
                header = TRUE, stringsAsFactors = FALSE)
nrow(dat) #40803

mastersch <- read.csv("MasterSchoolsAllYearsCorrected2018V10.csv", 
                      header = TRUE, stringsAsFactors = FALSE)
mastersch <- mastersch[mastersch$ï..SY == 2017, ]
mastersch <- mastersch[mastersch$open != "CLOSED" | mastersch$open != "CLOSEDGT1Y", ]
nrow(mastersch) #1629

################################################################################
# 6 yr cohort of 2016
rm(list = ls())

dat <- read.csv("Corrected 6yr cohort of 2016.csv",
                header = TRUE, stringsAsFactors = FALSE)
nrow(dat) #40046

mastersch <- read.csv("MasterSchoolsAllYearsCorrected2018V10.csv", 
                      header = TRUE, stringsAsFactors = FALSE)
mastersch <- mastersch[mastersch$ï..SY == 2016, ]
mastersch <- mastersch[mastersch$open != "CLOSED" | mastersch$open != "CLOSEDGT1Y", ]
nrow(mastersch) #1993

################################################################################



################################################################################
# calculate grad rates
################################################################################
dat$nrecord <- 1

groups <- c("nrecord", "gender", "ethnicity", "hispanic", 
            "everell", "everiep", "frl", "homeless", 
            "active_duty", "foster_care", "migrant_added")

################################################################################
# non-shared accountability: statewide rates
state <- dat
state <- state[!duplicated(state$studentid), ]
nrow(state) 
# 4-year cohort of 2018: N=26288
# 5-year cohort of 2017: N=26214


# not shared accountability, each student is counted once
rate1 <- function(dataset) {
    GradRates <- data.frame()
    
    for (group in groups) {
        #group <- enquo(group)
        GroupRate <- dataset %>%
            select(!!group, graduated, nrecord) %>%
            group_by_(group) %>%
            summarise(numerator = sum(graduated),
                      denominator = sum(nrecord),
                      NStudents = sum(nrecord),
                      Rate = mean(graduated))
        
        names(GroupRate) <- c("Group", "Numerator", "Denominator", 
                              "NStudents", "GradRate")
        GradRates <- rbind(GroupRate, GradRates)
    }
    GradRates
}

StateRate <- rate1(state)
StateRate$schnumb <- 999999
StateRate$DistrictCode <- 999
StateRate$SchoolCode <- 999
StateRate$SORT <- 1
nrow(StateRate) #24

################################################################################
# shared accountability (SA) rates (district and school level rates)
SAdat <- dat
SAdat$shared <- SAdat$graduated * SAdat$fraction
SARates <- data.frame()

rate2 <- function(dataset, code) {
    
    for (group in groups) {
        GroupRate <- dataset %>%
            select(studentid, group, code, nrecord, shared, fraction) %>%
            group_by_(code, group) %>%
            summarise(numerator = sum(shared), 
                      denominator = sum(fraction),
                      NStudents = length(unique(studentid)),
                      Rate = sum(shared) / sum(fraction))
        names(GroupRate) <- c("Code", "Group", 
                              "Numerator", "Denominator", "NStudents", "GradRate")
        
        GroupRate <- GroupRate[GroupRate$Code != 999999, ]
        SARates <- rbind(GroupRate, SARates)
    }
    SARates
}

DistrictRate <- rate2(SAdat, "districtcode")
DistrictRate$schnumb <- DistrictRate$Code * 1000
DistrictRate$DistrictCode <- DistrictRate$Code
DistrictRate$SchoolCode <- 0
DistrictRate$Code <- NULL
DistrictRate$SORT <- 2
nrow(DistrictRate)
# 4yr cohort of 2018: N=1872
# 5yr cohort of 2017: N=1505
# 6yr cohort of 2016: N=1510

SchoolRate <- rate2(SAdat, "schnumb")
SchoolRate$schnumb <- SchoolRate$Code
SchoolRate$DistrictCode <- floor(SchoolRate$schnumb / 1000)
SchoolRate$SchoolCode <- SchoolRate$schnumb - (SchoolRate$DistrictCode * 1000)
SchoolRate$Code <- NULL
SchoolRate$SORT <- 3
nrow(SchoolRate)
# 4yr cohort of 2018: N=5513
# 5yr cohort of 2017: N=4478
# 6yr cohort of 2016: N=4479

################################################################################
## merging and formatting
mrg <- rbind(StateRate, DistrictRate, SchoolRate)
mrg$GradRate <- round(mrg$GradRate * 100, digits = 1)

mrg$Group <- as.character(mrg$Group)
mrg$Group <- gsub("1", "All Students", mrg$Group)

mrg$SORTCODE[mrg$Group == "All Students"] <- 1
mrg$SORTCODE[mrg$Group == "Female"] <- 2
mrg$SORTCODE[mrg$Group == "Male"] <- 3
mrg$SORTCODE[mrg$Group == "Caucasian"] <- 4
mrg$SORTCODE[mrg$Group == "African American"] <- 5
mrg$SORTCODE[mrg$Group == "Hispanic"] <- 6
mrg$SORTCODE[mrg$Group == "Asian"] <- 7
mrg$SORTCODE[mrg$Group == "Native American"] <- 8
mrg$SORTCODE[mrg$Group == "Economically Disadvantaged"] <- 9
mrg$SORTCODE[mrg$Group == "Students with Disabilities"] <- 10
mrg$SORTCODE[mrg$Group == "English Learners"] <- 11
mrg$SORTCODE[mrg$Group == "Homeless"] <- 12
mrg$SORTCODE[mrg$Group == "Non Hispanic"] <- 13
mrg$SORTCODE[mrg$Group == "Active Duty"] <- 14
mrg$SORTCODE[mrg$Group == "Foster Care"] <- 15
mrg$SORTCODE[mrg$Group == "Migrant"] <- 16
mrg$SORTCODE[mrg$Group == "Non ED"] <- 17
mrg$SORTCODE[mrg$Group == "Non SWD"] <- 18
mrg$SORTCODE[mrg$Group == "Non ELs"] <- 19
mrg$SORTCODE[mrg$Group == "Non Homeless"] <- 20
mrg$SORTCODE[mrg$Group == "Not Active Duty"] <- 21
mrg$SORTCODE[mrg$Group == "Not Foster Care"] <- 22
mrg$SORTCODE[mrg$Group == "Not Migrant"] <- 23
mrg <- mrg[mrg$Group != "Hispanic2", ]

# add district and school names
mastersch <- mastersch[c("schnumb", "distcode", "distname", "schname")]
mrg$DistrictName <- mastersch$distname[match(mrg$DistrictCode, mastersch$distcode)]
mrg$DistrictName[mrg$DistrictCode == 100] <- "All State Charters"
mrg$DistrictName[mrg$DistrictCode == 999] <- "Statewide"
mrg$SchoolName <- mastersch$schname[match(mrg$schnumb, mastersch$schnumb)]
mrg$SchoolName[is.na(mrg$SchoolName) & mrg$SORT == 1] <- "All Schools"
mrg$SchoolName[is.na(mrg$SchoolName) & mrg$SORT == 2] <- "Districtwide"

mrg <- mrg[c("schnumb", "DistrictCode", "DistrictName", 
             "SchoolCode", "SchoolName", "Group",
             "Numerator", "Denominator", "NStudents", "GradRate", 
             "SORT", "SORTCODE")]

mrg$schnumb[mrg$schnumb == 999999] <- 000000
mrg$SORT <- NULL
mrg <- mrg[order(mrg$schnumb, mrg$SORTCODE), ]

missing <- mrg[is.na(mrg$SchoolName), ]
table(missing$schnumb)

mrg <- mrg[!is.na(mrg$SchoolName), ]
nrow(mrg)
# 4 yr cohort of 2018: N=6901
# 5 yr cohort of 2017: N=5419
# 6 yr cohort of 2016: N=5538

# delete district rates for state supported schools
mrg <- mrg[!(mrg$schnumb == 93000), ] #NM School for the Deaf
mrg <- mrg[!(mrg$schnumb == 94000), ] #NM School for the Blind and Visually Impaired
mrg <- mrg[!(mrg$schnumb == 95000), ] #Childrens Psychiatry
mrg <- mrg[!(mrg$schnumb == 97000), ] #Sequoyah Adolescent Treatment Center


# delete homebound for state charters
mrg <- mrg[!(mrg$DistrictCode > 500 & mrg$SchoolCode == 998), ]
table(mrg$schnumb[mrg$DistrictCode > 500])

# delete closed schools
mrg <- mrg[!(mrg$schnumb == 1007), ] #Bataan Charter
mrg <- mrg[!(mrg$schnumb == 1015), ] #Career Academic & Technical Academy
mrg <- mrg[!(mrg$schnumb == 1087), ] #Charter Vo-Tech Center
mrg <- mrg[!(mrg$schnumb == 1124), ] #Life Skills Center of Academy
mrg <- mrg[!(mrg$schnumb == 1594), ] #Sierra Alternative High
mrg <- mrg[!(mrg$schnumb == 1981), ] #Stronghurst Alternative School
mrg <- mrg[!(mrg$schnumb == 12038), ] #New Visions Program
mrg <- mrg[!(mrg$schnumb == 18002), ] #ACE Alternative Complex Education
mrg <- mrg[!(mrg$schnumb == 33015), ] #Hobbs Alternative High
mrg <- mrg[!(mrg$schnumb == 42002), ] #Deming Secure School
mrg <- mrg[!(mrg$schnumb == 45100), ] #Valmora High
mrg <- mrg[!(mrg$schnumb == 57020), ] #Broad Horizons Center
mrg <- mrg[!(mrg$schnumb == 69002), ] #Bridge Academy Charter
mrg <- mrg[!(mrg$schnumb == 80068), ] #Estancia Alternative
mrg <- mrg[!(mrg$schnumb == 88008), ] #Progressive Learning Center
mrg <- mrg[!(mrg$schnumb == 99002), ] #Sierra Blanca High School
mrg <- mrg[!(mrg$schnumb == 99005), ] #Lincoln Pines
mrg <- mrg[!(mrg$DistrictCode == 513), ] #Creative Ed Prep#1
mrg <- mrg[!(mrg$DistrictCode == 537), ] #The Learning Community
mrg <- mrg[!(mrg$DistrictCode == 558), ] #Health Sciences Academy
mrg <- mrg[!(mrg$DistrictCode == 556), ] #Anthony Charter School
nrow(mrg)
# 4 yr cohort of 2018: N=6711
# 5 yr cohort of 2017: N=5336
# 6 yr cohort of 2016: N=5350

#####################
# 4 yr cohort of 2018 - schools without names
mrg[is.na(mrg$SchoolName), ]
mrg[is.na(mrg$DistrictName), ]
# 8000, 17000, 37000, 62990
# 1997, 43997, 46997, 55997, 71997, 73997, 89997
# 55188 Espanola - Victory Christian
# 62990 Cuba - Amikids Sponsored by Amkids Inct

####################
# 5yr cohort of 2017 - schools without names
# 1997, 17997, 43997, 55997, 64999, 70999, 71997, 78997, 89997
# 17000, 95000
# 1007 Bataan Charter - closed in 2017
# 1038 Learning Community Charter - closed in 2015
# 42002 Deming Secure School - closed
# 42045 Mimbres Valley HS - opened in 2018, no 5-yr cohort
# 62990 Cuba - Amikids Sponsored by Amkids Inct
# 80100 Estancia Valley Learning - closed in 2017
# 513001 Creative Education Preparatory 1 Charter - closed in 2017
# 558001 Health Sciences Academy - closed in 2016

####################
# 6yr cohort of 2016 - schools without names
# 1997, 8999, 43997, 46997, 55997, 71997, 84999
# 1750 Simbra Leadership HS: opened in 2017, no 6yr cohort
# 71166 Early College Opportunities: opened in 2017, no 6yr cohort
# 62990 Cuba - Amikids Sponsored by Amkids Inct

################################################################################





################################################################################
# save output: remove schools that do not have a graduating class
################################################################################
# 4 yr cohort of 2018
cohort4 <- mrg

# 2018 - keep
cohort4[cohort4$schnumb == 17019, ] #Arrorhead Park Medical Academy
#first class in 2018

cohort4[cohort4$schnumb == 19018, ] #Alta Vista Early College HS
#first class in 2018

cohort4[cohort4$schnumb == 20035, ] #Carlsbad Early College HS
#first class in 2018

cohort4[cohort4$schnumb == 517001, ] #ABQ Sign Language Academy
#first class in 2018

cohort4[cohort4$schnumb == 542001, ] #Mission Achievement Succss(MAS)
#first class in 2018

cohort4[cohort4$schnumb == 557001, ] #Explore Academy
#first class in 2018

cohort4[cohort4$schnumb == 71169, ] #Santa Fe Engage
#first class in 2018

# 2019
cohort4[cohort4$schnumb == 1781, ] #International School at Mesa del Sol
cohort4 <- cohort4[cohort4$schnumb != 1781, ] #first class in 2019

cohort4[cohort4$schnumb == 4133, ] #Early College HS (Roswell)
cohort4 <- cohort4[cohort4$schnumb != 4133, ] #first class in 2019

cohort4[cohort4$schnumb == 561001, ] #Technology Leadership HS
cohort4 <- cohort4[cohort4$schnumb != 561001, ] #first class in 2019

cohort4[cohort4$schnumb == 71173, ] #Mandela International Magnet (MIMS)
cohort4 <- cohort4[cohort4$schnumb != 71173, ] #first class in 2019

# 2020
cohort4[cohort4$schnumb == 1750, ] #Siembra Leadership
cohort4 <- cohort4[cohort4$schnumb != 1750, ] #first class in 2020

cohort4[cohort4$schnumb == 41079, ] #Topper Freshman Academy
cohort4 <- cohort4[cohort4$schnumb != 41079, ] #first classs in 2020

cohort4[cohort4$schnumb == 568001, ] #Six Directions Indigenous School
cohort4 <- cohort4[cohort4$schnumb != 568001, ] #first class in 2020

cohort4[cohort4$schnumb == 572001, ] #Student Athelete Headquarters (SAHQ)
cohort4 <- cohort4[cohort4$schnumb != 572001, ] #first class in 2020

cohort4[cohort4$schnumb == 65180, ] #San Juan College HS
cohort4 <- cohort4[cohort4$schnumb != 65180, ] #first class in 2020

cohort4[cohort4$schnumb == 71166, ] #Early College Opportunities (ECO)
cohort4 <- cohort4[cohort4$schnumb != 71166, ] #first class in 2020

# 2021
cohort4[cohort4$schnumb == 42045, ] #Mimbres Valley Alternative School
cohort4 <- cohort4[cohort4$schnumb != 42045, ] #first class in 2021

cohort4[cohort4$schnumb == 562001, ] #DEAP
cohort4 <- cohort4[cohort4$schnumb != 562001, ] #first class in 2021

# 2022
cohort4[cohort4$schnumb == 568001, ] #Six Directions Indigenous School
cohort4 <- cohort4[cohort4$schnumb != 568001, ] #first class in 2022

nrow(cohort4) #6607

# save output
write.csv(cohort4, "Grad Rates of 4yr Cohort of 2018 UNMASKED.csv", 
          row.names = FALSE, na = "")


################################################################################
# 5 year cohort of 2017
cohort5 <- mrg

# 2019
cohort5[cohort5$schnumb == 17019, ] #Arrorhead Park Medical Academy
cohort5 <- cohort5[cohort5$schnumb != 17019, ] #first class in 2019

cohort5[cohort5$schnumb == 19018, ] #Alta Vista Early College HS
cohort5 <- cohort5[cohort5$schnumb != 19018, ] #first class in 2019

cohort5[cohort5$schnumb == 20035, ] #Carlsbad Early College HS
cohort5 <- cohort5[cohort5$schnumb != 20035, ] #first class in 2019

cohort5[cohort5$schnumb == 517001, ] #ABQ Sign Language Academy
cohort5 <- cohort5[cohort5$schnumb != 517001, ] #first class in 2019

cohort5[cohort5$schnumb == 542001, ] #Mission Achievement Succss(MAS)
cohort5 <- cohort5[cohort5$schnumb != 542001, ] #first class in 2019

cohort5[cohort5$schnumb == 557001, ] #Explore Academy
cohort5 <- cohort5[cohort5$schnumb != 557001, ] #first class in 2019

cohort5[cohort5$schnumb == 71169, ] #Santa Fe Engage
cohort5 <- cohort5[cohort5$schnumb != 71169, ] #first class in 2019

# 2020
cohort5[cohort5$schnumb == 1781, ] #International School at Mesa del Sol
cohort5 <- cohort5[cohort5$schnumb != 1781, ] #first class in 2020

cohort5[cohort5$schnumb == 4133, ] #Early College HS (Roswell)
cohort5 <- cohort5[cohort5$schnumb != 4133, ] #first class in 2020

cohort5[cohort5$schnumb == 561001, ] #Technology Leadership HS
cohort5 <- cohort5[cohort5$schnumb != 561001, ] #first class in 2020

cohort5[cohort5$schnumb == 71173, ] #Mandela International Magnet (MIMS)
cohort5 <- cohort5[cohort5$schnumb != 71173, ] #first class in 2020

# 2021
cohort5[cohort5$schnumb == 1750, ] #Siembra Leadership
cohort5 <- cohort5[cohort5$schnumb != 1750, ] #first class in 2021

cohort5[cohort5$schnumb == 41079, ] #Topper Freshman Academy
cohort5 <- cohort5[cohort5$schnumb != 41079, ] #first classs in 2021

cohort5[cohort5$schnumb == 568001, ] #Six Directions Indigenous School
cohort5 <- cohort5[cohort5$schnumb != 568001, ] #first class in 2021

cohort5[cohort5$schnumb == 572001, ] #Student Athelete Headquarters (SAHQ)
cohort5 <- cohort5[cohort5$schnumb != 572001, ] #first class in 2021

cohort5[cohort5$schnumb == 65180, ] #San Juan College HS
cohort5 <- cohort5[cohort5$schnumb != 65180, ] #first class in 2021

cohort5[cohort5$schnumb == 71166, ] #Early College Opportunities (ECO)
cohort5 <- cohort5[cohort5$schnumb != 71166, ] #first class in 2021

# 2022
cohort5[cohort5$schnumb == 42045, ] #Mimbres Valley Alternative School
cohort5 <- cohort5[cohort5$schnumb != 42045, ] #first class in 2022

cohort5[cohort5$schnumb == 562001, ] #DEAP
cohort5 <- cohort5[cohort5$schnumb != 562001, ] #first class in 2022

# 2023
cohort5[cohort5$schnumb == 568001, ] #Six Directions Indigenous School
cohort5 <- cohort5[cohort5$schnumb != 568001, ] #first class in 2023

nrow(cohort5) #5231

# save output
write.csv(cohort5, "Grad Rates of 5yr Cohort of 2017 UNMASKED.csv", 
          row.names = FALSE, na = "")


################################################################################
# 6 year cohort of 2016
cohort6 <- mrg

# 2019
cohort6[cohort6$schnumb == 17019, ] #Arrorhead Park Medical Academy
cohort6 <- cohort6[cohort6$schnumb != 17019, ] #first class in 2019

cohort6[cohort6$schnumb == 19018, ] #Alta Vista Early College HS
cohort6 <- cohort6[cohort6$schnumb != 19018, ] #first class in 2019

cohort6[cohort6$schnumb == 20035, ] #Carlsbad Early College HS
cohort6 <- cohort6[cohort6$schnumb != 20035, ] #first class in 2019

cohort6[cohort6$schnumb == 517001, ] #ABQ Sign Language Academy
cohort6 <- cohort6[cohort6$schnumb != 517001, ] #first class in 2019

cohort6[cohort6$schnumb == 542001, ] #Mission Achievement Succss(MAS)
cohort6 <- cohort6[cohort6$schnumb != 542001, ] #first class in 2019

cohort6[cohort6$schnumb == 557001, ] #Explore Academy
cohort6 <- cohort6[cohort6$schnumb != 557001, ] #first class in 2019

cohort6[cohort6$schnumb == 71169, ] #Santa Fe Engage
cohort6 <- cohort6[cohort6$schnumb != 71169, ] #first class in 2019

# 2020
cohort6[cohort6$schnumb == 1781, ] #International School at Mesa del Sol
cohort6 <- cohort6[cohort6$schnumb != 1781, ] #first class in 2020

cohort6[cohort6$schnumb == 4133, ] #Early College HS (Roswell)
cohort6 <- cohort6[cohort6$schnumb != 4133, ] #first class in 2020

cohort6[cohort6$schnumb == 561001, ] #Technology Leadership HS
cohort6 <- cohort6[cohort6$schnumb != 561001, ] #first class in 2020

cohort6[cohort6$schnumb == 71173, ] #Mandela International Magnet (MIMS)
cohort6 <- cohort6[cohort6$schnumb != 71173, ] #first class in 2020

# 2021
cohort6[cohort6$schnumb == 1750, ] #Siembra Leadership
cohort6 <- cohort6[cohort6$schnumb != 1750, ] #first class in 2021

cohort6[cohort6$schnumb == 41079, ] #Topper Freshman Academy
cohort6 <- cohort6[cohort6$schnumb != 41079, ] #first classs in 2021

cohort6[cohort6$schnumb == 568001, ] #Six Directions Indigenous School
cohort6 <- cohort6[cohort6$schnumb != 568001, ] #first class in 2021

cohort6[cohort6$schnumb == 572001, ] #Student Athelete Headquarters (SAHQ)
cohort6 <- cohort6[cohort6$schnumb != 572001, ] #first class in 2021

cohort6[cohort6$schnumb == 65180, ] #San Juan College HS
cohort6 <- cohort6[cohort6$schnumb != 65180, ] #first class in 2021

cohort6[cohort6$schnumb == 71166, ] #Early College Opportunities (ECO)
cohort6 <- cohort6[cohort6$schnumb != 71166, ] #first class in 2021

# 2022
cohort6[cohort6$schnumb == 42045, ] #Mimbres Valley Alternative School
cohort6 <- cohort6[cohort6$schnumb != 42045, ] #first class in 2022

cohort6[cohort6$schnumb == 562001, ] #DEAP
cohort6 <- cohort6[cohort6$schnumb != 562001, ] #first class in 2022

# 2023
cohort6[cohort6$schnumb == 568001, ] #Six Directions Indigenous School
cohort6 <- cohort6[cohort6$schnumb != 568001, ] #first class in 2023

nrow(cohort6) #5292

# save output
write.csv(cohort6, "Grad Rates of 6yr Cohort of 2016 UNMASKED.csv", 
          row.names = FALSE, na = "")

################################################################################





################################################################################
## SOAP files
################################################################################
SOAPfile <- function(file) {
    SOAP <- file[file$SORTCODE != 17, ]
    SOAP <- SOAP[SOAP$SORTCODE != 18, ]
    SOAP <- SOAP[SOAP$SORTCODE != 19, ]
    SOAP <- SOAP[SOAP$SORTCODE != 20, ]
    SOAP <- SOAP[SOAP$SORTCODE != 21, ]
    SOAP <- SOAP[SOAP$SORTCODE != 22, ]
    SOAP <- SOAP[SOAP$SORTCODE != 23, ]
    SOAP <- SOAP[c("DistrictCode", "SchoolCode", "DistrictName", "SchoolName", 
                   "Group", "GradRate")]
    SOAP <- SOAP[SOAP$DistrictCode != 999, ]
    SOAP <- SOAP[SOAP$DistrictCode != 100, ]
}

# 4yr cohort
SOAPdf4 <- SOAPfile(file = read.csv(
    "Grad Rates of 4yr Cohort of 2018 UNMASKED.csv", 
    header = TRUE, stringsAsFactors = FALSE))
# save output
current_date <- Sys.Date()
file_name <- paste0("SOAP 4yr Cohort of 2018 UNMASKED ", current_date, ".csv")
write.csv(SOAPdf4, file = file_name, row.names = FALSE, na = "")


# 5yr cohort
SOAPdf5 <- SOAPfile(file = read.csv(
    "Grad Rates of 5yr Cohort of 2017 UNMASKED.csv", 
    header = TRUE, stringsAsFactors = FALSE))
# save output
current_date <- Sys.Date()
file_name <- paste0("SOAP 5yr Cohort of 2017 UNMASKED ", current_date, ".csv")
write.csv(SOAPdf5, file = file_name, row.names = FALSE, na = "")


# 6yr cohort
SOAPdf6 <- SOAPfile(file = read.csv(
    "Grad Rates of 6yr Cohort of 2016 UNMASKED.csv", 
    header = TRUE, stringsAsFactors = FALSE))
# save output
current_date <- Sys.Date()
file_name <- paste0("SOAP 6yr Cohort of 2016 UNMASKED ", current_date, ".csv")
write.csv(SOAPdf6, file = file_name, row.names = FALSE, na = "")

################################################################################





################################################################################
## web files
################################################################################
webfile <- function(file) {
    web <- file
    web$Numerator <- NULL
    web$Denominator <- NULL
    web$GradRate[web$NStudents < 10] <- "*"
    web$NStudents <- NULL
    web <- web[web$SORTCODE != 17, ]
    web <- web[web$SORTCODE != 18, ]
    web <- web[web$SORTCODE != 19, ]
    web <- web[web$SORTCODE != 20, ]
    web <- web[web$SORTCODE != 21, ]
    web <- web[web$SORTCODE != 22, ]
    web <- web[web$SORTCODE != 23, ]
    web$SORTCODE <- NULL
    web$DistrictCode <- NULL
    web$SchoolCode <- NULL
    names(web) <- c("Code", "District", "School", "Group", "Rate (%)")
    web <- web[web$District != "All State Charters", ]
    web
}

# 4yr cohort
WEBdf4 <- webfile(file = read.csv(
    "Grad Rates of 4yr Cohort of 2018 UNMASKED.csv", 
    header = TRUE, stringsAsFactors = FALSE))
# save output
current_date <- Sys.Date()
file_name <- paste0("4yr Cohort of 2018 MASKED Web ", current_date, ".csv")
write.csv(WEBdf4, file = file_name, row.names = FALSE, na = "")


# 5yr cohort
WEBdf5 <- webfile(file = read.csv(
    "Grad Rates of 5yr Cohort of 2017 UNMASKED.csv", 
    header = TRUE, stringsAsFactors = FALSE))
# save output
current_date <- Sys.Date()
file_name <- paste0("5yr Cohort of 2017 MASKED Web ", current_date, ".csv")
write.csv(WEBdf5, file = file_name, row.names = FALSE, na = "")


# 6yr cohort
WEBdf6 <- webfile(file = read.csv(
    "Grad Rates of 6yr Cohort of 2016 UNMASKED.csv", 
    header = TRUE, stringsAsFactors = FALSE))
# save ouput
current_date <- Sys.Date()
file_name <- paste0("6yr Cohort of 2016 MASKED Web ", current_date, ".csv")
write.csv(WEBdf6, file = file_name, row.names = FALSE, na = "")

################################################################################
# demographics for web files
demographics <- function(file) {
    demo <- file[c("Group", "NStudents")]
    demo$Percent <- (demo$NStudents / demo$NStudents[demo$Group == "All Students"]) * 100
    demo$Percent <- round(demo$Percent, digits = 1)
    names(demo) <- c("Group", "N", "%")
    demo
}


# 4yr cohort
demo4 <- demographics(file = read.csv(
    "Grad Rates of 4yr Cohort of 2018 UNMASKED.csv", 
    header = TRUE, stringsAsFactors = FALSE, nrow = 16))
# save output
current_date <- Sys.Date()
file_name <- paste0("4yr Cohort of 2018 MASKED Web Demographics ", 
                    current_date, ".csv")
write.csv(demo4, file = file_name, row.names = FALSE, na = "")


# 5yr cohort
demo5 <- demographics(file = read.csv(
    "Grad Rates of 5yr Cohort of 2017 UNMASKED.csv", 
    header = TRUE, stringsAsFactors = FALSE, nrow = 13))
# save output
current_date <- Sys.Date()
file_name <- paste0("5yr Cohort of 2017 MASKED Web Demographics ",
                    current_date, ".csv")
write.csv(demo5, file = file_name, row.names = FALSE, na = "")


# 6yr cohort
demo6 <- demographics(file = read.csv(
    "Grad Rates of 6yr Cohort of 2016 UNMASKED.csv", 
    header = TRUE, stringsAsFactors = FALSE, nrow = 13))
# save output
current_date <- Sys.Date()
file_name <- paste0("6yr Cohort of 2016 MASKED Web Demographics ",
                    current_date, ".csv")
write.csv(demo6, file = file_name, row.names = FALSE, na = "")
################################################################################

rm(list = ls())
