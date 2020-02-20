##program cleans and appends 2016-2018 Survey of Economics and Decisionmaking public
##data from the Division of Consumer & Community Affairs at the Federal Reserve Board
##of Governors: https://www.federalreserve.gov/consumerscommunities/shed.htm

library(tidyverse)

shed18 <- read_csv("inputs/public2018.csv", col_types = cols(.default = "c"), 
                   ##encoding so apostrophes are read-in appropriately
                   locale=locale(encoding = "windows-1252")) %>%
  add_column(year = 2018) %>%
  ##population weight varies by year
  rename(weight = weight2b)
shed17 <- read_csv("inputs/SHED_2017_Public_Use.csv", col_types = cols(.default = "c"), 
                   locale=locale(encoding = "UTF-8")) %>%
  add_column(year = 2017) %>%
  rename(weight = weight3b)
shed16 <- read_csv("inputs/SHED_2016_Public_Data.csv", col_types = cols(.default = "c"), 
                   locale=locale(encoding = "windows-1252")) %>%
  add_column(year = 2016) %>%
  ##income under different variable name in 2016
  rename(I40 = I4A, 
         weight = weight3b)


shed <- shed18 %>%
  bind_rows(shed17, shed16) %>%
  select(year, ED0, ppcm0160, I40, B2, weight, D20, D30, D1I, SL1, SL3, K0,
         I9, EF1, CH2, CH3) %>%
  rename(Category = ppcm0160, 
         Education = ED0,
         Income = I40,
         WellBeing = B2,
         MoreWork = D20, 
         ScheduleVaries = D30, 
         StudentLoan = SL1,
         StudentLoanAmt = SL3,
         RetirementTrack = K0,
         IncomeVaries = I9, 
         EmergSavings = EF1, 
         MotherEdu = CH2, 
         FatherEdu = CH3) %>%
  mutate(Category = na_if(Category, "Not asked"),
         Category = na_if(Category, "Other (Please specify)"), 
         Category = na_if(Category, "Refused"), 
         Category = na_if(Category, "Other professional")) %>% 
  ##combining & renaming categories for visualization
  mutate(Industry = case_when(Category == "Teacher, college and university" | 
                                Category == "Teacher, except college and university" | 
                                Category == "Education, Training, and Library" ~
                                  "Education", 
                              Category == "Armed Services" |
                                Category == "Protective Service" ~ 
                                  "Military & police", 
                              Category == "Health Care Support (such as nursing aide, orderly, dental assistant)" |
                                Category == "Health Diagnosing or Treating Practitioner (such as physician, nurse, dentist, v" |
                                Category == "Other Health Care Practitioner (such as nurse, pharmacist, chiropractor, dietici" |
                                Category == "Other Health Care Practitioner (such as nurse, pharmacist, chiropractor, dietician)" |
                                Category == "Health Diagnosing or Treating Practitioner (such as physician, nurse, dentist, veterinarian, pharmacist)" |
                                Category == "Health Technologist or Technician (such as paramedic, lab technician)" |
                                Category == "Medical Doctor (such as physician, surgeon, dentist, veterinarian)" ~
                                "Healthcare",
                              Category == "Business and Financial Operations" |
                                Category == "Financial Operations or Financial Services (including Financial Advisor, Broker)" |
                                Category == "Business Operations (including Marketing)" ~
                                "Business & Finance", 
                              Category == "Architecture and Engineering" |
                                Category == "Computer and Mathematical" |
                                Category == "Life, Physical, and Social Sciences" ~
                                "Non-healthcare STEM", 
                              Category == "Retail Sales" |
                                Category == "Sales Representative" |
                                Category == "Other Sales" ~
                                "Sales", 
                              Category == "Building and Grounds Cleaning and Maintenance" |
                                Category == "Construction and Extraction" |
                                Category == "Installation, Maintenance, and Repair" ~
                                "Maintenance & construction",
                              Category == "Arts, Design, Entertainment, Sports, and Media" ~
                                "Arts & entertainment",
                              Category == "Farming, Forestry, and Fishing" ~
                                "Agriculture",
                              Category == "Food Preparation and Serving" ~
                                "Food service",
                              Category == "Lawyer, judge" ~
                                "Legal",
                              Category == "Community and Social Services" ~
                                "Social services",
                              Category == "Office and Administrative Support" ~
                                "Administrative services",
                              Category == "Personal Care and Service" ~
                                "Personal services",
                              Category == "Precision Production (such as machinist, welder, baker, printer, tailor)" ~
                                "Precision production",
                              Category == "Transportation and Material Moving" ~
                                "Transportation",
                                TRUE ~ Category)) %>%
  mutate(Education = recode(Education, "Professional degree (e.g. MBA, MD, JD)" = 
                              "Professional degree"), 
         Education = recode(Education, "Some college but no degree (including currently enrolled in college)" = 
                              "Some college, no degree"), 
         Education = recode(Education, "Doctoral Degree" =
                              "Doctoral degree")) %>%
  ##aggregating graduate degrees for parent analysis
  mutate(EducCompressed = if_else(Education == "Master’s degree" |
                                     Education == "Professional degree" |
                                     Education == "Doctoral degree", "Graduate degree", 
                                     Education)) %>%
  ##aggregating income variables
  mutate(Income = recode(Income, "$0" = "$0 to $4,999"), 
         Income = recode(Income, "$1 to $4,999" = "$0 to $4,999"), 
         Income = recode(Income, "0" = "$0 to $4,999"))

  # mutate(Industry = case_when(Category == "Teacher, college and university" | 
  #                               Category == "Teacher, except college and university" ~ "Education", 
  #                             Category == "Business and Financial Operations" |
  #                               Category == "Financial Operations or Financial Services (including Financial Advisor, Broker)" ~
  #                               "Business or Finance"))

# 
#   mutate(Industry = recode(Category, "Teacher, college and university" = "Education"), 
#          Industry = recode(Industry, "Teacher, except college and university" = "Education"), 
#          Industry = recode(Industry, "Education, Training, and Library" = "Education"), 
#          Industry = recode(Industry, "Business Operations (including Marketing)" = "Business or finance"), 
#          Industry = recode(Industry, "Health Care Support (such as nursing aide, orderly, dental assistant)" 
#                            = "Healthcare"),
#          Industry = recode(Industry, "Health Diagnosing or Treating Practitioner (such as physician, nurse, dentist, v" 
#                            = "Healthcare"),
#          Industry = recode(Industry, "Health Technologist or Technician (such as paramedic, lab technician)" 
#                            = "Healthcare"), 
#          Industry = recode(Industry, "Medical Doctor (such as physician, surgeon, dentist, veterinarian)" 
#                            = "Healthcare"), 
#          Industry = recode(Industry, "Other Health Care Practitioner (such as nurse, pharmacist, chiropractor, dietici" 
#                            = "Healthcare"), 
#          Industry = recode(Industry, "Sales Representative" = "Sales"), 
#          Industry = recode(Industry, "Retail Sales" = "Sales")) 

                           


write.csv(shed, "outputs/shed.csv")