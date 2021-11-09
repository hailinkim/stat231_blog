# Import data
nhis19 <- read_csv("adult19.csv")

demographic <- nhis19 %>% 
  select(SEX_A, ORIENT_A, HISPALLP_A, EDUC_A, NOTCOV_A, CITZNSTP_A, YRSINUS_A, 
         FDSCAT3_A, INCGRP_A, FAMINCTC_A, MHTHDLY_A, MHTHND_A, PAYBLL12M_A, 
         DENDL12M_A, DENNG12M_A, RXDG12M_A, RXLS12M_A, RXSK12M_A, DENNG12M_A) %>% 
  mutate(
    sex = case_when(
      SEX_A == 1 ~ "Male",
      SEX_A == 2 ~ "Female",
      TRUE ~ "Other"
    ),
    sex_orientation = case_when(
      ORIENT_A == 1 ~ "Gay/Lesbian",
      ORIENT_A == 2 ~ "Straight",
      ORIENT_A == 3 ~ "Bisexual",
      ORIENT_A == 4 ~ "Something else",
      TRUE ~ "Other"
    ),
    race = case_when(
      HISPALLP_A == 01 ~ "Hispanic",
      HISPALLP_A == 02 ~ "White",
      HISPALLP_A == 03 ~ "Black/African American",
      HISPALLP_A == 04 ~ "Asian",
      HISPALLP_A == 05 ~ "American Indian and Alaska Native",
      HISPALLP_A == 06 ~ "American Indian and Alaska Native",
      HISPALLP_A == 07 ~ "Other single and multiple races",
      TRUE ~ "Other"
    ),
    edu = case_when(
      EDUC_A == 00 ~ "Never attended/kindergarten only",
      EDUC_A == 01 ~ "Less than a high school diploma",
      EDUC_A == 02 ~ "Less than a high school diploma",
      EDUC_A == 03 ~ "GED or equivalent",
      EDUC_A == 04 ~ "High school graduate",
      EDUC_A == 05 ~ "Some college, no degree",
      EDUC_A == 06 ~ "Associate degree",
      EDUC_A == 07 ~ "Associate degree",
      EDUC_A == 08 ~ "Bachelor's degree",
      EDUC_A == 09 ~ "Master's degree",
      EDUC_A == 10 ~ "Professional school degree",
      EDUC_A == 11 ~ "Doctoral degree",
      TRUE ~ "Other"
    ),
    coverage = case_when(
      NOTCOV_A == 1 ~ "Not covered",
      NOTCOV_A == 2 ~ "Covered",
      TRUE ~ "Other"
    ),
    citizen = case_when(
      CITZNSTP_A == 1 ~ "US Citizen",
      CITZNSTP_A == 2 ~ "Non-US Citizen",
      TRUE ~ "Other"
    ),
    years_in_us = case_when(
      YRSINUS_A == 1 ~ "Less than 1 year",
      YRSINUS_A == 2 ~ "1 to less than 5 years",
      YRSINUS_A == 3 ~ "5 to less than 10 years",
      YRSINUS_A == 4 ~ "10 to less than 15 years",
      YRSINUS_A == 5 ~ "15 years or more",
      TRUE ~ "Unknown"
    ),
    citizen_years = paste0(citizen, ", ", years_in_us),
    citizen_years = case_when(
      str_detect(citizen_years, "^US Citizen") ~ "US Citizen",
      TRUE ~ citizen_years
    ),
    food_security = case_when(
      FDSCAT3_A == 1 ~ "Food secure",
      FDSCAT3_A == 2 ~ "Low food security",
      FDSCAT3_A == 3 ~ "Very low food security",
      TRUE ~ "Other"
    ),
    income_group = case_when(
      INCGRP_A == 1 ~ "$0 to $34,999",
      INCGRP_A == 2 ~ "$35,000 to $49,999",
      INCGRP_A == 3 ~ "$50,000 to $74,999",
      INCGRP_A == 4 ~ "$75,000 to $99,999",
      INCGRP_A == 5 ~ "$100,000 or greater",
      TRUE ~ "Other"
    ),
    delayed_mental = case_when(
      MHTHDLY_A == 1 ~ "Yes",
      MHTHDLY_A == 2 ~ "No",
      TRUE ~ "Other"
    ),
    unafford_mental = case_when(
      MHTHND_A == 1 ~ "Yes",
      MHTHND_A == 2 ~ "No",
      TRUE ~ "Other"
    ),
    bill_problem = case_when(
      PAYBLL12M_A == 1 ~ "Yes",
      PAYBLL12M_A == 2 ~ "No",
      TRUE ~ "Other"
    ),
    delayed_dental = case_when(
      DENDL12M_A == 1 ~ "Yes",
      DENDL12M_A == 2 ~ "No",
      TRUE ~ "Other"
    ),
    unafford_dental = case_when(
      DENNG12M_A == 1 ~ "Yes",
      DENNG12M_A == 2 ~ "No",
      TRUE ~ "Other"
    ),
    delayed_medical = case_when(
      DENNG12M_A == 1 ~ "Yes",
      DENNG12M_A == 2 ~ "No",
      TRUE ~ "Other"
    ),
    unafford_medical = case_when(
      DENNG12M_A == 1 ~ "Yes",
      DENNG12M_A == 2 ~ "No",
      TRUE ~ "Other"
    ),
    skip_med = case_when(
      RXSK12M_A == 1 ~ "Yes",
      RXSK12M_A == 2 ~ "No",
      TRUE ~ "Other"
    ),
    less_med = case_when(
      RXLS12M_A == 1 ~ "Yes",
      RXLS12M_A == 2 ~ "No",
      TRUE ~ "Other"
    ),
    unafford_med = case_when(
      RXDG12M_A == 1 ~ "Yes",
      RXDG12M_A == 2 ~ "No",
      TRUE ~ "Other"
    ),
    income = FAMINCTC_A 
  )


affordability <- demographic %>% 
  select(region, delayed_mental, unafford_mental) %>% 
  filter(delayed_mental == "Yes" | unafford_mental == "Yes") %>% 
  group_by(region) %>% 
  summarise(unafford_count = n()) %>% 
  inner_join(population, by = "region") %>% 
  mutate(unafford_prop = unafford_count/population) %>% 
  select(region, unafford_prop)

income <- demographic %>% 
  select(region, income) %>% 
  group_by(region) %>% 
  summarise(median_income = median(income))

food_security <- demographic %>% 
  select(region, food_security) %>% 
  filter(food_security=="Low food security" | food_security=="Very low food security") %>% 
  group_by(region) %>% 
  summarise(insecure_count = n()) %>% 
  inner_join(population, by = "region") %>% 
  mutate(insecure_prop = insecure_count/population) %>% 
  select(region, insecure_prop)

citizenship <- demographic %>% 
  select(region, citizen, years_in_us) %>% 
  filter(citizen == "Non-US Citizen" & 
           (years_in_us == "10 to less than 15 years" |  years_in_us == "15 years or more")) %>% 
  group_by(region) %>% 
  summarise(non_citizen_count = n()) %>% 
  inner_join(population, by = "region") %>% 
  mutate(non_citizen_prop = non_citizen_count/population) %>% 
  select(region, non_citizen_prop)

coverage <- demographic %>% 
  select(region, coverage) %>% 
  filter(coverage == "Not covered") %>% 
  group_by(region) %>% 
  summarise(uninsured_count = n()) %>% 
  inner_join(population, by = "region") %>% 
  mutate(uninsured_prop = uninsured_count/population) %>% 
  select(region, uninsured_prop)

socioeconomic <- income %>% 
  inner_join(food_security, by = "region") %>% 
  inner_join(coverage, by = "region") %>% 
  inner_join(citizenship, by = "region") %>% 
  inner_join(affordability, by = "region") %>% 
  inner_join(states2, by=c("region" = "region2")) %>% 
  dplyr::rename(state = region.y) 

#Export data set
write_csv(socioeconomic, "socioeconomic.csv")



#Data Wrangling for Barplot
distress <- demographic %>% 
  select(sex, sex_orientation, race, edu, food_security, income_group,
         anxiety, depression, citizen_years) %>% 
  filter(((anxiety != "None/Minimal")| (depression != "None/Minimal")),
         anxiety != "Other", depression != "Other") %>% 
  pivot_longer(c("anxiety", "depression"), names_to = "type", values_to = "degree")

#sexual orientation
population_sex <- demographic %>%
  filter(sex_orientation != "Other") %>% 
  group_by(sex_orientation) %>%
  summarise(population = n())

anxiety_sex <- distress %>% 
  filter(type == "anxiety", sex_orientation != "Other") %>% 
  mutate(
    anxiety2 = case_when(
      degree == "None/Minimal" ~ "No",
      TRUE ~ "Yes"
    )
  ) %>% 
  filter(anxiety2 == "Yes") %>% 
  select(sex_orientation, anxiety2) %>% 
  group_by(sex_orientation) %>% 
  summarize(count = n()) %>% 
  inner_join(population_sex, by = "sex_orientation") %>% 
  mutate(anxiety = count/population) %>% 
  select(sex_orientation, anxiety)

depression_sex <- distress %>% 
  filter(type == "depression", sex_orientation != "Other") %>% 
  mutate(
    depression2 = case_when(
      degree == "None/Minimal" ~ "No",
      TRUE ~ "Yes"
    )
  ) %>% 
  filter(depression2 == "Yes") %>% 
  select(sex_orientation, depression2) %>% 
  group_by(sex_orientation) %>% 
  summarize(count = n()) %>% 
  inner_join(population_sex, by = "sex_orientation") %>% 
  mutate(depression = count/population) %>% 
  select(sex_orientation, depression)
distress_sex <- anxiety_sex %>% 
  inner_join(depression_sex, by = "sex_orientation") %>% 
  pivot_longer(!sex_orientation, names_to = "type", values_to = "prop")

distress_sex$sex_orientation <- factor(distress_sex$sex_orientation, 
                                       level = rev(c("Straight", "Gay/Lesbian",
                                                     "Bisexual", "Something else")))

#race
population_race <- demographic %>%
  filter(race != "Other") %>% 
  group_by(race) %>%
  summarise(population = n())

anxiety_race <- distress %>% 
  filter(type == "anxiety", race != "Other") %>% 
  mutate(
    anxiety2 = case_when(
      degree == "None/Minimal" ~ "No",
      TRUE ~ "Yes"
    )
  ) %>% 
  filter(anxiety2 == "Yes") %>% 
  select(race, anxiety2) %>% 
  group_by(race) %>% 
  summarize(count = n()) %>% 
  inner_join(population_race, by = "race") %>% 
  mutate(anxiety = count/population) %>% 
  select(race, anxiety)

depression_race <- distress %>% 
  filter(type == "depression", race != "Other") %>% 
  mutate(
    depression2 = case_when(
      degree == "None/Minimal" ~ "No",
      TRUE ~ "Yes"
    )
  ) %>% 
  filter(depression2 == "Yes") %>% 
  select(race, depression2) %>% 
  group_by(race) %>% 
  summarize(count = n()) %>% 
  inner_join(population_race, by = "race") %>% 
  mutate(depression = count/population) %>% 
  select(race, depression)

distress_race <- anxiety_race %>% 
  inner_join(depression_race, by = "race") %>% 
  pivot_longer(!race, names_to = "type", values_to = "prop")

distress_race$race <- factor(distress_race$race, 
                             level = rev(c("White", "Black/African American",
                                           "Hispanic", "Asian", "American Indian and Alaska Native",
                                           "Other single and multiple races")))

#education level
population_edu <- demographic %>%
  filter(edu != "Other") %>% 
  group_by(edu) %>%
  summarise(population = n())

anxiety_edu <- distress %>% 
  filter(type == "anxiety", edu != "Other") %>% 
  mutate(
    anxiety2 = case_when(
      degree == "None/Minimal" ~ "No",
      TRUE ~ "Yes"
    )
  ) %>% 
  filter(anxiety2 == "Yes") %>% 
  select(edu, anxiety2) %>% 
  group_by(edu) %>% 
  summarize(count = n()) %>% 
  inner_join(population_edu, by = "edu") %>% 
  mutate(anxiety = count/population) %>% 
  select(edu, anxiety)

depression_edu <- distress %>% 
  filter(type == "depression", edu != "Other") %>% 
  mutate(
    depression2 = case_when(
      degree == "None/Minimal" ~ "No",
      TRUE ~ "Yes"
    )
  ) %>% 
  filter(depression2 == "Yes") %>% 
  select(edu, depression2) %>% 
  group_by(edu) %>% 
  summarize(count = n()) %>% 
  inner_join(population_edu, by = "edu") %>% 
  mutate(depression = count/population) %>% 
  select(edu, depression)

distress_edu <- anxiety_edu %>% 
  inner_join(depression_edu, by = "edu") %>% 
  pivot_longer(!edu, names_to = "type", values_to = "prop")

distress_edu$edu <- factor(distress_edu$edu, 
                           level = rev(c("Never attended/kindergarten only", 
                                         "Less than a high school diploma", "GED or equivalent",
                                         "High school graduate", "Some college, no degree",
                                         "Associate degree", "Bachelor's degree", "Master's degree",
                                         "Professional school degree", "Doctoral degree")))


#citizenship status
population_citizen <- demographic %>% 
  select(citizen_years) %>% 
  filter(!(str_detect(citizen_years, "Other|Unknown"))) %>% 
  group_by(citizen_years) %>% 
  summarize(population = n())

anxiety_citizen <- distress %>% 
  filter(type == "anxiety", !str_detect(citizen_years, "Other|Unknown")) %>% 
  mutate(
    anxiety2 = case_when(
      degree == "None/Minimal" ~ "No",
      TRUE ~ "Yes"
    )
  ) %>% 
  filter(anxiety2 == "Yes") %>% 
  select(citizen_years, anxiety2) %>% 
  group_by(citizen_years) %>% 
  summarize(count = n()) %>% 
  inner_join(population_citizen, by = "citizen_years") %>% 
  mutate(anxiety = count/population) %>% 
  select(citizen_years, anxiety)

depression_citizen <- distress %>% 
  filter(type == "depression", !str_detect(citizen_years, "Other|Unknown")) %>% 
  mutate(
    depression2 = case_when(
      degree == "None/Minimal" ~ "No",
      TRUE ~ "Yes"
    )
  ) %>% 
  filter(depression2 == "Yes") %>% 
  select(citizen_years, depression2) %>% 
  group_by(citizen_years) %>% 
  summarize(count = n()) %>% 
  inner_join(population_citizen, by = "citizen_years") %>% 
  mutate(depression = count/population) %>% 
  select(citizen_years, depression)

distress_citizen <- anxiety_citizen %>% 
  inner_join(depression_citizen, by = "citizen_years") %>% 
  pivot_longer(!citizen_years, names_to = "type", values_to = "prop")

distress_citizen$citizen_years <- factor(distress_citizen$citizen_years,
                                         level = rev(c("US Citizen",
                                                       "Non-US Citizen, 15 years or more",
                                                       "Non-US Citizen, 10 to less than 15 years",
                                                       "Non-US Citizen, 5 to less than 10 years",
                                                       "Non-US Citizen, 1 to less than 5 years",
                                                       "Non-US Citizen, Less than 1 year"
                                         )))

#food_security
population_food <- demographic %>%
  filter(food_security != "Other") %>% 
  group_by(food_security) %>%
  summarise(population = n())

anxiety_food <- distress %>% 
  filter(type == "anxiety", food_security != "Other") %>% 
  mutate(
    anxiety2 = case_when(
      degree == "None/Minimal" ~ "No",
      TRUE ~ "Yes"
    )
  ) %>% 
  filter(anxiety2 == "Yes") %>% 
  select(food_security, anxiety2) %>% 
  group_by(food_security) %>% 
  summarize(count = n()) %>% 
  inner_join(population_food, by = "food_security") %>% 
  mutate(anxiety = count/population) %>% 
  select(food_security, anxiety)

depression_food <- distress %>% 
  filter(type == "depression", food_security != "Other") %>% 
  mutate(
    depression2 = case_when(
      degree == "None/Minimal" ~ "No",
      TRUE ~ "Yes"
    )
  ) %>% 
  filter(depression2 == "Yes") %>% 
  select(food_security, depression2) %>% 
  group_by(food_security) %>% 
  summarize(count = n()) %>% 
  inner_join(population_food, by = "food_security") %>% 
  mutate(depression = count/population) %>% 
  select(food_security, depression)

distress_food <- anxiety_food %>% 
  inner_join(depression_food, by = "food_security") %>% 
  pivot_longer(!food_security, names_to = "type", values_to = "prop")

distress_food$food_insecurity <- factor(distress_food$food_security, 
                                        level = rev(c("Very low food security" , 
                                                      "Low food security",
                                                      "Food secure")))

#Income Group
population_income <- demographic %>%
  group_by(income_group) %>%
  summarise(population = n())

anxiety_income <- distress %>% 
  filter(type == "anxiety") %>% 
  mutate(
    anxiety2 = case_when(
      degree == "None/Minimal" ~ "No",
      TRUE ~ "Yes"
    )
  ) %>% 
  filter(anxiety2 == "Yes") %>% 
  select(income_group, anxiety2) %>% 
  group_by(income_group) %>% 
  summarize(count = n()) %>% 
  inner_join(population_income, by = "income_group") %>% 
  mutate(anxiety = count/population) %>% 
  select(income_group, anxiety)

depression_income <- distress %>% 
  filter(type=="depression") %>% 
  mutate(
    depression2 = case_when(
      degree == "None/Minimal" ~ "No",
      TRUE ~ "Yes"
    )
  ) %>% 
  filter(depression2 == "Yes") %>% 
  select(income_group, depression2) %>% 
  group_by(income_group) %>% 
  summarize(count = n()) %>% 
  inner_join(population_income, by = "income_group") %>% 
  mutate(depression = count/population) %>% 
  select(income_group, depression)

distress_income <- anxiety_income %>% 
  inner_join(depression_income, by = "income_group") %>% 
  pivot_longer(!income_group, names_to = "type", values_to = "prop")

distress_income$income_group <- factor(distress_income$income_group, 
                                       level = rev(c("$0 to $34,999" , "$35,000 to $49,999",
                                                     "$50,000 to $74,999", "$75,000 to $99,999",
                                                     "$100,000 or greater")))


#Export data sets for 1st barplot
distress_sex <- write_csv(distress_sex, "distress_sex.csv")
distress_race <- write_csv(distress_race, "distress_race.csv")
distress_income <- write_csv(distress_income, "distress_income.csv")
distress_food <- write_csv(distress_food, "distress_food.csv")
distress_edu <- write_csv(distress_edu, "distress_edu.csv")
distress_citizen <- write_csv(distress_citizen, "distress_citizen.csv")

#Data Wrangling for 2nd Barplot
distress2 <- distress %>% 
  select(-sex) %>% 
  filter(sex_orientation != "Other",
         edu != "Other",
         food_security != "Other",
         income_group != "Other",
         !(str_detect(citizen_years, "Other|Unknown")),
         degree != "None/Minimal"
  )

distress2$degree <- factor(distress2$degree,
                           level = c("Mild", "Moderate", "Severe", "Not Ascertained"))

#Export data set for 2nd barplot
distress2 <- write_csv(distress2, "distress2.csv")

#Data Wrangling for Table
mental <- nhis19 %>% 
  select (RACEALLP_A, HISP_A, SEX_A, ANXFREQ_A, DEPFREQ_A, ANXMED_A, DEPMED_A, 
          MHTHRPY_A, MHTHND_A, NOTCOV_A) %>% 
  mutate(
    #gender
    Gender = case_when(
      SEX_A == 1 ~ "Male",
      SEX_A == 2 ~ "Female",
      SEX_A == 7 ~ "Refused"
    ),
    #race
    Race = case_when(
      HISP_A == 1 ~ "Hispanic",
      RACEALLP_A == 1 ~ "White",
      RACEALLP_A == 2 ~ "Black/African American",
      RACEALLP_A == 4 ~ "AIAN",
      RACEALLP_A == 3 ~ "Asian",
      RACEALLP_A == 6 ~ "Other",
      TRUE ~ "Other"
    ),
    #couldn't afford mental healthcare
    # needed counseling/therapy but did not get it due to cost (MHTHND_A)
    Affordability = case_when(
      MHTHND_A == 1 ~ "Unaffordable",
      MHTHND_A == 2 ~ "Affordable",
      MHTHND_A == 7 ~ "Refused",
      MHTHND_A == 8 ~ "Not ascertained",
      MHTHND_A == 9 ~ "Don't know"
    ),
    
    Coverage = case_when(
      NOTCOV_A == 1 ~ "Uninsured",
      NOTCOV_A == 2 ~ "Insured",
      TRUE ~ "Other"
    ),
    
    #seen/talked to mental health professional
    # recieved counseling/therapy from mental health prof (MHTHRPY_A)
    "Recieved Couseling/Therapy past 12 months" = case_when(
      MHTHRPY_A == 1 ~ "Yes",
      MHTHRPY_A == 2 ~ "No",
      MHTHRPY_A == 7 ~ "Refused",
      MHTHRPY_A == 8 ~ "Not ascertained",
      MHTHRPY_A == 9 ~ "Don't know"
    ),
    #take medication for worried, nervous, anxious feelings and depression
    # took medicine for other emotions/concentration/behavior/mental health (MHRX_A)
    # take medication for anxious ANXMED_A
    # take meds for depression DEPMED_A
    "Takes Medication for Anxiety or Depression" = case_when(
      ANXMED_A == 1 ~ "Yes",
      ANXMED_A == 2 ~ "No",
      DEPMED_A == 1 ~ "Yes",
      DEPMED_A == 2 ~ "No",
      TRUE ~ "Other"
    ),
    # how often feel worried, nervous, or anxious
    "How Often Anxious" = case_when(
      ANXFREQ_A == 1 ~"Daily",
      ANXFREQ_A == 2 ~"Weekly",
      ANXFREQ_A == 3 ~"Monthly",
      ANXFREQ_A == 4 ~"A few times a year",
      ANXFREQ_A == 5 ~"Never",
      TRUE ~ "Other"
    ),
    # how often depressed
    "How Often Depressed" = case_when(
      DEPFREQ_A == 1 ~"Daily",
      DEPFREQ_A == 2 ~"Weekly",
      DEPFREQ_A == 3 ~"Monthly",
      DEPFREQ_A == 4 ~"A few times a year",
      DEPFREQ_A == 5 ~"Never",
      TRUE ~ "Other"
    )
  ) %>% 
  filter(Coverage != "Other") %>% 
  select(-c(1:10))

#Export data set for table
write_csv(mental, "mental.csv")

#Data Wrangling for Tobacco
tobacco <- nhis19 %>%
  mutate(
    sex = case_when(
      SEX_A == 1 ~ "Male",
      SEX_A == 2 ~ "Female"),
    cig_freq = case_when(
      SMKNOW_A == 1 ~ "Everyday",
      SMKNOW_A == 2 ~ "Some Days",
      SMKNOW_A == 3 ~ "Not at all",
      SMKNOW_A == 7 ~ "Refused",
      SMKNOW_A == 8 ~ "Not Ascertained",
      SMKNOW_A == 9 ~ "Don't Know"),
    cigar_freq = case_when(
      CIGARCUR_A == 1 ~ "Everyday",
      CIGARCUR_A == 2 ~ "Some Days",
      CIGARCUR_A == 3 ~ "Not at all",
      CIGARCUR_A == 7 ~ "Refused",
      CIGARCUR_A == 8 ~ "Not Ascertained",
      CIGARCUR_A == 9 ~ "Don't Know"),
    e_cig_freq = case_when(
      ECIGNOW_A == 1 ~ "Everyday",
      ECIGNOW_A == 2 ~ "Some Days",
      ECIGNOW_A == 3 ~ "Not at all",
      ECIGNOW_A == 7 ~ "Refused",
      ECIGNOW_A == 8 ~ "Not Ascertained",
      ECIGNOW_A == 9 ~ "Don't Know"),
    pipe_freq = case_when(
      PIPECUR_A == 1 ~ "Everyday",
      PIPECUR_A == 2 ~ "Some Days",
      PIPECUR_A == 3 ~ "Not at all",
      PIPECUR_A == 7 ~ "Refused",
      PIPECUR_A == 8 ~ "Not Ascertained",
      PIPECUR_A == 9 ~ "Don't Know"),
    smkless_freq = case_when(
      SMOKELSCUR_A == 1 ~ "Everyday",
      SMOKELSCUR_A == 2 ~ "Some Days",
      SMOKELSCUR_A == 3 ~ "Not at all",
      SMOKELSCUR_A == 7 ~ "Refused",
      SMOKELSCUR_A == 8 ~ "Not Ascertained",
      SMOKELSCUR_A == 9 ~ "Don't Know"),
    #Needed counseling/therapy but did not get it due to cost, past 12m
    afford_mental_care = case_when(
      MHTHND_A == 1 ~ "Yes",
      MHTHND_A == 2 ~ "No",
      MHTHND_A == 7 ~ "Refused",
      MHTHND_A == 8 ~ "Not Ascertained",
      MHTHND_A == 9 ~ "Don't Know")) %>% 
  filter(c(cig_freq != "Refused"|cig_freq != "Not Ascertained" |cig_freq != "Don't Know"),
         c(e_cig_freq != "Refused"|e_cig_freq != "Not Ascertained" |e_cig_freq != "Don't Know"),
         c(pipe_freq != "Refused"|pipe_freq != "Not Ascertained" |pipe_freq != "Don't Know"),
         c(smkless_freq != "Refused"| smkless_freq != "Not Ascertained" | smkless_freq!= "Don't Know"),
         afford_mental_care == "Yes" | afford_mental_care == "No") %>% 
  select(sex, AGEP_A, cig_freq, e_cig_freq, cigar_freq, pipe_freq, smkless_freq, afford_mental_care) %>% 
  drop_na()

tobacco_freq <- tobacco %>% 
  pivot_longer(-c(sex, AGEP_A, afford_mental_care),
               names_to = "Habit", values_to = "Frequency") %>% 
  mutate( Habit = case_when(
    Habit == "cig_freq" ~ "Cigarette use",
    Habit == "cigar_freq" ~"Cigars, cigarillos, or little filtered cigars use",
    Habit == "e_cig_freq" ~ "E-cigarette use",
    Habit == "pipe_freq" ~ "Pipe use",
    Habit == "smkless_freq" ~ "Smokeless tobacco use"))

#Export data set for tobacco
write_csv(tobacco_freq, "tobacco_freq.csv")
