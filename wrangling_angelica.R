library(readr)
library(tidyverse)
library(dplyr)
#rating data
# rating <- read_csv("/Users/angelica/Desktop/2019 Part C and D Medicare Star Ratings Data (v04 12 2019) [ZIP, 9MB]/2019 Star Ratings Fall Release (11_2018)/stars.csv")
# write_csv(rating, "rating.csv")
# rating_data <- read_csv("/Users/angelica/Desktop/2019 Part C and D Medicare Star Ratings Data (v04 12 2019) [ZIP, 9MB]/2019 Star Ratings Fall Release (11_2018)/rating_data.csv")
# write_csv(rating_data, "rating_data.csv")
master <- read_csv("data/rating.csv")
rating <- as.data.frame(master[, c(1:5, 28:39)])

#make the first row to be column names
names(rating) <- rating[1,]
rating <- rating[-1,]
#make the next row(rating variables) to be column names
colnames(rating)[-c(1:5)] <- rating[1, -c(1:5)]
#remove the time frame row
rating <- rating[-c(1,2),]
#rename the broken column
colnames(rating)[17] <- "C34: Call Center_Foreign Language Interpreter and TTY Availability"

#convert to long dataset to remove missing values(e.g. plan too small to be measured)
rating_long <- rating %>% 
  pivot_longer(cols = "C23: Getting Needed Care":"C34: Call Center_Foreign Language Interpreter and TTY Availability",
               names_to = "measure",
               names_prefix = "C\\d+: ",
               values_to = "ratings") %>% 
  filter(ratings %in% c(1:5))
rating_wide <- rating_long %>% 
  pivot_wider(names_from = "measure", values_from = "ratings") %>% 
  drop_na()

#data with percentages of members
rating_data <- read_csv("data/rating_data.csv")
rating_data2 <- as.data.frame(rating_data[, c(1:5, 28:39)])

names(rating_data2) <- rating_data2[1,]
rating_data2 <- rating_data2[-1,]
#make the next row(rating variables) to be column names
colnames(rating_data2)[-c(1:5)] <- rating_data2[1, -c(1:5)]
#remove the time frame row
rating_data2 <- rating_data2[-c(1,2),]
#rename the broken column
colnames(rating_data2)[17] <- "C34: Call Center_Foreign Language Interpreter and TTY Availability"
rating_long <- rating_data2 %>% 
  select(-"C31: Health Plan Quality Improvement") %>% 
  mutate_at(c("C30: Members Choosing to Leave the Plan","C32: Plan Makes Timely Decisions about Appeals",
              "C33: Reviewing Appeals Decisions", "C34: Call Center_Foreign Language Interpreter and TTY Availability"), ~str_remove(., "%")) %>% 
  pivot_longer(cols = "C23: Getting Needed Care":"C34: Call Center_Foreign Language Interpreter and TTY Availability",
               names_to = "measure",
               names_prefix = "C\\d+: ",
               values_to = "ratings") %>% 
  filter(!is.na(as.numeric(ratings)))
rating_wide <- rating_long %>% 
  pivot_wider(names_from = "measure", values_from = "ratings") %>% 
  drop_na() %>% 
  mutate(across("Getting Needed Care":"Call Center_Foreign Language Interpreter and TTY Availability", ~{as.numeric(.)/100}))


#tried clustering
set.seed(23)
rating_km3_out <- rating_wide %>% 
  select("Rating of Health Care Quality", "Rating of Health Plan") %>% 
  kmeans(centers = 3, nstart = 20)
rating2 <- rating_wide %>%
  mutate(clusters3_scaled = factor(rating_km3_out$cluster))
ggplot(data = rating2, aes(x = "Rating of Health Care Quality", y = "Rating of Health Plan")) + 
  geom_point(aes(color = clusters3_scaled),
             position=position_jitter(h=0.1, w=0.1)) +
  # ggrepel::geom_text_repel(aes(label = "Contract Name", color = clusters3_scaled), size = 3) +
  coord_fixed() +
  geom_point(data = data.frame(rating_km3_out$centers),
             aes(x = SAT_math25, y = SAT_verbal25),
             pch = "x", size = 8) +
  labs(x = "Math SAT (25th percentile)",
       y = "Verbal SAT (25th percentile)",
       color = "Cluster assignment")


GGally::ggpairs(data = rating2, aes(color = clusters3_scaled),
                columns = c("Getting Needed Care", "Getting Appointments and Care Quickly",
                            "Customer Service", "Rating of Health Care Quality",
                            "Rating of Health Plan", "Care Coordination", "Complaints about the Health Plan",
                            "Members Choosing to Leave the Plan", "Health Plan Quality Improvement",
                            "Plan Makes Timely Decisions about Appeals", "Reviewing Appeals Decisions",
                            "Call Center_Foreign Language Interpreter and TTY Availability"),
                upper = list(continuous = "blank"))

# Clustering using unstandardized variables (for comparison)
ma2_km3_out_unscaled <- ma_sample2 %>% 
  select(admit_rate:cost_avg) %>% 
  kmeans(centers = 3, nstart = 20)

############################
#survey data
nhis19 <- read_csv("data/adult19.csv")

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
      # HISPALLP_A == 07 ~ "Other single and multiple races",
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

#sex, race, education, citizenship, income group

#30032 observations
demographic2 <- demographic %>% 
  filter(across(sex:income_group, ~. != "Other")) 

access_all <- demographic2 %>% 
  group_by(race, sex, edu, citizen, income_group) %>% 
  summarize(access = n())

#9428 observations
access_no <- demographic2 %>% 
  filter(if_any(starts_with(c("unafford", "delayed", "bill", "skip", "less")), ~. =="Yes")) %>% 
  group_by(race, sex, edu, citizen, income_group) %>% 
  summarize(access = n())
access <- left_join(access_all, access_no, by=c("race" = "race", "sex" = "sex", "edu" = "edu",
                                                "citizen" = "citizen", "income_group" = "income_group"),
                    suffix = c(".all", ".no")) %>% 
  mutate(access.no = ifelse(is.na(access.no), 0, access.no)) 
