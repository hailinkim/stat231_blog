library(ggnetwork)
library(igraph)
library(readr)
library(tidyverse)
library(dplyr)
library(cluster)
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
  filter(across(sex:unafford_med, ~. != "Other")) %>% 
  mutate(barrier = ifelse(
    if_any(starts_with(c("unafford", "delayed", "bill", "skip", "less")), ~. == "Yes"), "Yes", "No"),
    income_sqrt = sqrt(income)) %>% 
  select(-c(SEX_A:RXSK12M_A, coverage:years_in_us, delayed_mental:unafford_med, income, income_group))

demographic3 <- demographic2 %>% 
  mutate(across(where(is.character), as.factor))


#checking the distribution of income
ggplot(demographic2, aes(x = sqrt(income))) +
  geom_histogram()

gower_df <- daisy(demographic3, metric = "gower")


# tmp <- demographic2 %>% 
#   group_by(coverage, barrier) %>% 
#   summarise(count = n())

access_all <- demographic2 %>% 
  group_by(race, sex, edu, citizen, income_group) %>% 
  summarize(access = n())

tmp1 <- demographic2 %>% 
  group_by(race, sex) %>% 
  summarise(access = n()) %>% 
  dplyr::rename(source = race, target = sex)
tmp2 <- demographic2 %>% 
  group_by(race, edu) %>% 
  summarise(access = n()) %>% 
  dplyr::rename(source = race, target = edu)
tmp3 <- demographic2 %>% 
  group_by(race, citizen) %>% 
  summarise(access = n()) %>% 
  dplyr::rename(source = race, target = citizen)
tmp4 <- demographic2 %>% 
  group_by(race, income_group) %>% 
  summarise(access = n()) %>% 
  dplyr::rename(source = race, target = income_group)
race <- bind_rows(tmp1, tmp2, tmp3, tmp4)
race_asian <- race %>% 
  filter(source == "Asian")
tmp5 <- demographic2 %>% 
  group_by(sex, edu) %>% 
  summarise(access = n()) %>% 
  dplyr::rename(source = sex, target = edu)
tmp6 <- demographic2 %>% 
  group_by(sex, citizen) %>% 
  summarise(access = n()) %>% 
  dplyr::rename(source = sex, target = citizen)
tmp7 <- demographic2 %>% 
  group_by(sex, income_group) %>% 
  summarise(access = n()) %>% 
  dplyr::rename(source = sex, target = income_group)
tmp8 <- demographic2 %>% 
  group_by(edu, citizen) %>% 
  summarise(access = n()) %>% 
  dplyr::rename(source = edu, target = citizen)
tmp9 <- demographic2 %>% 
  group_by(edu, income_group) %>% 
  summarise(access = n()) %>% 
  dplyr::rename(source = edu, target = income_group)
tmp10 <- demographic2 %>% 
  group_by(citizen, income_group) %>% 
  summarise(access = n()) %>% 
  dplyr::rename(source = citizen, target = income_group)
other <- bind_rows(tmp5, tmp6, tmp7, tmp8, tmp9, tmp10)

tmp <- bind_rows(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10)


#9428 observations
access_no <- demographic2 %>% 
  filter(if_any(starts_with(c("unafford", "delayed", "bill", "skip", "less")), ~. =="Yes")) %>% 
  group_by(race, sex, edu, citizen, income_group) %>% 
  summarize(access = n())
access <- left_join(access_all, access_no, by=c("race" = "race", "sex" = "sex", "edu" = "edu",
                                                "citizen" = "citizen", "income_group" = "income_group"),
                    suffix = c(".all", ".no")) %>% 
  mutate(access.no = ifelse(is.na(access.no), 0, access.no)) 

tmp_asian <- bind_rows(race_asian, other)
asian_igraph <- graph_from_data_frame(tmp, directed = TRUE)
summary(asian_igraph)
asian_network <- ggnetwork(asian_igraph)
g <- ggplot(data = asian_network, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(type = "closed", length = unit(8, "pt")), color = "lightgray",
             aes(size = access)) +
  geom_nodes() +
  geom_nodelabel(aes(label = name)) +
  theme_blank()


nhis_igraph <- graph_from_data_frame(tmp, directed = FALSE)
summary(nhis_igraph)
nhis_network <- ggnetwork(nhis_igraph)

g <- ggplot(data = nhis_network, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(type = "closed", length = unit(8, "pt")),
             color = "lightgray") +
  geom_nodes() +
  geom_nodelabel(aes(label = name)) +
  theme_blank()
