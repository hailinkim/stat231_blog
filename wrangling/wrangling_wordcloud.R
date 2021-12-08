library(readr)
library(tidyverse)
library(dplyr)
library(tidytext)
library(wordcloud)
library(textdata)
library(ggplot2)
library(janitor)

#rating data
rating16 <- read_csv("data/rating/2016.csv")
rating17 <- read_csv("data/rating/2017.csv")
rating18 <- read_csv("data/rating/2018.csv")
rating19 <- read_csv("data/rating/2019.csv")
rating20 <- read_csv("data/rating/2020.csv")

#ratings data set
#2016
#make the first row to be column names
names(rating16) <- rating16[1,]
#remove the original first row because it is redundant
rating16 <- rating16[-1,]
#for the sixth column and onward, make the current first row to be the column names
colnames(rating16)[-c(1:5)] <- rating16[1, -c(1:5)]
#remove the first row because it is redundant
  #remove the second row as well because it contains unnecessary information
rating16 <- rating16[-c(1,2),]

#remove unnecessary variables
rating16_2 <- rating16 %>% 
  select(c(1:8, 17:23, 25:27, 30:32, 35:37))

asNum <- function(x, na.rm = FALSE)(as.numeric(x))

rating16_3 <- rating16_2 %>% 
  #rename the variables
  rename_with(~str_remove(., "C\\d+: "), contains(":")) %>% 
  #remove percentage sign and make variables numeric
  mutate(across(c(6:24), ~str_remove(., "%")),
         across(c(6:24), asNum)) %>% 
  drop_na()  %>% 
  #compute row-wise sum of proportions for 3 columns related to diabetes
  mutate(Diabetes = select(., starts_with("Diabetes")) %>% rowSums(na.rm = TRUE), 
         "No Diabetes Care" = 1 - Diabetes/300) %>% 
  #remove unnecessary/redundant columns related to diabetes
  select(-c(10:12, 25)) %>% 
  #subtract values from 100 to focus on those who did NOT utilize certain services
    #convert them from percentages to proportions(decimals)
  mutate(across(c(6:16, 19:21), ~{100-.}),
         across(c(6:16, 18:21), ~{./100}))

rating16_4 <- rating16_3 %>% 
  dplyr::rename("No Breast Cancer Screening" = "Breast Cancer Screening",
                "No Colorectal Cancer Screening" = "Colorectal Cancer Screening",
                "No Access to Flu Vaccine" = "Annual Flu Vaccine",
                "No Osteoporosis Treatment" = "Osteoporosis Management in Women who had a Fracture",
                "No Treatment for Hypertension" = "Controlling Blood Pressure",
                "No Rheumatoid Arthritis Management" = "Rheumatoid Arthritis Management",
                "No Fall Risk Interventions" = "Reducing the Risk of Falling",
                "Not Getting Needed Care" = "Getting Needed Care",
                "Less Timely Care/Appointments" = "Getting Appointments and Care Quickly",
                "Poor Customer Service" = "Customer Service",
                "Poor Care Coordination" = "Care Coordination",
                "Less Timely Decisions about Appeals" = "Plan Makes Timely Decisions about Appeals",
                "TTY Services/Foreign Language Interpretation Unavailable" = 
                  "Call Center � Foreign Language Interpreter and TTY Availability",
                "Unfair Appeals Decisions" = "Reviewing Appeals Decisions",
                "Complaints" = "Complaints about the Health Plan") 

rating16_5 <- rating16_4 %>% 
  pivot_longer(cols = 6:22,
               names_to = "measure",
               values_to = "ratings") 

rating16_words <- rating16_5 %>% 
  group_by(measure) %>% 
  summarise(mean = mean(ratings)) %>% 
  #added line breaks to the words for wordcloud visualization
  mutate(sentences = str_replace_all(measure, " ", "\n"),
         year = "2016") %>% 
  select(-measure)

rating16_words <- rating16_words %>% 
  mutate(rating_type = case_when(
    sentences %in% c("No\nBreast\nCancer\nScreening", "No\nColorectal\nCancer\nScreening", 
                     "No\nAccess\nto\nFlu\nVaccine") ~ "Prevention",
    sentences %in% c("No\nDiabetes\nCare", "No\nFall\nRisk\nInterventions", 
                     "No\nOsteoporosis\nTreatment", "No\nRheumatoid\nArthritis\nManagement", 
                     "No\nTreatment\nfor\nHypertension") ~ "Treatment",
    TRUE ~ "Customer Satisfaction"
  )
)


#2017 (wrangling codes are exactly same as those for the 2016 data set)
names(rating17) <- rating17[1,]

rating17 <- rating17[-1,]

colnames(rating17)[-c(1:5)] <- rating17[1, -c(1:5)]

rating17 <- rating17[-c(1,2),]

rating17_2 <- rating17 %>% 
  select(c(1:8, 17:23, 25:27, 30:32, 35:37))

asNum <- function(x, na.rm = FALSE)(as.numeric(x))

rating17_3 <- rating17_2 %>% 
  rename_with(~str_remove(., "C\\d+: "), contains(":")) %>% 
  mutate(across(c(6:24), ~str_remove(., "%")),
         across(c(6:24), asNum)) %>% 
  drop_na()  %>% 
  mutate(Diabetes = select(., starts_with("Diabetes")) %>% rowSums(na.rm = TRUE),
         "No Diabetes Care" = 1 - Diabetes/300) %>% 
  select(-c(10:12, 25)) %>% 
  mutate(across(c(6:16, 19:21), ~{100-.}),
         across(c(6:16, 18:21), ~{./100}))

rating17_4 <- rating17_3 %>% 
  dplyr::rename("No Breast Cancer Screening" = "Breast Cancer Screening",
                "No Colorectal Cancer Screening" = "Colorectal Cancer Screening",
                "No Access to Flu Vaccine" = "Annual Flu Vaccine",
                "No Osteoporosis Treatment" = "Osteoporosis Management in Women who had a Fracture",
                "No Treatment for Hypertension" = "Controlling Blood Pressure",
                "No Rheumatoid Arthritis Management" = "Rheumatoid Arthritis Management",
                "No Fall Risk Interventions" = "Reducing the Risk of Falling",
                "Not Getting Needed Care" = "Getting Needed Care",
                "Less Timely Care/Appointments" = "Getting Appointments and Care Quickly",
                "Poor Customer Service" = "Customer Service",
                "Poor Care Coordination" = "Care Coordination",
                "Less Timely Decisions about Appeals" = "Plan Makes Timely Decisions about Appeals",
                "TTY Services/Foreign Language Interpretation Unavailable" = 
                  "Call Center � Foreign Language Interpreter and TTY Availability",
                "Unfair Appeals Decisions" = "Reviewing Appeals Decisions",
                "Complaints" = "Complaints about the Health Plan")

rating17_5 <- rating17_4 %>% 
  pivot_longer(cols = 6:22,
               names_to = "measure",
               values_to = "ratings") 

rating17_words <- rating17_5 %>% 
  group_by(measure) %>% 
  summarise(mean = mean(ratings)) %>% 
  mutate(sentences = str_replace_all(measure, " ", "\n"),
         year = "2017") %>% 
  select(-measure)

rating17_words <- rating17_words %>% 
  mutate(rating_type = case_when(
    sentences %in% c("No\nBreast\nCancer\nScreening", "No\nColorectal\nCancer\nScreening", 
                     "No\nAccess\nto\nFlu\nVaccine") ~ "Prevention",
    sentences %in% c("No\nDiabetes\nCare", "No\nFall\nRisk\nInterventions", 
                     "No\nOsteoporosis\nTreatment", "No\nRheumatoid\nArthritis\nManagement", 
                     "No\nTreatment\nfor\nHypertension") ~ "Treatment",
    TRUE ~ "Customer Satisfaction"
    )
  )


#2018 (wrangling codes are exactly same as those for the 2016 data set)
names(rating18) <- rating18[1,]

rating18 <- rating18[-1,]

colnames(rating18)[-c(1:5)] <- rating18[1, -c(1:5)]

rating18 <- rating18[-c(1,2),]

rating18_2 <- rating18 %>% 
  select(c(1:8, 17:24, 27:29, 32:34, 37:39))

asNum <- function(x, na.rm = FALSE)(as.numeric(x))

rating18_3 <- rating18_2 %>% 
  rename_with(~str_remove(., "C\\d+: "), contains(":")) %>% 
  mutate(across(c(6:25), ~str_remove(., "%")),
         across(c(6:25), asNum)) %>% 
  drop_na()  %>% 
  mutate(Diabetes = select(., starts_with("Diabetes")) %>% rowSums(na.rm = TRUE),
         "No Diabetes Care" = 1 - Diabetes/300) %>% 
  select(-c(10:12, 26)) %>% 
  mutate(across(c(6:17, 20:22), ~{100-.}),
         across(c(6:17, 19:22), ~{./100}))

rating18_4 <- rating18_3 %>% 
  dplyr::rename("No Breast Cancer Screening" = "Breast Cancer Screening",
                "No Colorectal Cancer Screening" = "Colorectal Cancer Screening",
                "No Access to Flu Vaccine" = "Annual Flu Vaccine",
                "No Osteoporosis Treatment" = "Osteoporosis Management in Women who had a Fracture",
                "No Treatment for Hypertension" = "Controlling Blood Pressure",
                "No Rheumatoid Arthritis Management" = "Rheumatoid Arthritis Management",
                "No Fall Risk Interventions" = "Reducing the Risk of Falling",
                "No Treatment for Urinary Incontinence" = "Improving Bladder Control",
                "Not Getting Needed Care" = "Getting Needed Care",
                "Less Timely Care/Appointments" = "Getting Appointments and Care Quickly",
                "Poor Customer Service" = "Customer Service",
                "Poor Care Coordination" = "Care Coordination",
                "Less Timely Decisions about Appeals" = "Plan Makes Timely Decisions about Appeals",
                "TTY Services/Foreign Language Interpretation Unavailable" = 
                  "Call Center � Foreign Language Interpreter and TTY Availability",
                "Unfair Appeals Decisions" = "Reviewing Appeals Decisions",
                "Complaints" = "Complaints about the Health Plan")

rating18_5 <- rating18_4 %>% 
  pivot_longer(cols = 6:23,
               names_to = "measure",
               values_to = "ratings") 

rating18_words <- rating18_5 %>% 
  group_by(measure) %>% 
  summarise(mean = mean(ratings)) %>% 
  mutate(sentences = str_replace_all(measure, " ", "\n"),
         year = "2018") %>% 
  select(-measure)

rating18_words <- rating18_words %>% 
  mutate(rating_type = case_when(
    sentences %in% c("No\nBreast\nCancer\nScreening", "No\nColorectal\nCancer\nScreening", 
                     "No\nAccess\nto\nFlu\nVaccine") ~ "Prevention",
    sentences %in% c("No\nDiabetes\nCare", "No\nFall\nRisk\nInterventions", 
                     "No\nOsteoporosis\nTreatment", "No\nRheumatoid\nArthritis\nManagement", 
                     "No\nTreatment\nfor\nUrinary\nIncontinence",
                     "No\nTreatment\nfor\nHypertension") ~ "Treatment",
    TRUE ~ "Customer Satisfaction"
    )
  )


#2019 (wrangling codes are exactly same as those for the 2016 data set)
names(rating19) <- rating19[1,]

rating19 <- rating19[-1,]

colnames(rating19)[-c(1:5)] <- rating19[1, -c(1:5)]

rating19 <- rating19[-c(1,2),]

rating19_2 <- rating19 %>% 
  select(c(1:8, 17:24, 27:30, 33:35, 37:39))

asNum <- function(x, na.rm = FALSE)(as.numeric(x))

rating19_3 <- rating19_2 %>% 
  rename_with(~str_remove(., "C\\d+: "), contains(":")) %>% 
  mutate(across(c(6:26), ~str_remove(., "%")),
         across(c(6:26), asNum)) %>% 
  drop_na()  %>% 
  mutate(Diabetes = select(., starts_with("Diabetes")) %>% rowSums(na.rm = TRUE),
         "No Diabetes Care" = 1 - Diabetes/300) %>% 
  select(-c(10:12, 27)) %>% 
  mutate(across(c(6:18, 21:23), ~{100-.}),
         across(c(6:18, 20:23), ~{./100}))

rating19_4 <- rating19_3 %>% 
  dplyr::rename("No Breast Cancer Screening" = "Breast Cancer Screening",
                "No Colorectal Cancer Screening" = "Colorectal Cancer Screening",
                "No Access to Flu Vaccine" = "Annual Flu Vaccine",
                "No Osteoporosis Treatment" = "Osteoporosis Management in Women who had a Fracture",
                "No Treatment for Hypertension" = "Controlling Blood Pressure",
                "No Rheumatoid Arthritis Management" = "Rheumatoid Arthritis Management",
                "No Fall Risk Interventions" = "Reducing the Risk of Falling",
                "No Treatment for Urinary Incontinence" = "Improving Bladder Control",
                "No Treatment for Cardiovascular Disease" = "Statin Therapy for Patients with Cardiovascular Disease",
                "Not Getting Needed Care" = "Getting Needed Care",
                "Less Timely Care/Appointments" = "Getting Appointments and Care Quickly",
                "Poor Customer Service" = "Customer Service",
                "Poor Care Coordination" = "Care Coordination",
                "Less Timely Decisions about Appeals" = "Plan Makes Timely Decisions about Appeals",
                "TTY Services/Foreign Language Interpretation Unavailable" = 
                  "Call Center � Foreign Language Interpreter and TTY Availability",
                "Unfair Appeals Decisions" = "Reviewing Appeals Decisions",
                "Complaints" = "Complaints about the Health Plan")

rating19_5 <- rating19_4 %>% 
  pivot_longer(cols = 6:24,
               names_to = "measure",
               values_to = "ratings") 

rating19_words <- rating19_5 %>% 
  group_by(measure) %>% 
  summarise(mean = mean(ratings)) %>% 
  mutate(sentences = str_replace_all(measure, " ", "\n"),
         year = "2019") %>% 
  select(-measure)

rating19_words <- rating19_words %>% 
  mutate(rating_type = case_when(
    sentences %in% c("No\nBreast\nCancer\nScreening", "No\nColorectal\nCancer\nScreening", 
                     "No\nAccess\nto\nFlu\nVaccine") ~ "Prevention",
    sentences %in% c("No\nDiabetes\nCare", "No\nFall\nRisk\nInterventions", 
                     "No\nOsteoporosis\nTreatment", "No\nRheumatoid\nArthritis\nManagement", 
                     "No\nTreatment\nfor\nHypertension", "No\nTreatment\nfor\nUrinary\nIncontinence",
                     "No\nTreatment\nfor\nCardiovascular\nDisease") ~ "Treatment",
    TRUE ~ "Customer Satisfaction"
  )
)


#2020 (wrangling codes are exactly same as those for the 2016 data set)
names(rating20) <- rating20[1,]

rating20 <- rating20[-1,]

colnames(rating20)[-c(1:5)] <- rating20[1, -c(1:5)]

rating20 <- rating20[-c(1,2),]

rating20_2 <- rating20 %>% 
  select(c(1:8, 17:23, 26:29, 32:34, 36:38))

asNum <- function(x, na.rm = FALSE)(as.numeric(x))

rating20_3 <- rating20_2 %>% 
  rename_with(~str_remove(., "C\\d+: "), contains(":")) %>% 
  mutate(across(c(6:25), ~str_remove(., "%")),
         across(c(6:25), asNum)) %>% 
  drop_na()  %>% 
  mutate(Diabetes = select(., starts_with("Diabetes")) %>% rowSums(na.rm = TRUE),
         "No Diabetes Care" = 1 - Diabetes/300) %>% 
  select(-c(10:12, 26)) %>% 
  mutate(across(c(6:17, 20:22), ~{100-.}),
         across(c(6:17, 19:22), ~{./100}))

rating20_4 <- rating20_3 %>% 
  dplyr::rename("No Breast Cancer Screening" = "Breast Cancer Screening",
                "No Colorectal Cancer Screening" = "Colorectal Cancer Screening",
                "No Access to Flu Vaccine" = "Annual Flu Vaccine",
                "No Osteoporosis Treatment" = "Osteoporosis Management in Women who had a Fracture",
                "No Rheumatoid Arthritis Management" = "Rheumatoid Arthritis Management",
                "No Fall Risk Interventions" = "Reducing the Risk of Falling",
                "No Treatment for Urinary Incontinence" = "Improving Bladder Control",
                "No Treatment for Cardiovascular Disease" = "Statin Therapy for Patients with Cardiovascular Disease",
                "Not Getting Needed Care" = "Getting Needed Care",
                "Less Timely Care/Appointments" = "Getting Appointments and Care Quickly",
                "Poor Customer Service" = "Customer Service",
                "Poor Care Coordination" = "Care Coordination",
                "Less Timely Decisions about Appeals" = "Plan Makes Timely Decisions about Appeals",
                "TTY Services/Foreign Language Interpretation Unavailable" = "Call Center � Foreign Language Interpreter and TTY Availability",
                "Unfair Appeals Decisions" = "Reviewing Appeals Decisions",
                "Complaints" = "Complaints about the Health Plan")

rating20_5 <- rating20_4 %>% 
  pivot_longer(cols = 6:23,
               names_to = "measure",
               values_to = "ratings") 

rating20_words <- rating20_5 %>% 
  group_by(measure) %>% 
  summarise(mean = mean(ratings)) %>% 
  mutate(sentences = str_replace_all(measure, " ", "\n"),
         year = "2020") %>% 
  select(-measure)

rating20_words <- rating20_words %>% 
  mutate(rating_type = case_when(
    sentences %in% c("No\nBreast\nCancer\nScreening", "No\nColorectal\nCancer\nScreening", 
                     "No\nAccess\nto\nFlu\nVaccine") ~ "Prevention",
    sentences %in% c("No\nDiabetes\nCare", "No\nFall\nRisk\nInterventions", 
                     "No\nOsteoporosis\nTreatment", "No\nRheumatoid\nArthritis\nManagement", 
                     "No\nTreatment\nfor\nUrinary\nIncontinence", "No\nTreatment\nfor\nCardiovascular\nDisease") ~ "Treatment",
    TRUE ~ "Customer Satisfaction"
    )
  )

#combine all years
ratings <- bind_rows(rating16_words, rating17_words, rating18_words, 
                     rating19_words, rating20_words)

# save the combined data set
write_csv(ratings, "blog-wordcloud/ratings.csv")