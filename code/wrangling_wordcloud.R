library(readr)
library(tidyverse)
library(dplyr)
library(tidytext)
library(wordcloud)
library(textdata)
library(ggplot2)
library(ggwordcloud) 
library(gganimate)  
#rating data
# rating <- read_csv("/Users/angelica/Desktop/2019 Part C and D Medicare Star Ratings Data (v04 12 2019) [ZIP, 9MB]/2019 Star Ratings Fall Release (11_2018)/stars.csv")
# write_csv(rating, "rating.csv")
# rating_data <- read_csv("/Users/angelica/Desktop/2019 Part C and D Medicare Star Ratings Data (v04 12 2019) [ZIP, 9MB]/2019 Star Ratings Fall Release (11_2018)/rating_data.csv")
# write_csv(rating_data, "rating_data.csv")
rating16 <- read_csv("data/rating/2016.csv")
rating16_display <- read_csv("data/rating/2016_display.csv")
rating17 <- read_csv("data/rating/2017.csv")
rating18 <- read_csv("data/rating/2018.csv")
rating19 <- read_csv("data/rating/2019.csv")
rating20 <- read_csv("data/rating/2020.csv")
rating21 <- read_csv("data/rating/2021.csv")
rating22 <- read_csv("data/rating/2022.csv")

#ratings data set
#2016
#make the next row(rating variables) to be column names
names(rating16) <- rating16[1,]
rating16 <- rating16[-1,]
colnames(rating16)[-c(1:5)] <- rating16[1, -c(1:5)]
#remove the time frame row
rating16 <- rating16[-c(1,2),]

rating16_2 <- as.data.frame(rating16[, c(1:8, 17:23, 25:27, 30:32, 35:37)])

asNum <- function(x, na.rm = FALSE)(as.numeric(x))
rating16_3 <- rating16_2 %>% 
  rename_with(~str_remove(., "C\\d+: "), contains(":")) %>% 
  mutate(across(c(6:24), ~str_remove(., "%")),
         across(c(6:24), asNum)) %>% 
  drop_na()  %>% 
  mutate(Diabetes = select(., starts_with("Diabetes")) %>% rowSums(na.rm = TRUE),
         "No Diabetes Care" = 1 - Diabetes/300) %>% 
  select(-c(10:12, 25)) %>% 
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
                "TTY Services/Foreign Language Interpretation Unavailable" = "Call Center � Foreign Language Interpreter and TTY Availability",
                "Unfair Appeals Decisions" = "Reviewing Appeals Decisions",
                "Complaints" = "Complaints about the Health Plan") 

rating16_5 <- rating16_4 %>% 
  pivot_longer(cols = 6:22,
               names_to = "measure",
               values_to = "ratings") 

rating16_words <- rating16_5 %>% 
  group_by(measure) %>% 
  summarise(mean = mean(ratings)) %>% 
  mutate(sentences = str_replace_all(measure, " ", "\n"),
         year = "2016") %>% 
  select(-measure)

rating16_words <- rating16_words %>% 
  mutate(rating_type = case_when(
    sentences %in% c("No\nBreast\nCancer\nScreening", "No\nColorectal\nCancer\nScreening", "No\nAccess\nto\nFlu\nVaccine") ~ "Prevention",
    sentences %in% c("No\nDiabetes\nCare", "No\nFall\nRisk\nInterventions", "No\nOsteoporosis\nTreatment", "No\nRheumatoid\nArthritis\nManagement", "No\nTreatment\nfor\nHypertension") ~ "Treatment",
    TRUE ~ "Customer Satisfaction"
  )
)


#2016 display measures data set
rating16_display <- as.data.frame(rating16_display[, c(1:4, 6, 12, 15, 16, 21, 22)])
#make the next row(rating variables) to be column names
names(rating16_display) <- rating16_display[1,]
rating16_display <- rating16_display[-1,]
colnames(rating16_display)[-c(1:4)] <- rating16_display[1, -c(1:4)]
rating16_display <- rating16_display[-1,]

rating16_display2 <- rating16_display %>% 
  rename_with(~str_remove(., " - DMC\\d+"), contains("-")) %>% 
  mutate(across(c(5:10), ~str_remove(., "%")),
         across(c(5:10), asNum)) %>% 
  drop_na()

rating16_display3 <- rating16_display2 %>% 
  dplyr::rename("Call Answer Untimeliness" = "Call Answer Timeliness",
                "Doctors who Communicate Poorly" = "Doctors who Communicate Well",
                "Lack of Access to Primary Care Doctor Visits" = "Access to Primary Care Doctor Visits",
                "No Reminders for Appointments" = "Reminders for Appointments",
                "No Reminders for Immunizations" = "Reminders for Immunizations") %>% 
  mutate(across(c(5, 6, 7, 9, 10), ~{100-.}),
         across(c(5:10), ~{./100}))

rating16_display4 <- rating16_display3 %>% 
  pivot_longer(cols = "Call Answer Untimeliness":"No Reminders for Immunizations",
               names_to = "measure",
               values_to = "ratings") %>% 
  mutate(year = "2016")

rating16_5 <- rating16_4 %>% 
  select(-c("Organization Marketing Name", "Organization Type")) 
rating16_display5 <- rating16_display4 %>% 
  select(-c("Organizatoin Name")) %>% 
  dplyr::rename("CONTRACT_ID" = "Contract Number")
rating16_all <- bind_rows(rating16_5, rating16_display5) 


set.seed(53)
rating16_words %>%
  with(wordcloud(words = sentences, freq = mean, scale = c(1.5,0.3), rot.per = 0.5))


#2017
# rating17 <- as.data.frame(rating17[, c(1:5, 25:37)])
#make the next row(rating variables) to be column names
names(rating17) <- rating17[1,]
rating17 <- rating17[-1,]
colnames(rating17)[-c(1:5)] <- rating17[1, -c(1:5)]
#remove the time frame row
rating17 <- rating17[-c(1,2),]
rating17_2 <- rating17[, c(1:8, 17:23, 25:27, 30:32, 35:37)]
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
                "TTY Services/Foreign Language Interpretation Unavailable" = "Call Center � Foreign Language Interpreter and TTY Availability",
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
    sentences %in% c("No\nBreast\nCancer\nScreening", "No\nColorectal\nCancer\nScreening", "No\nAccess\nto\nFlu\nVaccine") ~ "Prevention",
    sentences %in% c("No\nDiabetes\nCare", "No\nFall\nRisk\nInterventions", "No\nOsteoporosis\nTreatment", "No\nRheumatoid\nArthritis\nManagement", "No\nTreatment\nfor\nHypertension") ~ "Treatment",
    TRUE ~ "Customer Satisfaction"
    )
  )

# Word cloud
set.seed(22)
ggplot(
  rating17_words,
  aes(
    label = sentences, size = mean
  )
) +
  geom_text_wordcloud_area(shape = "star") +
  scale_size_area(max_size = 10) +
  theme_minimal()
rating17_words %>%
  with(wordcloud(words = sentences, freq = mean, scale = c(1.2,0.3), 
                 max.words = 17))
#, rot.per = 0.5



# rating17_words_wide<- rating17_words %>% 
#   pivot_wider(names_from = rating_type, values_from = mean) %>% 
#   mutate(across(everything(), .fns = ~replace_na(.,0)))
# comparison_words <- rating17_words_wide %>%
#   ungroup() %>% 
#   select(-sentences) %>%
#   as.matrix()
# rownames(comparison_words) <- rating17_words_wide$sentences
# comparison.cloud(comparison_words, 
#                  random.order = FALSE,
#                  scale = c(1, 0.35),
#                  colors = colors1,
#                  title.size = 2,
#                  title.colors = colors1)

# library(rvest)
# library(qdap)
# trans_cloud(rating17_words$sentences, rating17_words$rating_type, stem = T)



#2018
#make the next row(rating variables) to be column names
names(rating18) <- rating18[1,]
rating18 <- rating18[-1,]
colnames(rating18)[-c(1:5)] <- rating18[1, -c(1:5)]
#remove the time frame row
rating18 <- rating18[-c(1,2),]
rating18_2 <- rating18[, c(1:8, 17:24, 27:29, 32:34, 37:39)]

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
                "TTY Services/Foreign Language Interpretation Unavailable" = "Call Center � Foreign Language Interpreter and TTY Availability",
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
                     "No\nTreatment\nfor\nHypertension", "No\nTreatment\nfor\nUrinary\nIncontinence") ~ "Treatment",
    TRUE ~ "Customer Satisfaction"
    )
  )



#2019
#make the next row(rating variables) to be column names
names(rating19) <- rating19[1,]
rating19 <- rating19[-1,]
colnames(rating19)[-c(1:5)] <- rating19[1, -c(1:5)]
#remove the time frame row
rating19 <- rating19[-c(1,2),]

rating19_2 <- rating19[, c(1:8, 17:24, 27:30, 33:35, 37:39)]

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
                "TTY Services/Foreign Language Interpretation Unavailable" = "Call Center � Foreign Language Interpreter and TTY Availability",
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


#2020
#make the next row(rating variables) to be column names
names(rating20) <- rating20[1,]
rating20 <- rating20[-1,]
colnames(rating20)[-c(1:5)] <- rating20[1, -c(1:5)]
#remove the time frame row
rating20 <- rating20[-c(1,2),]

rating20_2 <- rating20[, c(1:8, 17:23, 26:29, 32:34, 36:38)]

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


#2021
#make the next row(rating variables) to be column names
names(rating21) <- rating21[1,]
rating21 <- rating21[-1,]
colnames(rating21)[-c(1:5)] <- rating21[1, -c(1:5)]
#remove the time frame row
rating21 <- rating21[-c(1,2),]

rating21_2 <- rating21[, c(1:8, 17:23, 25:28, 31:33, 35:37)]

asNum <- function(x, na.rm = FALSE)(as.numeric(x))
rating21_3 <- rating21_2 %>% 
  rename_with(~str_remove(., "C\\d+: "), contains(":")) %>% 
  mutate(across(c(6:25), ~str_remove(., "%")),
         across(c(6:25), asNum)) %>% 
  drop_na()  %>% 
  mutate(Diabetes = select(., starts_with("Diabetes")) %>% rowSums(na.rm = TRUE),
         "No Diabetes Care" = 1 - Diabetes/300) %>% 
  select(-c(10:12, 26)) %>% 
  mutate(across(c(6:17, 20:22), ~{100-.}),
         across(c(6:17, 19:22), ~{./100}))

rating21_4 <- rating21_3 %>% 
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

rating21_5 <- rating21_4 %>% 
  pivot_longer(cols = 6:23,
               names_to = "measure",
               values_to = "ratings") 

rating21_words <- rating21_5 %>% 
  group_by(measure) %>% 
  summarise(mean = mean(ratings)) %>% 
  mutate(sentences = str_replace_all(measure, " ", "\n"),
         year = "2021") %>% 
  select(-measure)

rating21_words <- rating21_words %>% 
  mutate(rating_type = case_when(
    sentences %in% c("No\nBreast\nCancer\nScreening", "No\nColorectal\nCancer\nScreening", 
                     "No\nAccess\nto\nFlu\nVaccine") ~ "Prevention",
    sentences %in% c("No\nDiabetes\nCare", "No\nFall\nRisk\nInterventions", 
                     "No\nOsteoporosis\nTreatment", "No\nRheumatoid\nArthritis\nManagement", 
                     "No\nTreatment\nfor\nUrinary\nIncontinence", "No\nTreatment\nfor\nCardiovascular\nDisease") ~ "Treatment",
    TRUE ~ "Customer Satisfaction"
    )
  )


#2022
#make the next row(rating variables) to be column names
names(rating22) <- rating22[1,]
rating22 <- rating22[-1,]
colnames(rating22)[-c(1:5)] <- rating22[1, -c(1:5)]
#remove the time frame row
rating22 <- rating22[-c(1,2),]

rating22_2 <- rating22[, c(1:8, 14:19, 21:24, 27:29, 31:33)]

asNum <- function(x, na.rm = FALSE)(as.numeric(x))
rating22_3 <- rating22_2 %>% 
  rename_with(~str_remove(., "C\\d+: "), contains(":")) %>% 
  mutate(across(c(6:24), ~str_remove(., "%")),
         across(c(6:24), asNum)) %>% 
  drop_na()  %>% 
  mutate(Diabetes = select(., starts_with("Diabetes")) %>% rowSums(na.rm = TRUE),
         "No Diabetes Care" = 1 - Diabetes/300) %>% 
  select(-c(9:11, 25)) %>% 
  mutate(across(c(6:16, 19:21), ~{100-.}),
         across(c(6:16, 18:21), ~{./100}))

rating22_4 <- rating22_3 %>% 
  dplyr::rename("No Breast Cancer Screening" = "Breast Cancer Screening",
                "No Colorectal Cancer Screening" = "Colorectal Cancer Screening",
                "No Access to Flu Vaccine" = "Annual Flu Vaccine",
                "No Rheumatoid Arthritis Management" = "Rheumatoid Arthritis Management",
                "No Fall Risk Interventions" = "Reducing the Risk of Falling",
                "No Treatment for Urinary Incontinence" = "Improving Bladder Control",
                "No Treatment for Cardiovascular Disease" = "Statin Therapy for Patients with Cardiovascular Disease",
                "Not Getting Needed Care" = "Getting Needed Care",
                "Less Timely Care/Appointments" = "Getting Appointments and Care Quickly",
                "Poor Customer Service" = "Customer Service",
                "Poor Care Coordination" = "Care Coordination",
                "Less Timely Decisions about Appeals" = "Plan Makes Timely Decisions about Appeals",
                "TTY Services/Foreign Language Interpretation Unavailable" = "Call Center – Foreign Language Interpreter and TTY Availability",
                "Unfair Appeals Decisions" = "Reviewing Appeals Decisions",
                "Complaints" = "Complaints about the Health Plan")

rating22_5 <- rating22_4 %>% 
  pivot_longer(cols = 6:22,
               names_to = "measure",
               values_to = "ratings") 

rating22_words <- rating22_5 %>% 
  group_by(measure) %>% 
  summarise(mean = mean(ratings)) %>% 
  mutate(sentences = str_replace_all(measure, " ", "\n"),
         year = "2022") %>% 
  select(-measure)

rating22_words <- rating22_words %>% 
  mutate(rating_type = case_when(
    sentences %in% c("No\nBreast\nCancer\nScreening", "No\nColorectal\nCancer\nScreening", 
                     "No\nAccess\nto\nFlu\nVaccine") ~ "Prevention",
    sentences %in% c("No\nDiabetes\nCare", "No\nFall\nRisk\nInterventions", "No\nRheumatoid\nArthritis\nManagement", 
                     "No\nTreatment\nfor\nUrinary\nIncontinence", "No\nTreatment\nfor\nCardiovascular\nDisease") ~ "Treatment",
    TRUE ~ "Customer Satisfaction"
    )
  )



#combine all years
ratings <- bind_rows(rating16_words, rating17_words, rating18_words, rating19_words, 
                     rating20_words, rating21_words, rating22_words)
# save the combined data set
write_csv(ratings, "blog-wordcloud/ratings.csv")

#wordcloud
set.seed(53)
rating_words %>%
  with(wordcloud(words = sentences, freq = mean, scale = c(1.5,0.3), max.words = 11))

gg <- rating_words %>%
  ggplot(aes(label = sentences, size=mean)) +
  geom_text_wordcloud(area_corr = TRUE) +
  scale_size_area(max_size = 10) +
  theme_minimal() 

gg2 <- gg + transition_time(as.integer(year)) +
  labs(title = 'Year: {frame_time}')

animate(gg2, nframes = length(unique(rating_words$year)), fps = 3, renderer=gifski_renderer(),
        width = 1000, height = 1000, res = 150)
anim_save(filename="testing.gif")

