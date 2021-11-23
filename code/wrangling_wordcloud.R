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


#display measures data set
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

# rating16_words <- rating16_all %>% 
#   unnest_tokens(output = sentences, input = measure, token = "sentences") %>%
#   group_by(sentences, year) %>% 
#   summarise(mean = mean(ratings)) %>% 
#   mutate(year=as.integer(year))
write_csv(rating_words, "data/rating_words.csv")

rating_words <- read_csv("data/rating_words.csv")

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
rating17_2 <- rating17[, -c(38:52)]
rating17_3 <- rating17_2 %>% 
  rename_with(~str_remove(., "C\\d+: "), contains(":"))
rating17_4 <- rating17_3[, -c(9:16, 24, 28, 29, 33, 34)]

asNum <- function(x, na.rm = FALSE)(as.numeric(x))
rating17_5 <- rating17_4 %>% 
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
                "Unfair Appeals Decisions" = "Reviewing Appeals Decisions") %>% 
  mutate(across(c(6:24), ~str_remove(., "%")),
         across(c(6:24), asNum)) %>% 
  drop_na() %>% 
  mutate(Diabetes = select(., starts_with("Diabetes")) %>% rowSums(na.rm = TRUE),
         "No Diabetes Care" = 1 - Diabetes/300) %>% 
  select(-c(10:12, 25)) %>% 
  mutate(across(c(6:16, 19:21), ~{100-.}),
         across(c(6:16, 18:21), ~{./100}))

rating17_6 <- rating17_5 %>% 
  pivot_longer(cols = 6:22,
               names_to = "measure",
               values_to = "ratings") 

rating17_words <- rating17_6 %>%
  group_by(measure) %>% 
  summarise(mean = mean(ratings)) %>% 
  mutate(sentences = str_replace_all(measure, " ", "\n"))

write_csv(rating17_words, "data/rating17_words.csv")

  mutate(rating_type = case_when(
    sentences %in% c("breast_cancer_screening", "colorectal_cancer_screening", "annual_flu_vaccine") ~ "prevention",
    sentences %in% c("special_needs_plan_snp_care_management", "care_for_older_adults_medication_review",
                   "care_for_older_adults_functional_status_assessment", "care_for_older_adults_pain_assessment",
                   "osteoporosis_management_in_women_who_had_a_fracture", "diabetes_care_eye_exam",
                   "diabetes_care_kidney_disease_monitoring", "diabetes_care_blood_sugar_controlled",
                   "controlling_blood_pressure", "rheumatoid_arthritis_management", "reducing_the_risk_of_falling") ~ "treatment",
    TRUE ~ "Customer Satisfaction"
    ),
    sentences2 = str_replace_all(sentences, "_", "\n")
  )
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



rating17_words_wide<- rating17_words %>% 
  pivot_wider(names_from = rating_type, values_from = mean) %>% 
  mutate(across(everything(), .fns = ~replace_na(.,0)))
comparison_words <- rating17_words_wide %>%
  ungroup() %>% 
  select(-sentences) %>%
  as.matrix()
rownames(comparison_words) <- rating17_words_wide$sentences
comparison.cloud(comparison_words, 
                 random.order = FALSE,
                 scale = c(1, 0.35),
                 colors = colors1,
                 title.size = 2,
                 title.colors = colors1)

library(rvest)
library(qdap)
trans_cloud(rating17_words$sentences, rating17_words$rating_type, stem = T)



#2018
rating18 <- as.data.frame(rating18[, c(1:5, 27:39)])
#make the next row(rating variables) to be column names
names(rating18) <- rating18[1,]
rating18 <- rating18[-1,]
colnames(rating18)[-c(1:5)] <- rating18[1, -c(1:5)]
#remove the time frame row
rating18 <- rating18[-c(1,2),]
#rename the broken column
colnames(rating18)[18] <- "C34: Call Center_Foreign Language Interpreter and TTY Availability"

rating18_2 <- rating18 %>% 
  select(-"C31: Health Plan Quality Improvement") %>% 
  rename_with(~str_remove(., "C\\d+: "), contains(":")) %>% 
  mutate(across(c("Getting Needed Care": "Call Center_Foreign Language Interpreter and TTY Availability"), ~str_remove(., "%")),
         across(c(6:17), asNum)) %>% 
  drop_na()

rating18_3 <- rating18_2 %>% 
  select(-c("Rating of Health Care Quality", "Rating of Health Plan")) %>% 
  dplyr::rename("Not Getting Needed Care" = "Getting Needed Care",
                "Less Timely Care and Appointments" = "Getting Appointments and Care Quickly",
                "Difficult to Get Information and Help from the Plan When Needed" = "Customer Service",
                "Plan Coordinates Members’ Care Poorly" = "Care Coordination",
                "Problems with Plan's Performance" = "Beneficiary Access and Performance Problems",
                "Less Timely Decisions about Appeals" = "Plan Makes Timely Decisions about Appeals",
                "TTY Services and Foreign Language Interpretation Unavailable When Needed" = "Call Center_Foreign Language Interpreter and TTY Availability",
                "Unfair Appeals Decisions" = "Reviewing Appeals Decisions") %>% 
  mutate(across(c(6, 7, 8, 9, 12, 13, 14, 15), ~{100-.}),
         across(c(6:9, 11:15), ~{./100}))

rating18_4 <- rating18_3 %>% 
  pivot_longer(cols = "Not Getting Needed Care":"TTY Services and Foreign Language Interpretation Unavailable When Needed",
               names_to = "measure",
               values_to = "ratings") %>% 
  mutate(year = "2018")


#2019
rating19 <- as.data.frame(rating19[, c(1:5, 28:39)])
#make the next row(rating variables) to be column names
names(rating19) <- rating19[1,]
rating19 <- rating19[-1,]
colnames(rating19)[-c(1:5)] <- rating19[1, -c(1:5)]
#remove the time frame row
rating19 <- rating19[-c(1,2),]
#rename the broken column
colnames(rating19)[17] <- "C34: Call Center_Foreign Language Interpreter and TTY Availability"

asNum <- function(x, na.rm = FALSE)(as.numeric(x))
rating19_2 <- rating19 %>% 
  select(-"C31: Health Plan Quality Improvement") %>% 
  rename_with(~str_remove(., "C\\d+: "), contains(":")) %>% 
  mutate(across(c("Members Choosing to Leave the Plan","Plan Makes Timely Decisions about Appeals",
              "Reviewing Appeals Decisions", "Call Center_Foreign Language Interpreter and TTY Availability"), ~str_remove(., "%")),
         across(c(6:16), asNum)) %>% 
  filter(across(c(6:16), ~!is.na(.)))

rating19_3 <- rating19_2 %>% 
  select(-c("Rating of Health Care Quality", "Rating of Health Plan")) %>% 
  dplyr::rename("Not Getting Needed Care" = "Getting Needed Care",
                "Less Timely Care and Appointments" = "Getting Appointments and Care Quickly",
                "Difficult to Get Information and Help from the Plan When Needed" = "Customer Service",
                "Plan Coordinates Members’ Care Poorly" = "Care Coordination",
                "Less Timely Decisions about Appeals" = "Plan Makes Timely Decisions about Appeals",
                "TTY Services and Foreign Language Interpretation Unavailable When Needed" = "Call Center_Foreign Language Interpreter and TTY Availability",
                "Unfair Appeals Decisions" = "Reviewing Appeals Decisions") %>% 
  mutate(across(c(6, 7, 8, 9, 12, 13, 14), ~{100-.}),
         across(c(6:9, 11:14), ~{./100}))

rating19_4<- rating19_3 %>% 
  pivot_longer(cols = "Not Getting Needed Care":"TTY Services and Foreign Language Interpretation Unavailable When Needed",
               names_to = "measure",
               values_to = "ratings") %>% 
  mutate(year = "2019")


#2020
rating20 <- as.data.frame(rating20[, c(1:5, 27:38)])
#make the next row(rating variables) to be column names
names(rating20) <- rating20[1,]
rating20 <- rating20[-1,]
colnames(rating20)[-c(1:5)] <- rating20[1, -c(1:5)]
#remove the time frame row
rating20 <- rating20[-c(1,2),]
#rename the broken column
colnames(rating20)[17] <- "C34: Call Center_Foreign Language Interpreter and TTY Availability"

asNum <- function(x, na.rm = FALSE)(as.numeric(x))
rating20_2 <- rating20 %>% 
  select(-"C30: Health Plan Quality Improvement") %>% 
  rename_with(~str_remove(., "C\\d+: "), contains(":")) %>% 
  mutate(across(c("Members Choosing to Leave the Plan","Plan Makes Timely Decisions about Appeals",
                  "Reviewing Appeals Decisions", "Call Center_Foreign Language Interpreter and TTY Availability"), ~str_remove(., "%")),
         across(c(6:16), asNum)) %>% 
  filter(across(c(6:16), ~!is.na(.)))

rating20_3 <- rating20_2 %>% 
  select(-c("Rating of Health Care Quality", "Rating of Health Plan")) %>% 
  dplyr::rename("Not Getting Needed Care" = "Getting Needed Care",
                "Less Timely Care and Appointments" = "Getting Appointments and Care Quickly",
                "Difficult to Get Information and Help from the Plan When Needed" = "Customer Service",
                "Plan Coordinates Members’ Care Poorly" = "Care Coordination",
                "Less Timely Decisions about Appeals" = "Plan Makes Timely Decisions about Appeals",
                "TTY Services and Foreign Language Interpretation Unavailable When Needed" = "Call Center_Foreign Language Interpreter and TTY Availability",
                "Unfair Appeals Decisions" = "Reviewing Appeals Decisions") %>% 
  mutate(across(c(6, 7, 8, 9, 12, 13, 14), ~{100-.}),
         across(c(6:9, 11:14), ~{./100}))

rating20_4<- rating20_3 %>% 
  pivot_longer(cols = "Not Getting Needed Care":"TTY Services and Foreign Language Interpretation Unavailable When Needed",
               names_to = "measure",
               values_to = "ratings") %>% 
  mutate(year = "2020")


#2021
rating21 <- as.data.frame(rating21[, c(1:5, 26:37)])
#make the next row(rating variables) to be column names
names(rating21) <- rating21[1,]
rating21 <- rating21[-1,]
colnames(rating21)[-c(1:5)] <- rating21[1, -c(1:5)]
#remove the time frame row
rating21 <- rating21[-c(1,2),]
#rename the broken column
colnames(rating21)[17] <- "C34: Call Center_Foreign Language Interpreter and TTY Availability"

asNum <- function(x, na.rm = FALSE)(as.numeric(x))
rating21_2 <- rating21 %>% 
  select(-"C29: Health Plan Quality Improvement") %>% 
  rename_with(~str_remove(., "C\\d+: "), contains(":")) %>% 
  mutate(across(c("Members Choosing to Leave the Plan","Plan Makes Timely Decisions about Appeals",
                  "Reviewing Appeals Decisions", "Call Center_Foreign Language Interpreter and TTY Availability"), ~str_remove(., "%")),
         across(c(6:16), asNum)) %>% 
  filter(across(c(6:16), ~!is.na(.)))

rating21_3 <- rating21_2 %>% 
  select(-c("Rating of Health Care Quality", "Rating of Health Plan")) %>% 
  dplyr::rename("Not Getting Needed Care" = "Getting Needed Care",
                "Less Timely Care and Appointments" = "Getting Appointments and Care Quickly",
                "Difficult to Get Information and Help from the Plan When Needed" = "Customer Service",
                "Plan Coordinates Members’ Care Poorly" = "Care Coordination",
                "Less Timely Decisions about Appeals" = "Plan Makes Timely Decisions about Appeals",
                "TTY Services and Foreign Language Interpretation Unavailable When Needed" = "Call Center_Foreign Language Interpreter and TTY Availability",
                "Unfair Appeals Decisions" = "Reviewing Appeals Decisions") %>% 
  mutate(across(c(6, 7, 8, 9, 12, 13, 14), ~{100-.}),
         across(c(6:9, 11:14), ~{./100}))

rating21_4<- rating21_3 %>% 
  pivot_longer(cols = "Not Getting Needed Care":"TTY Services and Foreign Language Interpretation Unavailable When Needed",
               names_to = "measure",
               values_to = "ratings") %>% 
  mutate(year = "2021")

#combine all years
rating <- bind_rows(rating16_4, rating17_4, rating18_4, rating19_4, rating20_4, rating21_4)
rating_words <- rating %>% 
  unnest_tokens(output = sentences, input = measure, token = "sentences") %>%
  group_by(sentences, year) %>% 
  summarise(mean = mean(ratings)) %>% 
  mutate(year=as.integer(year))
write_csv(rating_words, "data/rating_words.csv")

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

