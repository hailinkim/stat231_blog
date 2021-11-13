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
rating17 <- read_csv("data/rating/2017.csv")
rating18 <- read_csv("data/rating/2018.csv")
rating19 <- read_csv("data/rating/2019.csv")
rating20 <- read_csv("data/rating/2020.csv")
rating21 <- read_csv("data/rating/2021.csv")

#2016
rating16 <- as.data.frame(rating16[, c(1:5, 25:37)])
#make the next row(rating variables) to be column names
names(rating16) <- rating16[1,]
rating16 <- rating16[-1,]
colnames(rating16)[-c(1:5)] <- rating16[1, -c(1:5)]
#remove the time frame row
rating16 <- rating16[-c(1,2),]
#rename the broken column
colnames(rating16)[18] <- "C34: Call Center_Foreign Language Interpreter and TTY Availability"

rating16_2 <- rating16 %>% 
  select(-"C29: Health Plan Quality Improvement") %>% 
  rename_with(~str_remove(., "C\\d+: "), contains(":")) %>% 
  mutate(across(c("Getting Needed Care": "Call Center_Foreign Language Interpreter and TTY Availability"), ~str_remove(., "%")),
         across(c(6:17), asNum)) %>% 
  drop_na()

rating16_3 <- rating16_2 %>% 
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

rating16_4 <- rating16_3 %>% 
  pivot_longer(cols = "Not Getting Needed Care":"TTY Services and Foreign Language Interpretation Unavailable When Needed",
               names_to = "measure",
               values_to = "ratings") %>% 
  mutate(year = "2016")


#2017
rating17 <- as.data.frame(rating17[, c(1:5, 25:37)])
#make the next row(rating variables) to be column names
names(rating17) <- rating17[1,]
rating17 <- rating17[-1,]
colnames(rating17)[-c(1:5)] <- rating17[1, -c(1:5)]
#remove the time frame row
rating17 <- rating17[-c(1,2),]
#rename the broken column
colnames(rating17)[18] <- "C34: Call Center_Foreign Language Interpreter and TTY Availability"

rating17_2 <- rating17 %>% 
  select(-"C29: Health Plan Quality Improvement") %>% 
  rename_with(~str_remove(., "C\\d+: "), contains(":")) %>% 
  mutate(across(c("Getting Needed Care": "Call Center_Foreign Language Interpreter and TTY Availability"), ~str_remove(., "%")),
         across(c(6:17), asNum)) %>% 
  drop_na()

rating17_3 <- rating17_2 %>% 
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

rating17_4 <- rating17_3 %>% 
  pivot_longer(cols = "Not Getting Needed Care":"TTY Services and Foreign Language Interpretation Unavailable When Needed",
               names_to = "measure",
               values_to = "ratings") %>% 
  mutate(year = "2017")


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


set.seed(53)
rating_words %>%
  with(wordcloud(words = sentences, freq = mean, scale = c(1.5,0.3), max.words = 11))

library(colorspace)
gg <- rating_words %>%
  ggplot(aes(label = sentences, size=mean, color = mean)) +
  geom_text_wordcloud() +
  theme_minimal() +
  scale_color_continuous_diverging(palette = "Blue-Red")
  

gg2 <- gg + transition_time(year) +
  labs(title = 'Year: {frame_time}')

animate(gg2, nframes = 40, fps = 5, end_pause = 5, renderer=gifski_renderer())
anim_save(filename="testing.gif")


# #tried clustering
# set.seed(23)
# rating_km3_out <- rating_wide %>% 
#   select("Rating of Health Care Quality", "Rating of Health Plan") %>% 
#   kmeans(centers = 3, nstart = 20)
# rating2 <- rating_wide %>%
#   mutate(clusters3_scaled = factor(rating_km3_out$cluster))
# ggplot(data = rating2, aes(x = "Rating of Health Care Quality", y = "Rating of Health Plan")) + 
#   geom_point(aes(color = clusters3_scaled),
#              position=position_jitter(h=0.1, w=0.1)) +
#   # ggrepel::geom_text_repel(aes(label = "Contract Name", color = clusters3_scaled), size = 3) +
#   coord_fixed() +
#   geom_point(data = data.frame(rating_km3_out$centers),
#              aes(x = SAT_math25, y = SAT_verbal25),
#              pch = "x", size = 8) +
#   labs(x = "Math SAT (25th percentile)",
#        y = "Verbal SAT (25th percentile)",
#        color = "Cluster assignment")
# 
# 
# GGally::ggpairs(data = rating2, aes(color = clusters3_scaled),
#                 columns = c("Getting Needed Care", "Getting Appointments and Care Quickly",
#                             "Customer Service", "Rating of Health Care Quality",
#                             "Rating of Health Plan", "Care Coordination", "Complaints about the Health Plan",
#                             "Members Choosing to Leave the Plan", "Health Plan Quality Improvement",
#                             "Plan Makes Timely Decisions about Appeals", "Reviewing Appeals Decisions",
#                             "Call Center_Foreign Language Interpreter and TTY Availability"),
#                 upper = list(continuous = "blank"))
# 
# # Clustering using unstandardized variables (for comparison)
# ma2_km3_out_unscaled <- ma_sample2 %>% 
#   select(admit_rate:cost_avg) %>% 
#   kmeans(centers = 3, nstart = 20)