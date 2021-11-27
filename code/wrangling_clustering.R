library(readr)
library(tidyverse)
library(dplyr)
library(cluster)
library(Rtsne)
library(ggplot2)
library(ggforce)
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
  mutate(income_sqrt = sqrt(income)) %>% 
  select(-c(SEX_A:RXSK12M_A, citizen:years_in_us, delayed_mental:unafford_med, citizen_years, food_security, income, income_group))

demographic3 <- demographic2 %>% 
  mutate(across(where(is.character), as.factor))

#checking the distribution of income
ggplot(demographic, aes(x = sqrt(income))) +
  geom_histogram()


gower_df <- daisy(demographic3, metric = "gower")
save(gower_df, file = "gower_df.rda")

silhouette3 <- c()
silhouette3 = c(silhouette3, NA)
for(i in 3:8){
  pam_clusters3 = pam(as.matrix(gower_df2),
                     diss = TRUE,
                     k = i)
  silhouette3 = c(silhouette3 ,pam_clusters2$silinfo$avg.width)
}
silhouette2 <- append(silhouette, NA, after = 0)
silhouette2 <- append(silhouette2, c(NA, NA), after = 8)
save(silhouette2, file = "silhouette2.rda")

plot(1:10, silhouette2,
     xlab = "Clusters",
     ylab = "Silhouette Width")
lines(1:10, silhouette2)

pam_nhis = pam(gower_df, diss = TRUE, k = 8)
# pam_nhis = pam(gower_df, diss = TRUE, k=5)
save(pam_nhis, file = "pam_nhis.rda")
demographic3[pam_nhis$medoids, ]
pam_summary <- demographic3 %>%
  mutate(cluster = pam_nhis$clustering) %>%
  group_by(cluster) %>%
  do(cluster_summary = summary(.))
pam_summary$cluster_summary[[3]]

tsne_object <- Rtsne(gower_df, is_distance = TRUE)
save(tsne_object, file = "tsne_object.rda")
tsne_df <- tsne_object$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_nhis$clustering))
ggplot(aes(x = X, y = Y), data = tsne_df) +
  geom_point(aes(color = cluster))



################
#stratified random samples by coverage status
set.seed(1127)
tmp <- demographic3 %>% 
  group_by(coverage) %>% 
  sample_n(1000)
gower_df <- daisy(tmp, metric = "gower")
silhouette <- c()
silhouette = c(silhouette, NA)
for(i in 2:10){
  pam_clusters = pam(as.matrix(gower_df),
                     diss = TRUE,
                     k = i)
  silhouette = c(silhouette ,pam_clusters$silinfo$avg.width)
}
plot(1:10, silhouette,
     xlab = "Clusters",
     ylab = "Silhouette Width")
lines(1:10, silhouette)
pam_nhis = pam(gower_df, diss = TRUE, k = 5)

tmp[pam_nhis$medoids, ]
pam_summary  <- tmp %>%
  ungroup() %>% 
  mutate(cluster = pam_nhis$clustering) %>%
  group_by(cluster) %>%
  do(cluster_summary = summary(.))
pam_summary$cluster_summary[5]


#stratified random samples by race
set.seed(68)
tmp2 <- demographic3 %>% 
  filter(coverage == "Not covered") %>% 
  group_by(race) %>% 
  sample_n(20)
gower_df2 <- daisy(tmp2, metric = "gower")

silhouette2 <- c()
silhouette2 = c(silhouette2, NA)
for(i in 2:10){
  pam_clusters2 = pam(as.matrix(gower_df2),
                     diss = TRUE,
                     k = i)
  silhouette2 = c(silhouette2, pam_clusters2$silinfo$avg.width)
}
plot(1:10, silhouette2,
     xlab = "Clusters",
     ylab = "Silhouette Width")
lines(1:10, silhouette2)
pam_nhis2 = pam(gower_df2, diss = TRUE, k = 7)


tmp2[pam_nhis2$medoids, ]
pam_summary2 <- tmp2 %>%
  ungroup() %>% 
  mutate(cluster = pam_nhis2$clustering) %>%
  group_by(cluster) %>%
  do(cluster_summary = summary(.))
pam_summary2$cluster_summary[[7]]

tsne_object2 <- Rtsne(gower_df2, is_distance = TRUE)
tsne_df2 <- tsne_object2$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_nhis2$clustering))
my_pal <- RColorBrewer::brewer.pal(n=7, name = "Dark2")
ggplot(aes(x = X, y = Y), data = tsne_df2) +
  geom_point(aes(color = cluster, fill = cluster), size = 4, shape = 21) + 
  # geom_mark_ellipse(aes(color = cluster,
  #                       label=cluster),
  #                   expand = unit(0.5,"mm"),
  #                   label.buffer = unit(-5, 'mm'))+
  theme_bw() +
  scale_color_manual(values=c(my_pal)) +
  scale_fill_manual(values=c(paste(my_pal, "66", sep = "")))


#stratified random samples by sexual orientation
tmp3 <- demographic3 %>% 
  filter(coverage == "Not covered", sex_orientation != "Something else") %>%
  group_by(sex_orientation) %>% 
  sample_n(18)
  
gower_df3 <- daisy(tmp3, metric = "gower")

silhouette3 <- c()
silhouette3 = c(silhouette3, NA)
for(i in 2:10){
  pam_clusters3 = pam(as.matrix(gower_df3),
                      diss = TRUE,
                      k = i)
  silhouette3 = c(silhouette3, pam_clusters3$silinfo$avg.width)
}
plot(1:10, silhouette3,
     xlab = "Clusters",
     ylab = "Silhouette Width")
lines(1:10, silhouette3)
pam_nhis3 = pam(gower_df3, diss = TRUE, k = 10)


tmp3[pam_nhis3$medoids, ]
pam_summary3 <- tmp3 %>%
  ungroup() %>% 
  mutate(cluster = pam_nhis3$clustering) %>%
  group_by(cluster) %>%
  do(cluster_summary = summary(.))
pam_summary3$cluster_summary[[1]]

tsne_object3 <- Rtsne(gower_df3, is_distance = TRUE, perplexity = 16)
tsne_df3 <- tsne_object3$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_nhis3$clustering))
my_pal <- RColorBrewer::brewer.pal(n=10, name = "Set3")
ggplot(aes(x = X, y = Y), data = tsne_df3) +
  geom_point(aes(color = cluster, fill = cluster), size = 4, shape = 21) + 
  # geom_mark_ellipse(aes(color = cluster,
  #                       label=cluster),
  #                   expand = unit(0.5,"mm"),
  #                   label.buffer = unit(-5, 'mm'))+
  theme_bw() +
  scale_color_manual(values=c(my_pal)) +
  scale_fill_manual(values=c(paste(my_pal, "66", sep = "")))
