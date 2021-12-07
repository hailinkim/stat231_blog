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
  select(SEX_A, ORIENT_A, HISPALLP_A, EDUC_A, NOTCOV_A, CITZNSTP_A, FAMINCTC_A) %>% 
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
    income = FAMINCTC_A 
  )

#checking the distribution of income
ggplot(demographic, aes(x = income)) +
  geom_histogram()

ggplot(demographic, aes(x = sqrt(income))) +
  geom_histogram()

demographic2 <- demographic %>% 
  filter(across(sex:citizen, ~. != "Other")) %>% 
  mutate(income_sqrt = sqrt(income),
         across(where(is.character), as.factor)) %>% 
  select(-c(SEX_A:FAMINCTC_A, income))

write_csv(demographic2, "data/demographic.csv")

demographic_coverage_count <- demographic2 %>% 
  count(coverage)

write_csv(demographic_coverage_count, "data/demographic_coverage_count.csv")

################
#stratified random samples by coverage status
set.seed(112)
demographic_coverage <- demographic2 %>% 
  group_by(coverage) %>% 
  sample_n(2500)

gower_df <- daisy(demographic_coverage, metric = "gower")

silhouette <- c()
silhouette = c(silhouette, NA)
for(i in 2:10){
  pam_clusters = pam(as.matrix(gower_df),
                     diss = TRUE,
                     k = i)
  silhouette = c(silhouette, pam_clusters$silinfo$avg.width)
}

plot(1:10, silhouette,
     xlab = "Clusters",
     ylab = "Silhouette Width")
lines(1:10, silhouette)

pam_nhis = pam(gower_df, diss = TRUE, k = 5)

demographic_coverage[pam_nhis$medoids, ] #medioids represented by row numbers
pam_summary  <- demographic_coverage %>%
  ungroup() %>% 
  mutate(cluster = pam_nhis$clustering) %>%
  group_by(cluster) %>%
  do(cluster_summary = summary(.))
pam_summary$cluster_summary[5]


#stratified random samples by race
set.seed(1294)
demographic_race <- demographic2 %>% 
  group_by(race, coverage) %>% 
  sample_n(75) %>% 
  ungroup() %>% 
  mutate(across(where(is.character), as.factor))

write_csv(demographic_race, "data/demographic_race.csv")

#compute Gower distance matrix
gower_df2 <- daisy(demographic_race, metric = "gower")

#compute silhouette coefficients for differnet numbers of clusters
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

#run PAM clustering with 10 clusters
pam_nhis2 = pam(gower_df2, diss = TRUE, k = 10)

#clustering summary
pam_summary2 <- demographic_race %>%
  ungroup() %>% 
  mutate(cluster = pam_nhis2$clustering) %>%
  group_by(cluster) %>%
  do(cluster_summary = summary(.))

#summary for cluster 10
pam_summary2$cluster_summary[10]

#medioids table
medioids_race <- demographic_race[pam_nhis2$medoids, ] %>% 
  arrange(coverage, desc(income_sqrt))

write_csv(medioids_race, "data/medioids_race.csv")

#create tsne object for visualization/projections by using t-sne technique 
tsne_object <- Rtsne(gower_df2, is_distance = TRUE)
tsne_df <- tsne_object$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_nhis2$clustering))

#added cluster number to which each row belongs, to the stratified random sampled data set
cluster10 <- demographic_race %>%
  ungroup() %>% 
  mutate(cluster = factor(pam_nhis2$clustering))

#combined tsne object and initial data set for visualization
  #to display demographic info in the viz when hovered over
tsne_race <- cluster10 %>% 
  #used cbind instead of join functions from dplyr to join two data sets as is
  cbind(tsne_df) %>% 
  #remove redundant columns
  select(-11)

write_csv(tsne_race, "blog-clustering-race/tsne_race.csv")


#stratified random samples by sexual orientation
set.seed(1298)
demographic_sex <- demographic2 %>% 
  filter(sex_orientation != "Something else") %>%
  group_by(sex_orientation, coverage) %>% 
  sample_n(30) %>% 
  ungroup() %>% 
  mutate(across(where(is.character), as.factor))

write_csv(tmp3, "data/demographic_sex_orientation.csv")

gower_df3 <- daisy(demographic_sex, metric = "gower")

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

pam_nhis3 = pam(gower_df3, diss = TRUE, k = 8)

medioids_sex <- demographic_sex[pam_nhis3$medoids, ] %>% 
  arrange(coverage, desc(income_sqrt))

write_csv(medioids_sex, "data/medioids_sex.csv")

pam_summary3 <- demographic_sex %>%
  ungroup() %>% 
  mutate(cluster = pam_nhis3$clustering) %>%
  group_by(cluster) %>%
  do(cluster_summary = summary(.))
pam_summary3$cluster_summary[[1]]

tsne_object3 <- Rtsne(gower_df3, is_distance = TRUE)

tsne_df3 <- tsne_object3$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_nhis3$clustering))

cluster8 <- tmp3 %>%
  ungroup() %>% 
  mutate(cluster = as.factor(pam_nhis3$clustering))

tsne_sex <- cluster8 %>% 
  cbind(tsne_df3) %>% 
  select(-11)

write_csv(tsne_sex, "blog-clustering-sex/tsne_sex.csv")
