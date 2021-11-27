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

#sex, race, education, citizenship, income group

#30032 observations
demographic2 <- demographic %>% 
  filter(across(sex:citizen, ~. != "Other")) %>% 
  mutate(income_sqrt = sqrt(income)) %>% 
  select(-c(SEX_A:FAMINCTC_A, income))
demographic3 <- demographic2 %>% 
  mutate(across(where(is.character), as.factor))

#checking the distribution of income
ggplot(demographic3, aes(x = income_sqrt)) +
  geom_histogram()


################
#stratified random samples by coverage status
set.seed(112)
tmp <- demographic3 %>% 
  group_by(coverage) %>% 
  sample_n(2500)
gower_df <- daisy(tmp, metric = "gower")
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

tmp[pam_nhis$medoids, ] #mediois represented by row numbers
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
  sample_n(75)
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
  theme_bw() +
  scale_color_manual(values=c(my_pal)) +
  scale_fill_manual(values=c(paste(my_pal, "66", sep = "")))


#stratified random samples by sexual orientation
tmp3 <- demographic3 %>% 
  filter(coverage == "Not covered", sex_orientation != "Something else") %>%
  group_by(sex_orientation) %>% 
  sample_n(30)
write_csv(tmp3, "demographic_sex_orientation.csv")

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

tsne_object3 <- Rtsne(gower_df3, is_distance = TRUE, perplexity = 29)

tsne_df3 <- tsne_object3$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_nhis3$clustering))
cluster3 <- tmp3 %>%
  ungroup() %>% 
  mutate(cluster = as.factor(pam_nhis3$clustering))
tsne <- cluster3 %>% 
  inner_join(tsne_df3, by = "cluster")
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
