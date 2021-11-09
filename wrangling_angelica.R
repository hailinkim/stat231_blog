library(readr)
library(tidyverse)
rating <- read_csv("/Users/angelica/Desktop/2019 Part C and D Medicare Star Ratings Data (v04 12 2019) [ZIP, 9MB]/2019 Star Ratings Fall Release (11_2018)/stars.csv")
write_csv(rating, "rating.csv")
master <- read_csv("rating.csv")
rating <- as.data.frame(master[, c(1:5, 28:39)])


names(rating) <- rating[1,]
colnames(rating) <- rating[1, c(6:11)]
tmp <- as.data.frame(rating[1, c(6:11)])
names(tmp) <- tmp[1,]
rating <- rating[-1,]
coverage <- read_csv("countylevel_2019.csv")
autauga2 <- coverage %>% 
  filter(county_name == "Autauga County") %>% 
  count(agecat, racecat, sexcat, iprcat)

