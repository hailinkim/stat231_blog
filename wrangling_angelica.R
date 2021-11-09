library(readr)
library(tidyverse)
library(dplyr)
rating <- read_csv("/Users/angelica/Desktop/2019 Part C and D Medicare Star Ratings Data (v04 12 2019) [ZIP, 9MB]/2019 Star Ratings Fall Release (11_2018)/stars.csv")
write_csv(rating, "rating.csv")
master <- read_csv("rating.csv")
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




coverage <- read_csv("countylevel_2019.csv")
autauga2 <- coverage %>% 
  filter(county_name == "Autauga County") %>% 
  count(agecat, racecat, sexcat, iprcat)

