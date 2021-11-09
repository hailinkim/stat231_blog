library(readr)

write_csv(master, "rating.csv")
rating <- read_csv("rating.csv")


names(rating) <- rating[1,]

coverage <- read_csv("countylevel_2019.csv")
autauga2 <- coverage %>% 
  filter(county_name == "Autauga County") %>% 
  count(agecat, racecat, sexcat, iprcat)