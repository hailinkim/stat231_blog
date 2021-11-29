# load packages
library(readr)  
library(dplyr)
library(tidyverse)
library(maps)
library(ggplot2)
library(sf)
library(viridis)

<<<<<<< HEAD
data_map <- read_csv("data_map.csv")
# # Import data 
# sahie_2019 <- read_csv("sahie_2019.csv")
# sahie_2018 <- read_csv("sahie_2018.csv")
# sahie_2017 <- read_csv("sahie_2017.csv")
# sahie_2016 <- read_csv("sahie_2016.csv")
# sahie_2015 <- read_csv("sahie_2015.csv")
# sahie_2014 <- read_csv("sahie_2014.csv")
# sahie_2013 <- read_csv("sahie_2013.csv")
# sahie_2012 <- read_csv("sahie_2012.csv")
# 
# # data wrangling
# ## 2019
# data_2019 <- sahie_2019%>%
#   select(year, agecat, racecat, sexcat, iprcat, NIPR, NUI, NIC, PCTUI, PCTIC, PCTELIG, PCTLIIC, state_name, county_name)%>%
#   mutate(
#     Age = case_when(
#       agecat == 0 ~ "Under 65",
#       agecat == 1 ~ "18-64",
#       agecat == 2 ~ "40-64",
#       agecat == 3 ~ "50-64",
#       agecat == 4 ~ "Under 19",
#       agecat == 5 ~ "21-64"),
#     Race = case_when(
#       racecat == 0 ~ "All Races",
#       racecat == 1 ~"White",
#       racecat == 2 ~ "Black",
#       racecat == 3 ~"Hispanic"),
#     Sex = case_when(
#       sexcat == 0 ~"Both Sexes",
#       sexcat == 1 ~"Male",
#       sexcat == 2 ~"Female"
#     ),
#     Income = case_when(
#       iprcat == 0 ~"All income levels",
#       iprcat == 1 ~"At or below 200% of poverty",
#       iprcat == 2 ~"At or below 250% of poverty",
#       iprcat == 3 ~"At or below 138% of poverty",
#       iprcat == 4 ~"At or below 400% of poverty",
#       iprcat == 5 ~"Between 138% - 400% of poverty"
#     ))
# 
# DATA2_2019 <- data_2019%>%
#   select(year, Age, Race, Sex, Income, NUI, NIC, PCTUI, state_name)%>%
#   rename(Year = year, Uninsured = NUI, Insured = NIC,"% Uninsured"= PCTUI, State = state_name)%>%
#   filter(Income == "All income levels" & Age == "Under 65" & Sex == "Both Sexes")%>%
#   subset(Race!="All Races")
# 
# data_2019_filter <- data_2019%>%
#   select(year, Age, Race, Sex, Income, NUI, NIC, PCTUI, state_name, county_name)%>%
#   rename(Year = year, Uninsured = NUI, Insured = NIC,"% Uninsured"= PCTUI, State = state_name)%>%
#   filter(Income == "All income levels" & Age == "Under 65" & Sex == "Both Sexes" & is.na(county_name) & Race == "All Races")
# 
# ## 2018
# 
# data_2018 <- sahie_2018%>%
#   select(year, agecat, racecat, sexcat, iprcat, NIPR, NUI, NIC, PCTUI, PCTIC, PCTELIG, PCTLIIC, state_name, county_name)%>%
#   mutate(
#     Age = case_when(
#       agecat == 0 ~ "Under 65",
#       agecat == 1 ~ "18-64",
#       agecat == 2 ~ "40-64",
#       agecat == 3 ~ "50-64",
#       agecat == 4 ~ "Under 19",
#       agecat == 5 ~ "21-64"),
#     Race = case_when(
#       racecat == 0 ~ "All Races",
#       racecat == 1 ~"White",
#       racecat == 2 ~ "Black",
#       racecat == 3 ~"Hispanic"),
#     Sex = case_when(
#       sexcat == 0 ~"Both Sexes",
#       sexcat == 1 ~"Male",
#       sexcat == 2 ~"Female"
#     ),
#     Income = case_when(
#       iprcat == 0 ~"All income levels",
#       iprcat == 1 ~"At or below 200% of poverty",
#       iprcat == 2 ~"At or below 250% of poverty",
#       iprcat == 3 ~"At or below 138% of poverty",
#       iprcat == 4 ~"At or below 400% of poverty",
#       iprcat == 5 ~"Between 138% - 400% of poverty"
#     ))
# 
# DATA2_2018 <- data_2018%>%
#   select(year, Age, Race, Sex, Income, NUI, NIC, PCTUI, state_name)%>%
#   rename(Year = year, Uninsured = NUI, Insured = NIC, "% Uninsured"= PCTUI,State = state_name)%>%
#   filter(Income == "All income levels" & Age == "Under 65" & Sex == "Both Sexes")%>%
#   subset(Race!="All Races")
# 
# data_2018_filter <- data_2018%>%
#   select(year, Age, Race, Sex, Income, NUI, NIC, PCTUI, state_name, county_name)%>%
#   rename(Year = year, Uninsured = NUI, Insured = NIC,"% Uninsured"= PCTUI, State = state_name)%>%
#   filter(Income == "All income levels" & Age == "Under 65" & Sex == "Both Sexes" & is.na(county_name) & Race == "All Races")
# 
# # 2017
# 
# data_2017 <- sahie_2017%>%
#   select(year, agecat, racecat, sexcat, iprcat, NIPR, NUI, NIC, PCTUI, PCTIC, PCTELIG, PCTLIIC, state_name, county_name)%>%
#   mutate(
#     Age = case_when(
#       agecat == 0 ~ "Under 65",
#       agecat == 1 ~ "18-64",
#       agecat == 2 ~ "40-64",
#       agecat == 3 ~ "50-64",
#       agecat == 4 ~ "Under 19",
#       agecat == 5 ~ "21-64"),
#     Race = case_when(
#       racecat == 0 ~ "All Races",
#       racecat == 1 ~"White",
#       racecat == 2 ~ "Black",
#       racecat == 3 ~"Hispanic"),
#     Sex = case_when(
#       sexcat == 0 ~"Both Sexes",
#       sexcat == 1 ~"Male",
#       sexcat == 2 ~"Female"
#     ),
#     Income = case_when(
#       iprcat == 0 ~"All income levels",
#       iprcat == 1 ~"At or below 200% of poverty",
#       iprcat == 2 ~"At or below 250% of poverty",
#       iprcat == 3 ~"At or below 138% of poverty",
#       iprcat == 4 ~"At or below 400% of poverty",
#       iprcat == 5 ~"Between 138% - 400% of poverty"
#     ))
# 
# DATA2_2017 <- data_2017%>%
#   select(year, Age, Race, Sex, Income, NUI, NIC,PCTUI, state_name)%>%
#   rename(Year = year, Uninsured = NUI, Insured = NIC, "% Uninsured"= PCTUI, State = state_name)%>%
#   filter(Income == "All income levels" & Age == "Under 65" & Sex == "Both Sexes")%>%
#   subset(Race!="All Races")
# 
# data_2017_filter <- data_2017%>%
#   select(year, Age, Race, Sex, Income, NUI, NIC, PCTUI, state_name, county_name)%>%
#   rename(Year = year, Uninsured = NUI, Insured = NIC,"% Uninsured"= PCTUI, State = state_name)%>%
#   filter(Income == "All income levels" & Age == "Under 65" & Sex == "Both Sexes" & is.na(county_name) & Race == "All Races")
# 
# ## 2016
# data_2016 <- sahie_2016%>%
#   select(year, agecat, racecat, sexcat, iprcat, NIPR, NUI, NIC, PCTUI, PCTIC, PCTELIG, PCTLIIC, state_name, county_name)%>%
#   mutate(
#     Age = case_when(
#       agecat == 0 ~ "Under 65",
#       agecat == 1 ~ "18-64",
#       agecat == 2 ~ "40-64",
#       agecat == 3 ~ "50-64",
#       agecat == 4 ~ "Under 19",
#       agecat == 5 ~ "21-64"),
#     Race = case_when(
#       racecat == 0 ~ "All Races",
#       racecat == 1 ~"White",
#       racecat == 2 ~ "Black",
#       racecat == 3 ~"Hispanic"),
#     Sex = case_when(
#       sexcat == 0 ~"Both Sexes",
#       sexcat == 1 ~"Male",
#       sexcat == 2 ~"Female"
#     ),
#     Income = case_when(
#       iprcat == 0 ~"All income levels",
#       iprcat == 1 ~"At or below 200% of poverty",
#       iprcat == 2 ~"At or below 250% of poverty",
#       iprcat == 3 ~"At or below 138% of poverty",
#       iprcat == 4 ~"At or below 400% of poverty",
#       iprcat == 5 ~"Between 138% - 400% of poverty"
#     ))
# 
# DATA2_2016 <- data_2016%>%
#   select(year, Age, Race, Sex, Income, NUI, NIC, PCTUI, state_name)%>%
#   rename(Year = year, Uninsured = NUI, Insured = NIC, "% Uninsured"= PCTUI, State = state_name)%>%
#   filter(Income == "All income levels" & Age == "Under 65" & Sex == "Both Sexes")%>%
#   subset(Race!="All Races")
# 
# data_2016_filter <- data_2016%>%
#   select(year, Age, Race, Sex, Income, NUI, NIC, PCTUI, state_name, county_name)%>%
#   rename(Year = year, Uninsured = NUI, Insured = NIC,"% Uninsured"= PCTUI, State = state_name)%>%
#   filter(Income == "All income levels" & Age == "Under 65" & Sex == "Both Sexes" & is.na(county_name) & Race == "All Races")
# 
# ## 2015
# data_2015 <- sahie_2015%>%
#   select(year, agecat, racecat, sexcat, iprcat, NIPR, NUI, NIC, PCTUI, PCTIC, PCTELIG, PCTLIIC, state_name, county_name)%>%
#   mutate(
#     Age = case_when(
#       agecat == 0 ~ "Under 65",
#       agecat == 1 ~ "18-64",
#       agecat == 2 ~ "40-64",
#       agecat == 3 ~ "50-64",
#       agecat == 4 ~ "Under 19",
#       agecat == 5 ~ "21-64"),
#     Race = case_when(
#       racecat == 0 ~ "All Races",
#       racecat == 1 ~"White",
#       racecat == 2 ~ "Black",
#       racecat == 3 ~"Hispanic"),
#     Sex = case_when(
#       sexcat == 0 ~"Both Sexes",
#       sexcat == 1 ~"Male",
#       sexcat == 2 ~"Female"
#     ),
#     Income = case_when(
#       iprcat == 0 ~"All income levels",
#       iprcat == 1 ~"At or below 200% of poverty",
#       iprcat == 2 ~"At or below 250% of poverty",
#       iprcat == 3 ~"At or below 138% of poverty",
#       iprcat == 4 ~"At or below 400% of poverty",
#       iprcat == 5 ~"Between 138% - 400% of poverty"
#     ))
# 
# DATA2_2015 <- data_2015%>%
#   select(year, Age, Race, Sex, Income, NUI, NIC, PCTUI, state_name)%>%
#   rename(Year = year, Uninsured = NUI, Insured = NIC,"% Uninsured"=PCTUI, State = state_name)%>%
#   filter(Income == "All income levels" & Age == "Under 65" & Sex == "Both Sexes")%>%
#   subset(Race!="All Races")
# 
# data_2015_filter <- data_2015%>%
#   select(year, Age, Race, Sex, Income, NUI, NIC, PCTUI, state_name, county_name)%>%
#   rename(Year = year, Uninsured = NUI, Insured = NIC,"% Uninsured"= PCTUI, State = state_name)%>%
#   filter(Income == "All income levels" & Age == "Under 65" & Sex == "Both Sexes" & is.na(county_name) & Race == "All Races")
# 
# ## 2014
# data_2014 <- sahie_2014%>%
#   select(year, agecat, racecat, sexcat, iprcat, NIPR, NUI, NIC, PCTUI, PCTIC, PCTELIG, PCTLIIC, state_name, county_name)%>%
#   mutate(
#     Age = case_when(
#       agecat == 0 ~ "Under 65",
#       agecat == 1 ~ "18-64",
#       agecat == 2 ~ "40-64",
#       agecat == 3 ~ "50-64",
#       agecat == 4 ~ "Under 19",
#       agecat == 5 ~ "21-64"),
#     Race = case_when(
#       racecat == 0 ~ "All Races",
#       racecat == 1 ~"White",
#       racecat == 2 ~ "Black",
#       racecat == 3 ~"Hispanic"),
#     Sex = case_when(
#       sexcat == 0 ~"Both Sexes",
#       sexcat == 1 ~"Male",
#       sexcat == 2 ~"Female"
#     ),
#     Income = case_when(
#       iprcat == 0 ~"All income levels",
#       iprcat == 1 ~"At or below 200% of poverty",
#       iprcat == 2 ~"At or below 250% of poverty",
#       iprcat == 3 ~"At or below 138% of poverty",
#       iprcat == 4 ~"At or below 400% of poverty",
#       iprcat == 5 ~"Between 138% - 400% of poverty"
#     ))
# 
# DATA2_2014 <- data_2014%>%
#   select(year, Age, Race, Sex, Income, NUI, NIC, PCTUI, state_name)%>%
#   rename(Year = year, Uninsured = NUI, Insured = NIC, "% Uninsured"= PCTUI, State = state_name)%>%
#   filter(Income == "All income levels" & Age == "Under 65" & Sex == "Both Sexes")%>%
#   subset(Race!="All Races")
# 
# data_2014_filter <- data_2014%>%
#   select(year, Age, Race, Sex, Income, NUI, NIC, PCTUI, state_name, county_name)%>%
#   rename(Year = year, Uninsured = NUI, Insured = NIC,"% Uninsured"= PCTUI, State = state_name)%>%
#   filter(Income == "All income levels" & Age == "Under 65" & Sex == "Both Sexes" & is.na(county_name) & Race == "All Races")
# 
# ## 2013
# data_2013 <- sahie_2013%>%
#   select(year, agecat, racecat, sexcat, iprcat, NIPR, NUI, NIC, PCTUI, PCTIC, PCTELIG, PCTLIIC, state_name, county_name)%>%
#   mutate(
#     Age = case_when(
#       agecat == 0 ~ "Under 65",
#       agecat == 1 ~ "18-64",
#       agecat == 2 ~ "40-64",
#       agecat == 3 ~ "50-64",
#       agecat == 4 ~ "Under 19",
#       agecat == 5 ~ "21-64"),
#     Race = case_when(
#       racecat == 0 ~ "All Races",
#       racecat == 1 ~"White",
#       racecat == 2 ~ "Black",
#       racecat == 3 ~"Hispanic"),
#     Sex = case_when(
#       sexcat == 0 ~"Both Sexes",
#       sexcat == 1 ~"Male",
#       sexcat == 2 ~"Female"
#     ),
#     Income = case_when(
#       iprcat == 0 ~"All income levels",
#       iprcat == 1 ~"At or below 200% of poverty",
#       iprcat == 2 ~"At or below 250% of poverty",
#       iprcat == 3 ~"At or below 138% of poverty",
#       iprcat == 4 ~"At or below 400% of poverty",
#       iprcat == 5 ~"Between 138% - 400% of poverty"
#     ))
# 
# DATA2_2013 <- data_2013%>%
#   select(year, Age, Race, Sex, Income, NUI, NIC, PCTUI, state_name)%>%
#   rename(Year = year, Uninsured = NUI, Insured = NIC,"% Uninsured"= PCTUI, State = state_name)%>%
#   filter(Income == "All income levels" & Age == "Under 65" & Sex == "Both Sexes")%>%
#   subset(Race!="All Races")
# 
# data_2013_filter <- data_2013%>%
#   select(year, Age, Race, Sex, Income, NUI, NIC, PCTUI, state_name, county_name)%>%
#   rename(Year = year, Uninsured = NUI, Insured = NIC,"% Uninsured"= PCTUI, State = state_name)%>%
#   filter(Income == "All income levels" & Age == "Under 65" & Sex == "Both Sexes" & is.na(county_name) & Race == "All Races")
# 
# ## 2012
# data_2012 <- sahie_2012%>%
#   select(year, agecat, racecat, sexcat, iprcat, NIPR, NUI, NIC, PCTUI, PCTIC, PCTELIG, PCTLIIC, state_name, county_name)%>%
#   mutate(
#     Age = case_when(
#       agecat == 0 ~ "Under 65",
#       agecat == 1 ~ "18-64",
#       agecat == 2 ~ "40-64",
#       agecat == 3 ~ "50-64",
#       agecat == 4 ~ "Under 19",
#       agecat == 5 ~ "21-64"),
#     Race = case_when(
#       racecat == 0 ~ "All Races",
#       racecat == 1 ~"White",
#       racecat == 2 ~ "Black",
#       racecat == 3 ~"Hispanic"),
#     Sex = case_when(
#       sexcat == 0 ~"Both Sexes",
#       sexcat == 1 ~"Male",
#       sexcat == 2 ~"Female"
#     ),
#     Income = case_when(
#       iprcat == 0 ~"All income levels",
#       iprcat == 1 ~"At or below 200% of poverty",
#       iprcat == 2 ~"At or below 250% of poverty",
#       iprcat == 3 ~"At or below 138% of poverty",
#       iprcat == 4 ~"At or below 400% of poverty",
#       iprcat == 5 ~"Between 138% - 400% of poverty"
#     ))
# 
# DATA2_2012 <- data_2012%>%
#   select(year, Age, Race, Sex, Income, NUI, NIC, PCTUI, state_name)%>%
#   rename(Year = year, Uninsured = NUI, Insured = NIC, "% Uninsured"= PCTUI, State = state_name)%>%
#   filter(Income == "All income levels" & Age == "Under 65" & Sex == "Both Sexes")%>%
#   subset(Race!="All Races")
# 
# data_2012_filter <- data_2012%>%
#   select(year, Age, Race, Sex, Income, NUI, NIC, PCTUI, state_name, county_name)%>%
#   rename(Year = year, Uninsured = NUI, Insured = NIC,"% Uninsured"= PCTUI, State = state_name)%>%
#   filter(Income == "All income levels" & Age == "Under 65" & Sex == "Both Sexes" & is.na(county_name) & Race == "All Races")
# 
# # merge
# totalDATA <- rbind(DATA2_2018, DATA2_2019, DATA2_2017, DATA2_2016, DATA2_2015, DATA2_2014, DATA2_2013, DATA2_2012)%>%
#   rename(Percent_Uninsured = "% Uninsured")
# # for map
# total_data <- rbind(data_2012_filter, data_2013_filter, data_2014_filter, data_2015_filter, data_2016_filter, data_2017_filter, data_2018_filter, data_2018_filter, data_2019_filter)%>%
#   rename(Percent_Uninsured = "% Uninsured")%>%
#   select(Year, Age, Race, Sex, Income, Uninsured, Insured, Percent_Uninsured, State)
# 
# # map wrangling
# data(state) 
# state_info <- data.frame(Region = state.region,
#                          # Match state variable name in map data
#                          ID = tolower(state.name), 
#                          # Match state variable name in summary data
#                          State = state.abb)
# head(state_info)
# state_df <- map_data("state") 
# usa_map <- maps::map("state",
#                      plot = FALSE, fill = TRUE) %>% 
#   st_as_sf()
# #total years
# total_data <- total_data %>% 
#   mutate(state = tolower(State),
#          Percent_Uninsured = as.numeric(Percent_Uninsured))
# data_map <- usa_map %>% 
#   left_join(state_info, by = "ID") %>% 
#   left_join(total_data, by = c("ID" = "state"))
# head(data_map)
# 
# #2012
# data_2012_filter<- data_2012_filter%>%
#   rename(Percent_Uninsured = "% Uninsured")%>%
#   mutate(state = tolower(State),
#          Percent_Uninsured = as.numeric(Percent_Uninsured))
# data_map_2012 <- usa_map %>% 
#   left_join(state_info, by = "ID") %>% 
#   left_join(data_2012_filter, by = c("ID" = "state"))
# #2013
# data_2013_filter<- data_2013_filter%>%
#   rename(Percent_Uninsured = "% Uninsured")%>%
#   mutate(state = tolower(State),
#          Percent_Uninsured = as.numeric(Percent_Uninsured))
# data_map_2013 <- usa_map %>% 
#   left_join(state_info, by = "ID") %>% 
#   left_join(data_2013_filter, by = c("ID" = "state"))
# #2014
# data_2014_filter<- data_2014_filter%>%
#   rename(Percent_Uninsured = "% Uninsured")%>%
#   mutate(state = tolower(State),
#          Percent_Uninsured = as.numeric(Percent_Uninsured))
# data_map_2014 <- usa_map %>% 
#   left_join(state_info, by = "ID") %>% 
#   left_join(data_2014_filter, by = c("ID" = "state"))
# #2015
# data_2015_filter<- data_2015_filter%>%
#   rename(Percent_Uninsured = "% Uninsured")%>%
#   mutate(state = tolower(State),
#          Percent_Uninsured = as.numeric(Percent_Uninsured))
# data_map_2015 <- usa_map %>% 
#   left_join(state_info, by = "ID") %>% 
#   left_join(data_2015_filter, by = c("ID" = "state"))
# #2016
# data_2016_filter<- data_2016_filter%>%
#   rename(Percent_Uninsured = "% Uninsured")%>%
#   mutate(state = tolower(State),
#          Percent_Uninsured = as.numeric(Percent_Uninsured))
# data_map_2016 <- usa_map %>% 
#   left_join(state_info, by = "ID") %>% 
#   left_join(data_2016_filter, by = c("ID" = "state"))
# #2017
# data_2017_filter<- data_2017_filter%>%
#   rename(Percent_Uninsured = "% Uninsured")%>%
#   mutate(state = tolower(State),
#          Percent_Uninsured = as.numeric(Percent_Uninsured))
# data_map_2017 <- usa_map %>% 
#   left_join(state_info, by = "ID") %>% 
#   left_join(data_2017_filter, by = c("ID" = "state"))
# #2018
# data_2018_filter<- data_2018_filter%>%
#   rename(Percent_Uninsured = "% Uninsured")%>%
#   mutate(state = tolower(State),
#          Percent_Uninsured = as.numeric(Percent_Uninsured))
# data_map_2018 <- usa_map %>% 
#   left_join(state_info, by = "ID") %>% 
#   left_join(data_2018_filter, by = c("ID" = "state"))
# #2019
# data_2019_filter<- data_2019_filter%>%
#   rename(Percent_Uninsured = "% Uninsured")%>%
#   mutate(state = tolower(State),
#          Percent_Uninsured = as.numeric(Percent_Uninsured))
# data_map_2019 <- usa_map %>% 
#   left_join(state_info, by = "ID") %>% 
#   left_join(data_2019_filter, by = c("ID" = "state"))
# 


# For widgets: 
race_choices <- unique(totalDATA$Race)
state_choices <- unique(totalDATA$State)
=======
total_data <- read_csv("total_data.csv")

# map wrangling
data(state)
state_info <- data.frame(Region = state.region,
                         # Match state variable name in map data
                         ID = tolower(state.name),
                         # Match state variable name in summary data
                         State = state.abb)

state_df <- map_data("state")
usa_map <- maps::map("state",
                     plot = FALSE, fill = TRUE) %>%
  st_as_sf()

data_map <- usa_map %>%
  left_join(state_info, by = "ID") %>%
  left_join(total_data, by = c("ID" = "state"))
>>>>>>> 97d7f27804d78ee96ad82a40b97c58567621e7e4


###### ui ######
ui <- navbarPage(
  title = "blog",
  # tab 2
  tabPanel(
    title = "Map",
    sidebarLayout(
      sidebarPanel(
        sliderInput(
        # slider for year
        inputId = "animation",
          label = "Select Years:",
          value = 2012,
          min = 2012,
          max = 2019,
          step = NULL,
          round = TRUE,
          ticks = TRUE,
          animate = 
            animationOptions(interval = 1000, loop = TRUE))
        ),
      mainPanel(
        plotOutput(outputId = "map"))
      )
    )
)


##### server #####
server  <- function(input, output){

  output$map <- renderPlot({
    dat <- data_map %>%
      filter(Year %in% input$animation)
    
    ggplot(data = dat, aes(fill = Percent_Uninsured)) +
    geom_sf() +
    scale_fill_viridis(option = "turbo", direction = -1) +
    theme_void()
    
  })
}
shinyApp(ui = ui, server = server)  
