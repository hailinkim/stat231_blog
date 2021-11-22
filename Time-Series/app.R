# Load necessary packages   
library(shiny)     
library(shinythemes)  
library(tidyverse)
library(dplyr) 
library(bslib)
library(thematic)
library(shinyWidgets)  
library(DT)
library(ggrepel)

# Import data 
sahie_2019 <- read_csv("sahie_2019.csv")
sahie_2018 <- read_csv("sahie_2018.csv")
sahie_2017 <- read_csv("sahie_2017.csv")
sahie_2016 <- read_csv("sahie_2016.csv")
sahie_2015 <- read_csv("sahie_2015.csv")
sahie_2014 <- read_csv("sahie_2014.csv")
sahie_2013 <- read_csv("sahie_2013.csv")
sahie_2012 <- read_csv("sahie_2012.csv")


## 2019
data_2019 <- sahie_2019%>%
  select(year, agecat, racecat, sexcat, iprcat, NIPR, NUI, NIC, PCTUI, PCTIC, PCTELIG, PCTLIIC, state_name, county_name)%>%
  mutate(
    Age = case_when(
      agecat == 0 ~ "Under 65",
      agecat == 1 ~ "18-64",
      agecat == 2 ~ "40-64",
      agecat == 3 ~ "50-64",
      agecat == 4 ~ "Under 19",
      agecat == 5 ~ "21-64"),
    Race = case_when(
      racecat == 0 ~ "All Races",
      racecat == 1 ~"White",
      racecat == 2 ~ "Black",
      racecat == 3 ~"Hispanic"),
    Sex = case_when(
      sexcat == 0 ~"Both Sexes",
      sexcat == 1 ~"Male",
      sexcat == 2 ~"Female"
    ),
    Income = case_when(
      iprcat == 0 ~"All income levels",
      iprcat == 1 ~"At or below 200% of poverty",
      iprcat == 2 ~"At or below 250% of poverty",
      iprcat == 3 ~"At or below 138% of poverty",
      iprcat == 4 ~"At or below 400% of poverty",
      iprcat == 5 ~"Between 138% - 400% of poverty"
    ))

DATA2_2019 <- data_2019%>%
  select(year, Age, Race, Sex, Income, NUI, NIC, PCTUI, state_name)%>%
  rename(Year = year, Uninsured = NUI, Insured = NIC, Percent_Uninsured= PCTUI, State = state_name)%>%
  filter(Income == "All income levels" & Age == "Under 65" & Sex == "Both Sexes")%>%
  subset(Race!="All Races")

## 2018

data_2018 <- sahie_2018%>%
  select(year, agecat, racecat, sexcat, iprcat, NIPR, NUI, NIC, PCTUI, PCTIC, PCTELIG, PCTLIIC, state_name, county_name)%>%
  mutate(
    Age = case_when(
      agecat == 0 ~ "Under 65",
      agecat == 1 ~ "18-64",
      agecat == 2 ~ "40-64",
      agecat == 3 ~ "50-64",
      agecat == 4 ~ "Under 19",
      agecat == 5 ~ "21-64"),
    Race = case_when(
      racecat == 0 ~ "All Races",
      racecat == 1 ~"White",
      racecat == 2 ~ "Black",
      racecat == 3 ~"Hispanic"),
    Sex = case_when(
      sexcat == 0 ~"Both Sexes",
      sexcat == 1 ~"Male",
      sexcat == 2 ~"Female"
    ),
    Income = case_when(
      iprcat == 0 ~"All income levels",
      iprcat == 1 ~"At or below 200% of poverty",
      iprcat == 2 ~"At or below 250% of poverty",
      iprcat == 3 ~"At or below 138% of poverty",
      iprcat == 4 ~"At or below 400% of poverty",
      iprcat == 5 ~"Between 138% - 400% of poverty"
    ))

DATA2_2018 <- data_2018%>%
  select(year, Age, Race, Sex, Income, NUI, NIC, PCTUI, state_name)%>%
  rename(Year = year, Uninsured = NUI, Insured = NIC, Percent_Uninsured= PCTUI,State = state_name)%>%
  filter(Income == "All income levels" & Age == "Under 65" & Sex == "Both Sexes")%>%
  subset(Race!="All Races")

## 2017

data_2017 <- sahie_2017%>%
  select(year, agecat, racecat, sexcat, iprcat, NIPR, NUI, NIC, PCTUI, PCTIC, PCTELIG, PCTLIIC, state_name, county_name)%>%
  mutate(
    Age = case_when(
      agecat == 0 ~ "Under 65",
      agecat == 1 ~ "18-64",
      agecat == 2 ~ "40-64",
      agecat == 3 ~ "50-64",
      agecat == 4 ~ "Under 19",
      agecat == 5 ~ "21-64"),
    Race = case_when(
      racecat == 0 ~ "All Races",
      racecat == 1 ~"White",
      racecat == 2 ~ "Black",
      racecat == 3 ~"Hispanic"),
    Sex = case_when(
      sexcat == 0 ~"Both Sexes",
      sexcat == 1 ~"Male",
      sexcat == 2 ~"Female"
    ),
    Income = case_when(
      iprcat == 0 ~"All income levels",
      iprcat == 1 ~"At or below 200% of poverty",
      iprcat == 2 ~"At or below 250% of poverty",
      iprcat == 3 ~"At or below 138% of poverty",
      iprcat == 4 ~"At or below 400% of poverty",
      iprcat == 5 ~"Between 138% - 400% of poverty"
    ))

DATA2_2017 <- data_2017%>%
  select(year, Age, Race, Sex, Income, NUI, NIC,PCTUI, state_name)%>%
  rename(Year = year, Uninsured = NUI, Insured = NIC, Percent_Uninsured= PCTUI, State = state_name)%>%
  filter(Income == "All income levels" & Age == "Under 65" & Sex == "Both Sexes")%>%
  subset(Race!="All Races")

## 2016
data_2016 <- sahie_2016%>%
  select(year, agecat, racecat, sexcat, iprcat, NIPR, NUI, NIC, PCTUI, PCTIC, PCTELIG, PCTLIIC, state_name, county_name)%>%
  mutate(
    Age = case_when(
      agecat == 0 ~ "Under 65",
      agecat == 1 ~ "18-64",
      agecat == 2 ~ "40-64",
      agecat == 3 ~ "50-64",
      agecat == 4 ~ "Under 19",
      agecat == 5 ~ "21-64"),
    Race = case_when(
      racecat == 0 ~ "All Races",
      racecat == 1 ~"White",
      racecat == 2 ~ "Black",
      racecat == 3 ~"Hispanic"),
    Sex = case_when(
      sexcat == 0 ~"Both Sexes",
      sexcat == 1 ~"Male",
      sexcat == 2 ~"Female"
    ),
    Income = case_when(
      iprcat == 0 ~"All income levels",
      iprcat == 1 ~"At or below 200% of poverty",
      iprcat == 2 ~"At or below 250% of poverty",
      iprcat == 3 ~"At or below 138% of poverty",
      iprcat == 4 ~"At or below 400% of poverty",
      iprcat == 5 ~"Between 138% - 400% of poverty"
    ))

DATA2_2016 <- data_2016%>%
  select(year, Age, Race, Sex, Income, NUI, NIC, PCTUI, state_name)%>%
  rename(Year = year, Uninsured = NUI, Insured = NIC, Percent_Uninsured= PCTUI, State = state_name)%>%
  filter(Income == "All income levels" & Age == "Under 65" & Sex == "Both Sexes")%>%
  subset(Race!="All Races")

## 2015
data_2015 <- sahie_2015%>%
  select(year, agecat, racecat, sexcat, iprcat, NIPR, NUI, NIC, PCTUI, PCTIC, PCTELIG, PCTLIIC, state_name, county_name)%>%
  mutate(
    Age = case_when(
      agecat == 0 ~ "Under 65",
      agecat == 1 ~ "18-64",
      agecat == 2 ~ "40-64",
      agecat == 3 ~ "50-64",
      agecat == 4 ~ "Under 19",
      agecat == 5 ~ "21-64"),
    Race = case_when(
      racecat == 0 ~ "All Races",
      racecat == 1 ~"White",
      racecat == 2 ~ "Black",
      racecat == 3 ~"Hispanic"),
    Sex = case_when(
      sexcat == 0 ~"Both Sexes",
      sexcat == 1 ~"Male",
      sexcat == 2 ~"Female"
    ),
    Income = case_when(
      iprcat == 0 ~"All income levels",
      iprcat == 1 ~"At or below 200% of poverty",
      iprcat == 2 ~"At or below 250% of poverty",
      iprcat == 3 ~"At or below 138% of poverty",
      iprcat == 4 ~"At or below 400% of poverty",
      iprcat == 5 ~"Between 138% - 400% of poverty"
    ))

DATA2_2015 <- data_2015%>%
  select(year, Age, Race, Sex, Income, NUI, NIC, PCTUI, state_name)%>%
  rename(Year = year, Uninsured = NUI, Insured = NIC, Percent_Uninsured = PCTUI, State = state_name)%>%
  filter(Income == "All income levels" & Age == "Under 65" & Sex == "Both Sexes")%>%
  subset(Race!="All Races")

## 2014
data_2014 <- sahie_2014%>%
  select(year, agecat, racecat, sexcat, iprcat, NIPR, NUI, NIC, PCTUI, PCTIC, PCTELIG, PCTLIIC, state_name, county_name)%>%
  mutate(
    Age = case_when(
      agecat == 0 ~ "Under 65",
      agecat == 1 ~ "18-64",
      agecat == 2 ~ "40-64",
      agecat == 3 ~ "50-64",
      agecat == 4 ~ "Under 19",
      agecat == 5 ~ "21-64"),
    Race = case_when(
      racecat == 0 ~ "All Races",
      racecat == 1 ~"White",
      racecat == 2 ~ "Black",
      racecat == 3 ~"Hispanic"),
    Sex = case_when(
      sexcat == 0 ~"Both Sexes",
      sexcat == 1 ~"Male",
      sexcat == 2 ~"Female"
    ),
    Income = case_when(
      iprcat == 0 ~"All income levels",
      iprcat == 1 ~"At or below 200% of poverty",
      iprcat == 2 ~"At or below 250% of poverty",
      iprcat == 3 ~"At or below 138% of poverty",
      iprcat == 4 ~"At or below 400% of poverty",
      iprcat == 5 ~"Between 138% - 400% of poverty"
    ))

DATA2_2014 <- data_2014%>%
  select(year, Age, Race, Sex, Income, NUI, NIC, PCTUI, state_name)%>%
  rename(Year = year, Uninsured = NUI, Insured = NIC, Percent_Uninsured= PCTUI, State = state_name)%>%
  filter(Income == "All income levels" & Age == "Under 65" & Sex == "Both Sexes")%>%
  subset(Race!="All Races")

## 2013
data_2013 <- sahie_2013%>%
  select(year, agecat, racecat, sexcat, iprcat, NIPR, NUI, NIC, PCTUI, PCTIC, PCTELIG, PCTLIIC, state_name, county_name)%>%
  mutate(
    Age = case_when(
      agecat == 0 ~ "Under 65",
      agecat == 1 ~ "18-64",
      agecat == 2 ~ "40-64",
      agecat == 3 ~ "50-64",
      agecat == 4 ~ "Under 19",
      agecat == 5 ~ "21-64"),
    Race = case_when(
      racecat == 0 ~ "All Races",
      racecat == 1 ~"White",
      racecat == 2 ~ "Black",
      racecat == 3 ~"Hispanic"),
    Sex = case_when(
      sexcat == 0 ~"Both Sexes",
      sexcat == 1 ~"Male",
      sexcat == 2 ~"Female"
    ),
    Income = case_when(
      iprcat == 0 ~"All income levels",
      iprcat == 1 ~"At or below 200% of poverty",
      iprcat == 2 ~"At or below 250% of poverty",
      iprcat == 3 ~"At or below 138% of poverty",
      iprcat == 4 ~"At or below 400% of poverty",
      iprcat == 5 ~"Between 138% - 400% of poverty"
    ))

DATA2_2013 <- data_2013%>%
  select(year, Age, Race, Sex, Income, NUI, NIC, PCTUI, state_name)%>%
  rename(Year = year, Uninsured = NUI, Insured = NIC,Percent_Uninsured= PCTUI, State = state_name)%>%
  filter(Income == "All income levels" & Age == "Under 65" & Sex == "Both Sexes")%>%
  subset(Race!="All Races")

## 2012
data_2012 <- sahie_2012%>%
  select(year, agecat, racecat, sexcat, iprcat, NIPR, NUI, NIC, PCTUI, PCTIC, PCTELIG, PCTLIIC, state_name, county_name)%>%
  mutate(
    Age = case_when(
      agecat == 0 ~ "Under 65",
      agecat == 1 ~ "18-64",
      agecat == 2 ~ "40-64",
      agecat == 3 ~ "50-64",
      agecat == 4 ~ "Under 19",
      agecat == 5 ~ "21-64"),
    Race = case_when(
      racecat == 0 ~ "All Races",
      racecat == 1 ~"White",
      racecat == 2 ~ "Black",
      racecat == 3 ~"Hispanic"),
    Sex = case_when(
      sexcat == 0 ~"Both Sexes",
      sexcat == 1 ~"Male",
      sexcat == 2 ~"Female"
    ),
    Income = case_when(
      iprcat == 0 ~"All income levels",
      iprcat == 1 ~"At or below 200% of poverty",
      iprcat == 2 ~"At or below 250% of poverty",
      iprcat == 3 ~"At or below 138% of poverty",
      iprcat == 4 ~"At or below 400% of poverty",
      iprcat == 5 ~"Between 138% - 400% of poverty"
    ))

DATA2_2012 <- data_2012%>%
  select(year, Age, Race, Sex, Income, NUI, NIC, PCTUI, state_name)%>%
  rename(Year = year, Uninsured = NUI, Insured = NIC, Percent_Uninsured= PCTUI, State = state_name)%>%
  filter(Income == "All income levels" & Age == "Under 65" & Sex == "Both Sexes")%>%
  subset(Race!="All Races")


totalDATA <- rbind(DATA2_2018, DATA2_2019, DATA2_2017, DATA2_2016, DATA2_2015, DATA2_2014, DATA2_2013, DATA2_2012)%>%
  mutate(Percent_Uninsured = as.numeric(Percent_Uninsured))
tmp <- totalDATA %>%
  filter(Year == 2018, State == "Alabama")


# For widgets: 
race_choices <- unique(totalDATA$Race)
state_choices <- unique(totalDATA$State)


###### ui ######
ui <- navbarPage(
  
  ### TIME SERIES
  tabPanel(
    title = "Time Series",
    sidebarLayout(
      sidebarPanel(
        
       # race input
        selectInput(
          inputId = "race",
          label = "Choose a race to plot:",
          choices = race_choices),

        # state input
        checkboxGroupInput(inputId = "state",
                           label = "Include States:",
                           choices = state_choices,
                           selected = "Alabama",
                           inline = TRUE)
        
      ),
      mainPanel(plotOutput(outputId = "time"))
    )
  )
)

##### server #####
server  <- function(input, output){
  
  output$time <- renderPlot({
    data <- totalDATA %>% 
      filter(Race %in% input$race, State %in% input$state)
    # group =1
    ggplot(data = data, aes_string(x="Year", y= "Percent_Uninsured"))+ 
      geom_point(aes(color = State)) +
      geom_line(aes(group = State, color = State))
  }
  )
}
shinyApp(ui = ui, server = server)  

