# load packages
library(readr)  
library(dplyr)
library(tidyverse)
library(maps)
library(ggplot2)
library(sf)
library(viridis)

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
