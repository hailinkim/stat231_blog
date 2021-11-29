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

 
totalDATA <- read_csv("totalDATA.csv")

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
 
