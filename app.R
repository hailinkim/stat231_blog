library(readr)
library(shiny)
rating_words <- read_csv("data/rating_words.csv")
ui <- fluidPage(
  
  # App title ----
  titlePanel("Wordcloud"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      # Input: Animation with custom interval (in ms) ----
      # to control speed, plus looping
      sliderInput("animation", "Looping Animation:",
                  min = 2016, max = 2021,
                  value = 2016, step = 1,
                  animate =
                    animationOptions(interval = 300, loop = TRUE))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
    )
  )
)