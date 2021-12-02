library(readr)
library(shiny)
library(dplyr)
library(wordcloud)

ratings <- read_csv("ratings.csv", show_col_types = FALSE)
# for selectInput
type_choices <- unique(ratings$rating_type)

shinyApp(
  ui <- fluidPage(
    
    # App title ----
    titlePanel("Wordcloud"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      # Sidebar to demonstrate various slider options ----
      sidebarPanel(
        
        # Input: Animation with custom interval (in ms) ----
        # to control speed, plus looping
        sliderInput("animation", "Year:",
                    min = 2016, max = 2022,
                    value = 2016, step = 1,
                    animate = animationOptions(interval = 500, loop = TRUE)),
        
        selectizeInput(inputId = "type",
                       label = "Choose a measure type",
                       choices = type_choices,
                       selected = c("Prevention", "Treatment", "Customer Satisfaction"),
                       multiple = TRUE),
        
      ),
      
      # Main panel for displaying outputs ----
      mainPanel(
        plotOutput("wordcloud", width = "100%", height = 650)
      )
    )
  ),
  
  server <- function(input, output) {
    
    output$wordcloud <- renderPlot({
      
      dat <- ratings %>%
        filter(year %in% input$animation, rating_type %in% input$type) 
      
      my_pal <- RColorBrewer::brewer.pal(n = 3, name = "Dark2")
      
      set.seed(1120)
      dat %>%
        with(wordcloud(words = sentences, freq = mean, scale = c(2,0.8), 
                       colors = my_pal[as.factor(rating_type)], ordered.colors = TRUE))
      #need to adjust position
      legend(0, 1, legend = levels(factor(ratings$rating_type)),
             text.col=brewer.pal(3, "Dark2")[unique(factor(ratings$rating_type))])
    })
  },
  options = list(height = 600)
)
shinyApp(ui = ui, server = server)