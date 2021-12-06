library(readr)
library(shiny)
library(dplyr)
library(wordcloud)
library(extrafont)

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
                    min = 2016, max = 2021,
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
        plotOutput("wordcloud", width = "100%", height = "415px")
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
        with(wordcloud(words = sentences, freq = mean, scale = c(1.2, 0.57), 
                       colors = my_pal[as.factor(rating_type)], ordered.colors = TRUE),
             family = "Lato Hairline")
      #need to adjust position
      legend(0.8, 0.8, title = "Type of Metrics",
             legend = levels(factor(ratings$rating_type)),
             fill = brewer.pal(3, "Dark2")[unique(factor(ratings$rating_type))], cex = 0.6)
    }
    )
  },
  options = list(height = 600)
)
shinyApp(ui = ui, server = server)