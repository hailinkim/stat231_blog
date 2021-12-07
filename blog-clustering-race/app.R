library(readr)
library(dplyr)
library(shiny)
library(plotly)

tsne_race <- read_csv("tsne_race.csv", show_col_types = FALSE) %>% 
  #focus on the uninsured 
  filter(coverage == "Not covered") %>% 
  #convert some variables into factors 
    #because factor variables in the original data set aren't read as factors
  mutate(across(where(is.character), as.factor),
         cluster = as.factor(cluster))

ui <- fluidPage(
  
  plotlyOutput("clustering")
)

server <- function(input, output) {
  
  output$clustering <- renderPlotly({
    my_pal <- RColorBrewer::brewer.pal(n = 10, name = "Set3")
    g <- ggplot(data = tsne_race, aes(x = X, y = Y, 
                                      text = paste(
                                        "Coverage Status: ", coverage,
                                        "\nRace: ", race,
                                        "\nSexual Orientation: ", sex_orientation,
                                        "\nEducation level: ", edu,
                                        "\nCitizenship status: ", citizen
                                      )
    )) +
      geom_point(aes(color = cluster, fill = cluster), size = 4, shape = 21) + 
      theme_bw() +
      scale_color_manual(values=c(my_pal)) +
      scale_fill_manual(values=c(paste(my_pal, "66", sep = "")))
    
    ggplotly(g, tooltip = "text")
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)