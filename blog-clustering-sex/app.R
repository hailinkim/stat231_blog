library(readr)
library(dplyr)
library(shiny)
library(plotly)

tsne_sex <- read_csv("tsne_sex.csv", show_col_types = FALSE) %>% 
  filter(coverage == "Not covered") %>% 
  mutate(across(where(is.character), as.factor),
         cluster = as.factor(cluster))

ui <- fluidPage(
  
  plotlyOutput("clustering")
)

server <- function(input, output) {
  
  output$clustering <- renderPlotly({
    my_pal <- RColorBrewer::brewer.pal(n = 8, name = "Set3")
    g <- ggplot(data = tsne_sex, aes(x = X, y = Y, 
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
      scale_color_manual(values = c(my_pal)) +
      scale_fill_manual(values = c(paste(my_pal, "66", sep = "")))
    
    ggplotly(g, tooltip = "text")
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)