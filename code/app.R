library(readr)
library(shiny)
library(dplyr)
library(cluster)
library(Rtsne)
library(ggplot2)
library(ggforce)
library(plotly)

tmp3 <- read_csv("data/demographic_sex_orientation.csv") %>% 
  mutate(across(where(is.character), as.factor))
gower_df3 <- daisy(tmp3, metric = "gower")
silhouette3 <- c()
silhouette3 = c(silhouette, NA)
for(i in 2:10){
  pam_clusters3 = pam(as.matrix(gower_df3),
                      diss = TRUE,
                      k = i)
  silhouette3 = c(silhouette3, pam_clusters3$silinfo$avg.width)
}
pam_nhis3 = pam(gower_df3, diss = TRUE, k = 10)
tsne_object3 <- Rtsne(gower_df3, is_distance = TRUE, perplexity = 29)

tsne_df3 <- tsne_object3$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_nhis3$clustering))
cluster3 <- tmp3 %>%
  ungroup() %>% 
  mutate(cluster = factor(pam_nhis3$clustering))
tsne <- cluster3 %>% 
  inner_join(tsne_df3, by = "cluster")

ui <- fluidPage(
  
  # App title ----
  titlePanel("Clustering"),
  
  plotlyOutput("clustering")
  # uiOutput("hover_info")
)

server <- function(input, output) {
  
  output$clustering <- renderPlotly({
    my_pal <- RColorBrewer::brewer.pal(n=10, name = "Set3")
    g <- ggplot(data = tsne, aes(x = X, y = Y, 
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