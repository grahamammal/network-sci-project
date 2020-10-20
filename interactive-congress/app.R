#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(readr)
library(glue)
projected_coords <- read_csv("projected_coords_rankings.csv")



# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("cosmo"),
    sidebarLayout(
          sidebarPanel(
            sliderInput("congress",
                        "Congress:",
                        min = 1,
                        max = 116,
                        value = 116,
                        step = 1),
            radioButtons("centrality", "Centrality Measure:",
                         choices = c("Betweenness", "PageRank"),
                         selected = "Betweenness"),
          ),
    mainPanel(
      plotOutput("congress_plot", height = "400px")
    ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$congress_plot <- renderPlot({
        size_by <- tolower((input$centrality))
        
        projected_coords %>%
            mutate(party_name = factor(party_name, levels = c("Democrat",
                                                              "Republican",
                                                              "Other"))) %>% 
            filter(congress == input$congress) %>% 
            ggplot() +
            geom_point(aes(x = V1, 
                           y = V2, 
                           group = icpsr,
                           color = party_name,
                           size = !!sym(size_by))) +
            scale_color_manual(values = c("#5768AC", "#FA5A50", "#989898"), drop = FALSE) +
            labs(x = "First Singular Vector",
                 y = "Second Singular Vector", 
                 color = "Senator Party",
                 title = glue("Congress: {input$congress}, Start Year: {(input$congress-1)*2 + 1789}"),
                 size = input$centrality) +
          theme_minimal()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


