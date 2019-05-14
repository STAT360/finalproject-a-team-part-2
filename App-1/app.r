library(shiny)
library(dplyr)
library(plotly)
ui <- fluidPage(
  titlePanel("Democratic Primary Candidate Frontrunner Data"),
  
    mainPanel(plotOutput("distPlot"))
    #add some user interaction
)
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    candidates <- read.csv(file="president_primary_polls.csv", header=TRUE, sep=",")
    #only include relevant candidates
    candidates <- filter(candidates, pct > 10)
    candidates <- filter(candidates, party == "DEM")
    #change y value to percentage instead of count
    #ggplot(data = candidates)+geom_bar(mapping = aes(x = answer), fill = "blue")
    ggplot(data=candidates, aes(x=answer, y=pct)) +
      geom_bar(stat="pct")
  })
  
}

shinyApp(ui = ui, server = server)