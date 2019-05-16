library(shiny)
library(dplyr)
library(plotly)
if (interactive()) {
  ui <- fluidPage(
    numericInput("PollId", "Poll Id:", 57975, min = 56000, max = 59000),
    verbatimTextOutput("value"),
    
    titlePanel("Democratic Primary Candidate Frontrunner Data"),
      mainPanel(plotOutput("distPlot"))
      #add some user interaction
    
  )
  server <- function(input, output) {
    candidates <- read.csv(file="president_primary_polls.csv", header=TRUE, sep=",")
    #only include relevant candidates
    candidates <- filter(candidates, pct > 10)
    candidates <- filter(candidates, party == "DEM")
    output$value <- renderPrint({ 
        candidates[candidates$poll_id == {input$PollId}, "pollster"][1]
      })
    output$distPlot <- renderPlot({
      candidates <- filter(candidates, poll_id == { input$PollId })
      #change y value to percentage instead of count
      #ggplot(data = candidates)+geom_bar(mapping = aes(x = answer), fill = "blue")
      ggplot(data=candidates, aes(x=answer, y=pct)) +
        geom_bar(stat="identity")
    })
    
  }
  if(interactive()){
    shinyApp(ui = ui, server = server)
  }
}