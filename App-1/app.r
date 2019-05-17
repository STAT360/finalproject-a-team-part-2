library(shiny)
library(dplyr)
library(plotly)
candidates <- read.csv(file="president_primary_polls.csv", header=TRUE, sep=",")
vec<-candidates%>%
  filter(party=="DEM")%>%
  select(poll_id)%>%
  distinct()

#if (interactive()) {
  ui <- fluidPage(
  
    #numericInput("PollId", "Poll Id:", 57975, min = 56000, max = 59000),
    #verbatimTextOutput("value"),
    selectInput("var",
                label="Choose a polling ID: ",
                choices = vec
                
                
                ),
    #selectInput("id",
                #label="Choose a polling ID: ",
                #choices = 
      
    #),
    
    titlePanel("Democratic Primary Candidate Frontrunner Data"),
      mainPanel(plotOutput("distPlot"),
                textOutput("selected_var")
                )
      #add some user interaction
    
  )
  server <- function(input, output) {
    #candidates <- read.csv(file="president_primary_polls.csv", header=TRUE, sep=",")
    #only include relevant candidates
    candidates <- filter(candidates, pct > 5)
    candidates <- filter(candidates, party == "DEM")
    #output$value <- renderPrint({ 
        #candidates[candidates$poll_id == {input$PollId}, "pollster"][1]
      #})
    output$selected_var <- renderText({
      paste("This poll comes from: ", candidates[candidates$poll_id == {input$var},"pollster"][1] )
      
    })
    output$distPlot <- renderPlot({
      candidates <- filter(candidates, poll_id == { input$var })
      #change y value to percentage instead of count
      #ggplot(data = candidates)+geom_bar(mapping = aes(x = answer), fill = "blue")
      ggplot(data=candidates, aes(x=answer, y=pct)) +
        geom_bar(stat="identity",fill="blue")
    })
    
  }
  #if(interactive()){
    shinyApp(ui = ui, server = server)
  #}
#}