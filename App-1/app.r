library(shiny)
library(dplyr)
library(plotly)
candidates <- read.csv(file="president_primary_polls.csv", header=TRUE, sep=",")
vec<-candidates%>%
  filter(party=="DEM")%>%
  select(question_id)%>%
  distinct()
vec2<-candidates%>%
  filter(party=="DEM")%>%
  select(pollster)%>%
  distinct()

#if (interactive()) {
  ui <- fluidPage(
  
    #numericInput("PollId", "Poll Id:", 57975, min = 56000, max = 59000),
    #verbatimTextOutput("value"),
    selectInput("pster",
                label="Choose a pollster",
                choices = vec2
      
    ),
    selectInput("var",
                label="Choose a question ID: ",
                choices = NULL
                
                
                ),
    #sliderInput("Select the lowest percent you want to include: ",
               # min = 0, max = 100,
      
    #),
    numericInput("percent",label="Choose the lowest percent to display: ",5, min = 0, max = 100),
    
    titlePanel("Democratic Primary Candidate Data"),
      mainPanel(plotOutput("distPlot"),
                textOutput("selected_var"),
                textOutput("selected_var2")
                )
      #add some user interaction
    
  )
  server <- function(input, output,session) {
    #candidates <- read.csv(file="president_primary_polls.csv", header=TRUE, sep=",")
    #only include relevant candidates
    #candidates <- filter(candidates, pct > 5)
    observeEvent(input$pster,{
      updateSelectInput(session,'var',
                        choices = candidates[candidates$pollster == {input$pster},"question_id"])
    })
    candidates <- filter(candidates, party == "DEM")
    #output$value <- renderPrint({ 
        #candidates[candidates$poll_id == {input$PollId}, "pollster"][1]
      #})
    output$selected_var <- renderText({
      paste("This poll comes from: ", candidates[candidates$question_id == {input$var},"pollster"][1] )
      
      
    })
    output$selected_var2 <- renderText({
      
      paste("Time of poll: ", candidates[candidates$question_id == {input$var},"start_date"][1] , "to",candidates[candidates$question_id == {input$var},"end_date"][1]  )
      
    })
    output$distPlot <- renderPlot({
      candidates <- filter(candidates, question_id == { input$var })
      candidates <- filter(candidates, pct >= {input$percent})
      #change y value to percentage instead of count
      #ggplot(data = candidates)+geom_bar(mapping = aes(x = answer), fill = "blue")
      ggplot(data=candidates, aes(x=answer, y=pct)) +
        geom_bar(stat="identity",fill="blue") +
        theme(axis.text.x=element_text(angle=60, hjust=1)) +
        xlab("Candidate Name") + 
        ylab("Percent")
    })
    
  }
  #if(interactive()){
    shinyApp(ui = ui, server = server)
  #}
#}