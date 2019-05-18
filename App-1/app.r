library(shiny)
library(dplyr)
library(plotly)
library(RColorBrewer)
library(base)
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
    checkboxGroupInput("method",
                       "Choose the method of poll: ",
                       choiceNames = c("Online", "Live Phone", "IVR/Online", "Online/IVR","Unspecified"),
                       choiceValues = c("Online","Live Phone","IVR/Online","Online/IVR","")
                       #choices = c("Online","Live Phone","IVR/Online","Online/IVR","")
                       ),
    selectInput("pster",
                label="Choose a pollster",
                choices = NULL
                  ),
    selectInput("var",
                label="Choose a question ID: ",
                choices = NULL
                
                
                ),
    numericInput("percent",label="Choose the lowest percent to display: ",5, min = 0, max = 100),
    
    titlePanel("Democratic Primary Candidate Data"),
      mainPanel(plotOutput("distPlot"),
                textOutput("selected_var2"),
                textOutput("selected_var3")
                )
      #add some user interaction
    
  )
  server <- function(input, output,session) {
    #candidates <- read.csv(file="president_primary_polls.csv", header=TRUE, sep=",")
    #only include relevant candidates
    #candidates <- filter(candidates, pct > 5)
    observeEvent(input$method,{
      updateSelectInput(session,'pster',
                        choices = candidates[candidates$methodology %in% {input$method},"pollster"] 
      )
    })
    observeEvent(input$pster,{
      updateSelectInput(session,'var',
                        choices = candidates[candidates$methodology %in% {input$method} & candidates$pollster == {input$pster} ,"question_id"]
                        )
    })
    candidates <- filter(candidates, party == "DEM")
    #output$value <- renderPrint({ 
        #candidates[candidates$poll_id == {input$PollId}, "pollster"][1]
      #})
    output$selected_var2 <- renderText({
      
      paste("Time of poll: ", candidates[candidates$question_id == {input$var},"start_date"][1] , "to",candidates[candidates$question_id == {input$var},"end_date"][1]  )
      
    })
    output$selected_var3 <- renderText({
      win<-candidates[candidates$question_id == {input$var},]
      win<-win[which.max(win$pct),]
      paste("Projected Winner based on this poll:", win$answer)
    })
    output$distPlot <- renderPlot({
      candidates <- filter(candidates, question_id == { input$var })
      candidates <- filter(candidates, pct >= {input$percent})
      #change y value to percentage instead of count
      #ggplot(data = candidates)+geom_bar(mapping = aes(x = answer), fill = "blue")
      ggplot(data=candidates, aes(x=answer, y=pct, fill = pct)) +
        geom_bar(stat="identity") +
        theme(axis.text.x=element_text(angle=60, hjust=1)) +
        xlab("Candidate Name") + 
        ylab("Percent") + 
        scale_fill_gradient(low= "lightblue", high = "darkblue")
      
        
    })
    
  }
  #if(interactive()){
    shinyApp(ui = ui, server = server)
  #}
#}