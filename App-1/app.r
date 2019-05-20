library(shiny)
library(dplyr)
library(plotly)
library(RColorBrewer)
library(base)
candidates <- read.csv(file = "president_primary_polls.csv", header = TRUE, sep = ",")
winners <- candidates %>%
  filter(party == "DEM") %>%
  group_by(question_id) %>%
  filter(pct == max(pct)) %>%
  ungroup() %>%
  select(answer) %>%
  distinct()

ui <- fluidPage(
  titlePanel("Democratic Primary Candidate Data"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("method",
        "Choose the method of poll: ",
        choiceNames = c("Online", "Live Phone", "IVR/Online", "Online/IVR", "Unspecified"),
        choiceValues = c("Online", "Live Phone", "IVR/Online", "Online/IVR", "")
      ),
      selectInput("pster",
        label = "Choose a pollster: ",
        choices = NULL
      ),
      selectInput("var",
        label = "Choose a question ID: ",
        choices = NULL
      ),
      numericInput("percent", label = "Choose the lowest percent to display: ", 5, min = 0, max = 100)
    ),
    mainPanel(
      plotOutput("distPlot"),
      textOutput("selected_var2"),
      textOutput("selected_var3"),
      uiOutput(outputId = "my_ui")
    )
  )
)
server <- function(input, output, session) {
  observeEvent(input$method, {
    updateSelectInput(session, "pster",
      choices = candidates[candidates$methodology %in% {
        input$method
      }, "pollster"]
    )
  })
  observeEvent(input$pster, {
    updateSelectInput(session, "var",
      choices = candidates[candidates$methodology %in% {
        input$method
      } & candidates$pollster == {
        input$pster
      }, "question_id"]
    )
  })
  candidates <- filter(candidates, party == "DEM")
  output$selected_var2 <- renderText({
    paste("Time of poll: ", candidates[candidates$question_id == {
      input$var
    }, "start_date"][1], "to", candidates[candidates$question_id == {
      input$var
    }, "end_date"][1])
  })
  output$selected_var3 <- renderText({
    win <- candidates[candidates$question_id == {
      input$var
    }, ]
    win <- win[which.max(win$pct), ]
    paste("Projected Winner based on this poll:", win$candidate_name)
  })
  output$distPlot <- renderPlot({
    candidates <- filter(candidates, question_id == {
      input$var
    })
    candidates <- filter(candidates, pct >= {
      input$percent
    })
    ggplot(data = candidates, aes(x = answer, y = pct, fill = pct)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      ggtitle("Democratic Primary Poll Results") +
      xlab("Candidate Name") +
      ylab("Percent") +
      scale_fill_gradient(low = "lightblue", high = "darkblue")
  })
  output$my_ui <- renderUI({
    win <- candidates[candidates$question_id == {
      input$var
    }, ]
    win <- win[which.max(win$pct), ]
    if (isTRUE(win$answer == "Biden")) {
      img(src = "Joe_Biden.jpg", height = "300px")
    }
    else if (isTRUE(win$answer == "Sanders")) {
      img(src = "Bernie_Sanders.jpg", height = "300px")
    }
    else if (isTRUE(win$answer == "Warren")) {
      img(src = "Elizabeth_Warren.jpeg", height = "300px")
    }
    else if (isTRUE(win$answer == "O'Rourke")) {
      img(src = "Beto_ORourke.jpg", height = "300px")
    }
    else if (isTRUE(win$answer == "Harris")) {
      img(src = "Kamala_Harris.jpg", height = "300px")
    }
    else if (isTRUE(win$answer == "Obama")) {
      img(src = "Michelle_Obama.jpg", height = "300px")
    } else {
      img(src = "Unknown_portrait.jpg", height = "300px")
    }
  })
}
shinyApp(ui = ui, server = server)
