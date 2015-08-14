library(shiny)

shinyUI(fluidPage(
      titlePanel("Text Prediction"),
      
      sidebarLayout(
            sidebarPanel(
                  
                  textInput("typing", "Enter Text"),
                  
                  radioButtons("medium",
                               label = "Text Medium",
                               choices = list("News Article" = 1, "Blog Entry" = 2, "Twitter Feed" = 3)),

                  h5("Display text predictions based on the medium of your writing:"),
                  tags$ul(
                        tags$li("Are you a new reporter? I'll give you results generated from other news articles. No RTs here!"),
                        tags$li("Are you a blogger? You'll get suggestions based on the best blog entries."),
                        tags$li("Tweeting something out quick and fast? No time for proper grammar, have to get to your point!"),
                        tags$li("Do you do it all? You'll get the best prediction we can provide.")
                  ),
                  
                  textOutput("error"),
                  tags$head(tags$style("#error{color: red;
                                 font-size: 13px;
                                 font-style: italic;
                                 text-align: center;
                                 }"
                  )
                  )
            ),
            
            mainPanel(
                  h3("Suggestions"),
                  dataTableOutput("table1")             
            )
      )
))