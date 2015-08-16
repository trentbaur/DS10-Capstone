library(shiny)

shinyUI(fluidPage(
    titlePanel("Text Prediction"),
      
    sidebarLayout(
        sidebarPanel(

            textInput("typing", "Enter Text"),
                      
            h5("Text is predicted based on the analysis of news articles, blog entries and twitter feeds."),
            
            textOutput("error"),
            tags$head(tags$style("#error{color: red;
                                 font-size: 13px;
                                 font-style: italic;
                                 text-align: center;
                                 }")
                )
        ),

        mainPanel(
            h3("Top Suggestion"),
            h6(textOutput("top_prediction")),

            h3("Alternative Suggestions"),
            dataTableOutput("table1")
        )
    )
))