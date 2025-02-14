# server.R

source("../Models.R")

if (!exists('grams')) {
  grams <- load_grams(master=1, mincount=4)
}

shinyServer(function(input, output) {
      #---------------------------------------------------
      #     Reactive data functions
      #     Only run when underlying dataset changes
      #---------------------------------------------------
      dataInput <- reactive({
            predict_word(input$typing)
      })

      output$table1 <- renderDataTable(
        dataInput(), options = list(paging = F, searching = F))

})



#       up <- reactive({
#             nrow(dataInput()[dataInput()[,1] < dataInput()[,4],])
#       })
#       
#       stats <- reactive ({
#             dt <- data.frame(nrow(dataInput()))
#             dt$up <- up()
#             dt$down <- nrow(dataInput()) - up()
#             dt$percent <- percent(up() / nrow(dataInput()))
#             
#             colnames(dt) <- c("Total Days", "Up Days", "Down Days", "Percent Up")
#             dt
#       })
#       
#       metrics <- reactive ({
#             dat <- if (input$stats == 1) # Open-Close
#                   dataInput()[,4] - dataInput()[,1]
#             else
#                   dataInput()[,2] - dataInput()[,3]
#             
#             df <- data.frame(round(mean(dat), 4))
#             df$median <- round(median(dat), 4)
#             df$min <- round(min(dat), 4)
#             df$max <- round(max(dat), 4)
#             df$sd <- round(sd(dat), 4)
#             
#             colnames(df) <- c("Mean", "Median", "Min", "Max", "Standard Deviation")
#             
#             df
#       })
#       

      #----------------------------------
      #     Modify output widgets
      #----------------------------------
#       output$error <- renderText({
#             if (input$ma > nrow(dataInput()))
#                   "Moving Average will not appear if Day parameter is greater than total days in graph."
#       })
#       
#       output$plot <- renderPlot({
#             chartSeries(dataInput(),
#                         theme = chartTheme("white"), 
#                         type = "candle",
#                         log.scale = F,
#                         TA = NULL,
#                         name = input$symb)
#             
#             if (input$ma <= nrow(dataInput()))
#                   addSMA(n = input$ma, col = "blue")
#       })
      




