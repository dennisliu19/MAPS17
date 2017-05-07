library(shiny)
library(shinydashboard)
library(ggplot2)

function(input, output,session) {
  ### BY GROUP TAB
  plot_data <- reactive({
    tangram[tangram$GroupName %in% input$bygroupid,]
  })
  
  # For storing which rows have been excluded
  vals <- reactiveValues(
    keeprows = c()
  )
  observe({
    vals$keeprows <- rep(TRUE, nrow(plot_data()))
  })
  
  output$bygroup_plot1 <- renderPlot({
    keep    <- plot_data()[vals$keeprows,  ]
    exclude <- plot_data()[!vals$keeprows, ]
    
    if (input$color_factor != "None") {
      ggplot(keep, aes_string(x = input$X, y = input$Y)) +
        geom_point(aes_string(color = input$color_factor), size = 4) +
        geom_point(data = exclude, size = 4, shape = 21, fill = NA, color = "black", alpha = 0.25)
    } else {
      ggplot(keep, aes_string(x = input$X, y = input$Y)) + geom_point(size = 4) +
        geom_point(data = exclude, shape = 21, size = 4, fill = NA, color = "black", alpha = 0.25)
    }
  })

  output$toggled_table <- renderDataTable(plot_data()[!vals$keeprows,c(1:20)],
                                          options = list(
                                            pageLength = 5,
                                            scrollX = TRUE))
                                            
  # Toggle points that are clicked
  observeEvent(input$bygroup_plot1_click, {
    res <- nearPoints(plot_data(), input$bygroup_plot1_click, allRows = TRUE)
    isolate({vals$keeprows <- xor(vals$keeprows, res$selected_)})

  })
  
  # Toggle points that are brushed, when button is clicked
  observeEvent(input$exclude_toggle, {
    res <- brushedPoints(plot_data(), input$bygroup_plot1_brush, allRows = TRUE)
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Reset all points
  observeEvent(input$exclude_reset, {
    vals$keeprows <- rep(TRUE, nrow(mtcars))
  })
  
  
  # Download cleaned data
  output$cleaned_data = downloadHandler('tangram-filtered.csv', content = function(file) {
    keep    <- plot_data()[vals$keeprows,  ]
    write.csv(keep, file)
  })
  
  ### BY FACTOR TAB
  tangram$transformation <- tangram$TimeUsed
  
  observe({
    if(input$factor == "level_gender"){
      tangram1 <- tangram[tangram$level_gender == "F" | tangram$level_gender == "M",]
      groupchoice <- c(unique(tangram1$GroupName),"all")
    }
    if(input$factor == "level_stem"){
      tangram2 <- tangram[tangram$level_stem == "Y" | tangram$level_stem == "N",]
      groupchoice <- c(unique(tangram2$GroupName),"all")
    }   
    if(input$factor == "level_athl"){
      tangram3 <- tangram[tangram$level_athl == "Y" | tangram$level_athl == "N",]
      groupchoice <- c(unique(tangram3$GroupName),"all")
    }
    updateSelectizeInput(session,'byfactorid',"GroupName",choices = groupchoice,selected = NULL)
  })
  
  ttestdata <- reactive({
    if ( "log" == input$transformation){
      tangram$transformation <- log(tangram$transformation+1)
    }
    if ( "exp" == input$transformation){
      tangram$transformation <- (tangram$transformation)^2
    }
    if ( "sqrt" == input$transformation){
      tangram$transformation <- sqrt(tangram$transformation)
    }
    if(input$factor == "level_gender"){
      tangram <- tangram[tangram$level_gender == "F" | tae) {
      summary(aov(transformation ~ Factor,data = ttestdata()))
    } else {
      #ttestdata()$aovfac <- input$factor
      #ttestdata()$aovadd <- input$additional_factor
    summary(aov(transformation~Factor,data = ttestdata()))
    }
    })
}
