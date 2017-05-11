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
  
  output$toggled_table <- renderDataTable(plot_data()[!vals$keeprows,],
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
      tangram <- tangram[tangram$level_gender == "F" | tangram$level_gender == "M",]
      tangram$Factor <- tangram$level_gender
    }
    if(input$factor == "level_stem"){
      tangram <- tangram[tangram$level_stem == "Y" | tangram$level_stem == "N",]
      tangram$Factor <- tangram$level_stem
    }   
    if(input$factor == "level_athl"){
      tangram <- tangram[tangram$level_athl == "Y" | tangram$level_athl == "N",]
      tangram$Factor <- tangram$level_athl
    }
    tangram[tangram$GroupName %in% input$byfactorid,]
  })
  
  
  output$plot1 <- renderPlot({
    ggplot(ttestdata(),aes(Factor,transformation)) +
      geom_boxplot(varwidth = TRUE,fill = "white", colour = "#3366FF",
                   outlier.colour = "red", outlier.shape = 1)
  })
  output$text1 <- renderText({
    paste("P-value for",input$factor, "is", t.test(transformation ~ Factor,data=ttestdata())$p.value)
  })

  ### ANOVA
  
  output$plot2 <- renderPlot({
    if (input$additional_factor == "None") {
      ggplot(ttestdata(),aes(Factor,transformation)) +
        geom_boxplot(varwidth = TRUE,fill = "white", colour = "#3366FF",
                     outlier.colour = "red", outlier.shape = 1)
    } else {
      ggplot(ttestdata(),aes_string(x = input$additional_factor, y = "transformation")) +
        geom_boxplot(aes(fill=Factor),colour = "#3366FF",
                     outlier.colour = "red", outlier.shape = 1) 
    }
  })
  
  output$anovaSummary <- renderDataTable(
    if (input$additional_factor == "None") {
      return (isolate(anova(transformation ~ Factor, data = ttestdata())))
    } else {
      if (length(unique(ttestdata()[,input$additional_factor])) < 2) {
        return (isolate(data.table(variable = c(input$Factor),
                                   anova(lm(transformation ~ Factor, data = ttestdata())))))
      } else {
        return (isolate(data.table(variable = c(input$Factor,input$additional_factor),
                                   (anova(lm(transformation ~ Factor+ttestdata()[,input$additional_factor], data = ttestdata()))))))
      }
    }, options = list(scrollX = TRUE))
}