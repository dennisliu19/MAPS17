
library(shiny)
library(shinydashboard)
library(data.table)

ui <- dashboardPage(
  dashboardHeader(title = "Tangram Data"),
  dashboardSidebar(
    sidebarMenu(
      convertMenuItem(menuItem("By Group",tabName = "bygroup",icon= icon("dashboard"),
               selectizeInput('bygroupid', "GroupName", choices = groupname ,groupname[2], multiple = TRUE),
               selectInput('Y', 'Choose Y: ', choices = responsechoice),
               selectInput('X', 'Choose X: ', choices = x_choices),
               selectInput('color_factor', 'Colored By: ', choices = color_choices)
      ),tabName = "bygroup"),
      convertMenuItem(menuItem("By Factor", tabName = "byfactor", icon = icon("bar-chart-o"),
               selectizeInput('factor', "Factor", choices = factorchoice , multiple = FALSE),
               selectizeInput('byfactorid', "GroupName", choices = groupname ,selected = NULL, multiple = TRUE),
               selectInput('response',"Response Variable", choices = responsechoice),
               selectizeInput('transformation',"Data Transformation", choices = transformation, selected = "none",multiple=FALSE)
      )
    ,tabName = "byfactor")
  )
  )
  ,
  dashboardBody(
    tabItems(
      tabItem(tabName = "bygroup",
              fluidRow(
                column(width = 12,
                       box(width = NULL, 
                           status = "primary",
                           title = "Data Exploration",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                plotOutput("bygroup_plot1",
                           click = "bygroup_plot1_click",
                           brush = brushOpts(id = "bygroup_plot1_brush")),
                actionButton("exclude_toggle", "Toggle points"),
                actionButton("exclude_reset", "Reset"),
                downloadButton('cleaned_data', 'Download Cleaned Data')),
                box(width = NULL,
                    status = "primary",
                    title = "Chosen Observations",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    dataTableOutput("toggled_table"))
                )
              )),
      
      tabItem(tabName = "byfactor",
    # Boxes need to be put in a row (or column)
    fluidRow(
      column(width=6,
      box(width=NULL,
          status = "primary",
          title = "T-test",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("plot1", height = 280 ),
          height = 350),
      box(
        width=NULL,
        title = "T-Test Statistics",
        status="primary",
        solidHeader = TRUE,
        collapsible = TRUE
        ,textOutput("text1")
      )
    ),
    column(width=6,
           box(width=NULL,
               title = "ANOVA", 
               status = "primary", 
               solidHeader = TRUE,
               collapsible = TRUE,
               selectInput("additional_factor", "Choose one additional factor: ",
                           additional_factors,
                           selected = "None"),
               plotOutput("plot2",height = 200),
               height = 350),
           box(width=NULL,
               status = "primary",
               title = "ANOVA Statistics",
               solidHeader = TRUE,
               collapsible = TRUE,
               textOutput("text2"))
        )
       )
      )
    )
  )
)

