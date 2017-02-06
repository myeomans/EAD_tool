library(shiny)
library(shinydashboard)
library(ggplot2)
library(reshape2)
library(htmlTable)
loadOrSourceFiles <- function(path, pattern = NULL,ignore.case = FALSE, load = TRUE ){
  files <- list.files(path = path,pattern = pattern,ignore.case = ignore.case)
  for( file in files){
    file_full <- paste0(path,file)
    if(load == TRUE ) {
      load(file_full, envir = globalenv())
    } else {
      source(file_full)
    }
  }
}
loadOrSourceFiles(path = "../functions/", load = FALSE)

options(shiny.maxRequestSize=30*1024^2) 
ui <- dashboardPage(
  dashboardHeader(title = "Interaction Dashboard"),
  dashboardSideBarCsvInput(),
  dashboardBody(
    tabItems(
      tabItem(tabName = "instructions",
              h2("Instructions"),
              p("Upload your data with the following specifications:"),
              h3("Data checks")
              ),
      tabItem(tabName = "dashboard",
              h2("Dashboard"),
                # Boxes need to be put in a row (or column)
                fluidRow(
                  tabBox(
                    height = "600px",
                    tabPanel("Example Table", htmlOutput("descriptive_table")),
                    tabPanel("Activity in Threads", plotOutput("activity_plot" , height = 600))
                  ),
                  # Boxes need to be put in a row (or column)
                  tabBox(
                    height = "350px",
                     tabPanel( "Distribution of comments",
                               plotOutput("posts_plot", height = 280)),
                  tabPanel("Heat map", plotOutput("heat_plot", height = 280))
                ),
                tabBox(
                  height = "350px",
                  tabPanel( "Distribution of upvotes",
                            plotOutput("upvotes_plot", height = 280)),
                  tabPanel("Exp vs Actual", plotOutput("expected_actual_plot", height = 280))
                )
                 # 
                )
              )
    )
  )
)

server <- function(input, output) {
  # s_checks <- checkRep(input)
  
  output$upvotes_plot <- renderPlotFromCsv(input, "upvotes", makeUpvoteFacetGraph)
  output$posts_plot <- renderPlotFromCsv(input, "posts", makeCommentFacetGraph)
  output$activity_plot <- renderPlotFromCsv(input, "users", makeActivityIdeologyGraph)
  output$heat_plot <- renderPlotFromCsv(input, "posts", plotHeatMap)
  output$descriptive_table <- shiny::renderText({exampleTable()})
  output$expected_actual_plot <- renderPlotFromCsvTwoInputs(input, "users","posts" , plotExpectedVsActualPosts)
}

shinyApp(ui, server)
