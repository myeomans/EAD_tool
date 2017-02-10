##################################################################################
#  
#                       Engagement Across Differences Tool
#
# Mike Yeomans, Alejandro Kantor, Brandon Stewart, Justin Reich, Dustin Tingley
#
##################################################################################
##################################################################################
library(shiny)
library(shinydashboard)
library(ggplot2)
library(reshape2)
library(htmlTable)
library(data.table)
options(shiny.maxRequestSize=30*1024^2) 
##########################################################################
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
loadOrSourceFiles(path = "functions/", load = FALSE)
##########################################################################
ui <- dashboardPage(
  dashboardHeader(title = "EAD Toolkit"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "instructions" ),
      menuItem("Users", tabName = "users"),
      menuItem("Forum Activity", tabName = "forum")
    )
  ),
  dashboardBody(
    tabItems(
      introTab(),
      userTab(),
      forumTab()
    )
  )
)
##########################################################################
server <- function(input, output) {
  output$upvotes_plot <-shiny::renderPlot({
    makeUpvoteFacetGraph(dfFromCsv(input, "upvotes"),input)
  })
  
  output$posts_plot <-shiny::renderPlot({
    makeCommentFacetGraph(dfFromCsv(input, "posts"),input)
  })

  output$activity_plot <-shiny::renderPlot({
    makeActivityIdeologyGraph(dfFromCsv(input, "survey"),input)
  })
  
  output$distribution_plot <-shiny::renderPlot({
    makeIdeologyGraph(dfFromCsv(input, "survey"),input)
  })
  
  output$descriptive_table <-shiny::renderText({
    demoTable(dfFromCsv(input, "survey"),
              dfFromCsv(input, "enrol"),
              dfFromCsv(input, "personcourse"), 
              input)
  })
  
  # Needs to be lower-dimensional... e.g. Tripartite
  output$heat_plot <-shiny::renderPlot({
    renderPlotFromCsv(dfFromCsv(input, "posts"),input)
  })

  # What is the point of this?
  output$expected_actual_plot <- renderPlotFromCsvTwoInputs(input, "survey","posts", plotExpectedVsActualPosts)
}

shinyApp(ui, server)
