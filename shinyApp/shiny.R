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
      menuItem("Intoduction", tabName = "instructions" ),
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
  output$upvotes_plot <- renderPlotFromCsv(input, "upvotes", makeUpvoteFacetGraph)
  output$posts_plot <- renderPlotFromCsv(input, "posts", makeCommentFacetGraph)
  output$activity_plot <- renderPlotFromCsv(input, "users", makeActivityIdeologyGraph)
  output$distribution_plot <- renderPlotFromCsv(input, "users", makeIdeologyGraph)
  output$descriptive_table <-renderTableFromCsv(input, "users","enrol","personcourse", demoTable)
  
  # Needs to be lower-dimensional
  output$heat_plot <- renderPlotFromCsv(input, "posts", plotHeatMap)
  # What is this about??
  output$expected_actual_plot <- renderPlotFromCsvTwoInputs(input, "users","posts", plotExpectedVsActualPosts)
}
shinyApp(ui, server)

