##################################################################################
#  
#                       Engagement Across Differences Tool
#
# Mike Yeomans, Alejandro Kantor, Brandon Stewart, Justin Reich, Dustin Tingley
#
##################################################################################
##################################################################################
library(shiny)
library(dplyr)
library(shinydashboard)
library(ggplot2) 
library(reshape2)
library(htmlTable)
library(data.table)
library(shinyBS)
library(qdap)
library(quanteda)
library(tm)
library(Matrix)

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
      menuItem("Forum Activity", tabName = "forum"),
      menuItem("Text Analysis", tabName = "text")
    )
  ),
  dashboardBody(
    tabItems(
      introTab(),
      userTab(),
      forumTab(),
      textTab()
    )
  )
)
##########################################################################
server <- function(input, output) {
  # Get data sets
  getSurveyPre <- reactive({dfFromCsv(input, "survey")})
  getPostsPre <- reactive({dfFromCsv(input, "posts")})
  getUpvotesPre <- reactive({dfFromCsv(input, "upvotes")})
  getEnrol <- reactive({dfFromCsv(input, "enrol")})
  
  getSurvey <- reactive({
    makeSurveyData(getSurveyPre() , getPosts(), getUpvotes(), input)
  })
  getPosts <- reactive({
    makePostData(getPostsPre() , getSurveyPre(), input)
  })
  getUpvotes <- reactive({
    makeUpvoteData(getUpvotesPre() , getSurveyPre(), getPosts(), input)
  })
  
  output$upvotes_plot <-shiny::renderPlot({
    makeFacetGraph(getUpvotes(), s_data_origin = "upvote", input)
  })
  
  output$posts_plot <-shiny::renderPlot({
    makeFacetGraph(getPosts(), s_data_origin = "post", input)
  })
  
  output$posts_plot1 <-shiny::renderPlot({
    makeFacetGraph(getPosts(), s_data_origin = "post", input)
  })
  
  output$activity_plot <-shiny::renderPlot({
    makeActivityIdeologyGraph(getSurvey(),
                              input)
  })
  
  output$distribution_plot <-shiny::renderPlot({
    makeIdeologyGraph(getSurvey(),
                      input)
  })
  
  output$descriptive_table <-shiny::renderText({
    demoTable(getSurvey(),
              getEnrol(),
              input)
  })
  
  output$heat_post_plot <-shiny::renderPlot({
    plotHeatMap(getPosts(),
                s_data_origin = "post",
                input)
  })
  
  output$expected_actual_plot<-shiny::renderPlot({
    plotExpectedVsActualPosts(getSurvey(),
                              getPosts(),
                              input)
  })
  
  output$heat_upvote_plot<-shiny::renderPlot({
    plotHeatMap(getUpvotes(),
                s_data_origin = "upvote",
                input)    
  })
  
  output$word_plot <-shiny::renderPlot({
    makeWordGraph(getPosts(),input)
  })
  
  output$word_plot1 <-shiny::renderPlot({
    makeWordGraph(getPosts(),input)
  })
}

shinyApp(ui, server)
