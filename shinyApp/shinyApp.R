library(shiny)
library(shinydashboard)
library(ggplot2)
library(reshape2)
library(htmlTable)
library(data.table)
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
  dashboardHeader(title = "EAD Toolkit"),
  dashboardSideBarCsvInput(),
  dashboardBody(
    tabItems(
      tabItem(tabName = "instructions",
              h2("Engagement Across Differences"),
              p(text.blocks$intro),
              textInput('course.name', 'Forum Name'),
              dateInput('start.date', 'Forum Launch Date'),
              h3("Filters for Analysis"),
              checkboxInput('usa.only', 'American Students Only', TRUE),
              checkboxInput('self.posts', 'Count Self-Replies', FALSE),
              h3("Focal Threads"),
              checkboxInput('course.only', 'Course-Generated Threads Only', TRUE),
              textInput('course.team.names', 'Course Team User Names')
              # radioButtons('sep', 'Separator',
              #              c(Comma=',',
              #                Semicolon=';',
              #                Tab='\t'),
              #              ','),
      ),
      tabItem(tabName = "users",
              h2("User Information"),
              fluidRow(
                box(
                  height = "300px",
                  width = 12,
                  tabPanel("Example Table", htmlOutput("descriptive_table"))
                )),
              fluidRow(
                box(
                  width = 6,
                  height = "300px",
                  tabPanel("Distribution of Interest", plotOutput("distribution_plot", height = 250))
                )
              )),
      tabItem(tabName = "forum",
              h2("Forum Activity"),
              # Boxes need to be put in a row (or column)
              fluidRow(
                box(
                  height = "600px",
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
  output$distribution_plot <- renderPlotFromCsv(input, "users", makeIdeologyGraph)
  output$heat_plot <- renderPlotFromCsv(input, "posts", plotHeatMap)
  output$descriptive_table <- shiny::renderText({exampleTable()})
  output$expected_actual_plot <- renderPlotFromCsvTwoInputs(input, "users","posts" , plotExpectedVsActualPosts)
}

shinyApp(ui, server)
