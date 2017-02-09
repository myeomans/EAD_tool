
fileInputCsv <- function(s_obj_name , s_title){
  shiny::fileInput(s_obj_name, s_title,
                   accept = c(
                     'text/csv',
                     'text/comma-separated-values',
                     'text/plain',
                     '.csv'
                   )
  )
}
dashboardSideBarCsvInput <- function(){
  dashboardSidebar(
    sidebarMenu(
      menuItem("Instructions", tabName = "instructions" ),
      menuItem("Users", tabName = "users"),
      menuItem("Forum Activity", tabName = "forum")
    ),
    fileInputCsv( "users", 'Upload users data'),
    tags$hr(), 
    fileInputCsv('posts', 'Upload posts data'),
    tags$hr(),
    fileInputCsv('upvotes', 'Upload upvote data'),
    tags$hr()
  )
}



dfFromCsv <- function(input, s_file_name){
  l_in_file <- input[[s_file_name]]
  
  if (is.null(l_in_file)) {
    return(NULL) 
    
  } else {
    df_data <- read.csv(l_in_file$datapath, header = T, stringsAsFactors = F)
    return( df_data )
  }
}

renderPlotFromCsv <- function(input, s_file_name, plotFunction){
  renderPlot({
    df_data <- dfFromCsv( input, s_file_name )
    
    if (is.null(df_data)) {
      return(NULL) 
    } else {
      return( plotFunction(df_data))
    }
  })
}

renderPlotFromCsvTwoInputs <- function(input, s_file_name_1, s_file_name_2, plotFunction){
  renderPlot({
    df_data_1 <- dfFromCsv( input, s_file_name_1 )
    df_data_2 <- dfFromCsv( input, s_file_name_2 )
    if (is.null(df_data_1) | is.null(df_data_2))  {
      return(NULL) 
    } else {
      return( plotFunction(df_data_1, df_data_2))
    }
  })
}


exampleTable <- function(){
  output <- data.frame( course_enroll = c("5,409", "49.8%", "79.9%", "32 (14.2)", "", "", "14.3%") ,
                        survey_resp = c("2,009", "57.7%","83.3%","36 (14.5)","52.9%","57.2%","31.8%"),
                        usa_samp =c("1,340","61.0%","86.2%","38 (14.9)","57.7%","56.0%","32.7%"),
                        any_act= c("569","59.4%","88.1%","40 (15.1)","63.4%","75.4%","61.0%")
  )
  htmlTable(output,
            header = paste0("&nbsp;&nbsp;&nbsp;",c("Enrolled in Course", "Answered Survey", 
                       "From USA", "Active In Forum"),"&nbsp;&nbsp;&nbsp;"), 
            n.tspanner = c(1, 4 ,2), 
            rnames = c( "Observations",
                        "% Female",
                        "College Degree",
                        "Median Age (SD)",
                        "% Teachers",
                        "Intended  Course Completion",
                        "Actual Course Completion"
            ) ,
            tspanner = c("","","")
            #caption="Basic table with both column spanners (groups) and row groups",
  )
}