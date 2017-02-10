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

dfFromCsv <- function(input, s_file_name){
  l_in_file <- input[[s_file_name]]
  if (is.null(l_in_file)) {
    return(NULL) 
  } else {
    df_data <- read.csv(l_in_file$datapath, stringsAsFactors = F)
    return(df_data)
  }
}

renderTableFromCsv <- function(input, s_file_name_1, s_file_name_2, s_file_name_3, tableFunction, ...){
  shiny::renderText({
    df_data_1 <- dfFromCsv( input, s_file_name_1 )
    df_data_2 <- dfFromCsv( input, s_file_name_2 )
    df_data_3 <- dfFromCsv( input, s_file_name_3 )
    ifelse(is.null(df_data_1), NULL, tableFunction(df_data_1,df_data_2,df_data_3, ...))
  })
}

renderPlotFromCsvTwoInputs <- function(input, s_file_name_1, s_file_name_2, plotFunction, ...){
  shiny::renderPlot({
    df_data_1 <- dfFromCsv( input, s_file_name_1 )
    df_data_2 <- dfFromCsv( input, s_file_name_2 )
    ifelse((is.null(df_data_1) | is.null(df_data_2)), NULL, plotFunction(df_data_1, df_data_2, ...))
  })
}
