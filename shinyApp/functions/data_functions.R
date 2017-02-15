###############################################################################################
# Alejandro's Shiny interfaces
###############################################################################################
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
    #import using fread since it is quicker
    dt_data <- fread(input = l_in_file$datapath)
    df_data <- data.frame(dt_data)
    return(df_data)
  }
}

###############################################################################################
