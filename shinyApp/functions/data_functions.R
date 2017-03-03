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

makeCounts <- function(df_data, s_col_to_count, s_count_name){
  df_count <- as.data.frame(table(df_data[[s_col_to_count]]) , stringsAsFactors = FALSE )
  names(df_count) <- c(s_col_to_count, s_count_name)
  if( is.integer(df_data[[s_col_to_count]])   ){
    df_count[[s_col_to_count]] <- as.integer(df_count[[s_col_to_count]])
  } else if (is.numeric(df_data[[s_col_to_count]]) ){
    df_count[[s_col_to_count]] <- as.numeric(df_count[[s_col_to_count]])
  }
  if(sum(is.na(df_count[[s_col_to_count]]))>0){
    stop(paste0("NAs " ,"in", "s_count_name"))
  }
  return(df_count)
}
###############################################################################################
