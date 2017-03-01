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
  return(df_count)
}

makeSurveyData <- function(df_survey = NULL, df_post = NULL, df_upvote=NULL, settings = NULL){
  if(is.null(df_survey)){
    return(NULL)
  }
  
  # fix TriPart levels
  if( !is.null(df_survey$TriPart )){
    df_survey$TriPart <- factor(df_survey$TriPart, levels = c("Liberal", "Moderate","Conservative"))
  }
  
  # add components from df_post
  if(!is.null(df_post)){
    df_count_post <- makeCounts(df_post,"user_id", "count_post")
    df_count_replies <- makeCounts(df_post[df_post$level==2,],"user_id", "count_replies")
    df_count_comments <- makeCounts(df_post[df_post$level>2,],"user_id", "count_comments")
    
    df_survey <- merge( df_survey, df_count_post, by = "user_id", all.x=TRUE)
    df_survey <- merge( df_survey, df_count_replies, by = "user_id", all.x=TRUE)
    df_survey <- merge( df_survey, df_count_comments, by = "user_id", all.x=TRUE)
  }
  
  # add components from df_upvote
  if(!is.null(df_upvote)){
    df_count_upvotes <- makeCounts(df_upvote,"user_id", "count_upvotes")
    
    df_survey <- merge( df_survey, df_count_upvotes, by = "user_id", all.x=TRUE)
  }
  
  return(df_survey)
}

makePostData <- function(df_post=NULL, df_survey=NULL,settings = NULL){
  if(is.null(df_post) | is.null(df_survey)){
    return(NULL)
  } else {
    df_survey_parent <- df_survey
    names(df_survey_parent) <- paste0("parent_", names(df_survey))
    df_post <- merge(df_post, df_survey, by = "user_id", all.x = TRUE)
    df_post <- merge(df_post, df_survey_parent, by = "parent_user_id", all.x = TRUE)
    
    if(settings$course.only){
      course.leads<-strsplit(settings$admin.names,",")[[1]]
      course.threads<-df_post[(df_post$user_id%in%course.leads)&(df_post$level==1),"post_id"]
      df_post<-df_post[df_post$thread_id%in%course.threads,]
    }
    if(settings$usa.only){df_post<-df_post[(df_post$USA==1)&!is.na(df_post$USA),]}
    if(!settings$self.posts){
      df_post<-df_post[df_post$parent_user_id!=df_post$user_id,]
    }
    
  }
  return(df_post)
}

makeUpvoteData <- function(df_upvote=NULL, df_survey=NULL, df_post=NULL, settings = NULL){
  if(is.null(df_upvote) | is.null(df_survey)| is.null(df_post)){
    return(NULL)
  } else {
    df_survey_parent <- df_survey
    names(df_survey_parent) <- paste0("parent_", names(df_survey))
    
    df_upvote <- merge(df_upvote, df_survey, by = "user_id", all.x = TRUE)
    df_upvote <- merge(df_upvote, df_survey_parent, by = "parent_user_id", all.x = TRUE)
    
    df_upvote<-df_upvote[df_upvote$post_id%in%df_post$post_id,]
  }
  
  return(df_upvote)
}
###############################################################################################
