
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
    
    df_survey <- merge( df_survey, df_count_post, by = "user_id", all.x=TRUE,)
    df_survey <- merge( df_survey, df_count_replies, by = "user_id", all.x=TRUE)
    df_survey <- merge( df_survey, df_count_comments, by = "user_id", all.x=TRUE)
    
    df_survey[ is.na(df_survey$count_post) , "count_post"] <- 0L
    df_survey[ is.na(df_survey$count_replies) , "count_replies"] <- 0L
    df_survey[ is.na(df_survey$count_comments) , "count_comments"] <- 0L
    
  }
  
  # add components from df_upvote
  
  if(!is.null(df_upvote)){
    df_count_upvotes <- makeCounts(df_upvote,"user_id", "count_upvotes")
    
    df_survey <- merge( df_survey, df_count_upvotes, by = "user_id", all.x=TRUE)
    df_survey[ is.na(df_survey$count_upvotes) , "count_upvotes"] <- 0L
  }
  
  return(df_survey)
}