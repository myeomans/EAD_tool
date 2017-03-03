
makeUpvoteData <- function(df_upvote=NULL, df_survey=NULL, df_post=NULL, settings = NULL){
  if(is.null(df_upvote) | is.null(df_survey)| is.null(df_post)){
    return(NULL)
  } else {
    if(!is.null(df_survey$TriPart)){
      df_survey$TriPart <- factor(df_survey$TriPart, levels = c("Liberal", "Moderate","Conservative"))
    }
    
    df_survey_parent <- df_survey
    names(df_survey_parent) <- paste0("parent_", names(df_survey))
    
    df_upvote <- merge(df_upvote, df_survey, by = "user_id", all.x = TRUE)
    df_upvote <- merge(df_upvote, df_survey_parent, by = "parent_user_id", all.x = TRUE)
    
    df_upvote<-df_upvote[df_upvote$post_id%in%df_post$post_id,]
  }
  
  return(df_upvote)
}