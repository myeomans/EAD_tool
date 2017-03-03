
makePostData <- function(df_post=NULL, df_survey=NULL,settings = NULL){
  if(is.null(df_post) | is.null(df_survey)){
    return(NULL)
  } else {
    if(!is.null(df_survey$TriPart)){
      df_survey$TriPart <- factor(df_survey$TriPart, levels = c("Liberal", "Moderate","Conservative"))
    }
    
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
