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
    df_data <- read.csv(l_in_file$datapath, stringsAsFactors = F)
    return(df_data)
  }
}

###############################################################################################
###############################################################################################
data.checker<-function(posts_x, forum_x, enrol_x, survey_x, of.interest="leftright",
                       USA.only=TRUE, self.posts=FALSE, course.threads.only=TRUE,
                       course.name="Course", start.date=NULL, course.team.names=NULL){
  ###############################################################################################
  # Cleaning (dull, slow, offloaded)
  ###############################################################################################
  # User Population

  users_x<-users_process(survey_x, enrol_x, start.date)
  # Forum Structure
  posts_x<-post_process(posts_x, start.date)
  upvotes_x<-upvote_process(forum_x)
  ###############################################################################################
  # Distribution of Interest Scores
  ###############################################################################################
  
  posts_x<-merge(posts_x,users_x[users_x$included==1,c("user_id","leftright")],by="user_id", all.x=T)
  ###############################################################################################
  # Merging User Data Into Forum Activity
  ###############################################################################################
  posts_x[,c("toppost_leftright","parent_leftright")]<-NA
  for (x in 1:nrow(posts_x)){
    try(posts_x[x,]$toppost_leftright<-posts_x[posts_x$mongoid==posts_x[x,"comment_thread_id"],]$leftright,silent=T)
    if(posts_x[x,]$level==3){
      try(posts_x[x,]$parent_leftright<-posts_x[posts_x$mongoid==posts_x[x,"parent_ids"],]$leftright,silent=T)
    }
  }
  ###############################################################################################
  #upvotes_x<-merge(upvotes_x,posts_x[,c("mongoid","thread_id","username")],by="mongoid", all.x=T)
  #names(upvotes_x)[names(upvotes_x)=="username"]<-"poster_username"
  upvotes_x[,c("poster_username","thread_id","poster_leftright","upvote_leftright")]<-NA
  for (uv in 1:nrow(upvotes_x)){
    try(upvotes_x[uv,]$thread_id<-posts_x[posts_x$mongoid==upvotes_x[uv,]$mongoid,"thread_id"],silent=T)
    try(upvotes_x[uv,]$poster_username<-posts_x[posts_x$mongoid==upvotes_x[uv,]$mongoid,"username"], silent=T)
    try(upvotes_x[uv,]$poster_leftright<-as.numeric(unlist(users_x[users_x$username==upvotes_x[uv,]$poster_username,"leftright"]),silent=T))
    try(upvotes_x[uv,]$upvote_leftright<-as.numeric(unlist(users_x[users_x$username==upvotes_x[uv,]$upvote_username,"leftright"]),silent=T))
  }
  upvotes_x$course.post<-1*(upvotes_x$thread_id%in%posts_x[(posts_x$course.post==1)&(posts_x$level==1),"thread_id"])
  ###############################################################################################
  return(list(users=users_x,
              posts=posts_x,
              upvotes=upvotes_x))
}
###############################################################################################
###############################################################################################
users_process<-function(user.data, enrol.data, start.date){
  if((sum(colnames(user.data)[1:10]==paste0("V",1:10))==10)){
    colnames(user.data)[1:10]<-as.character(unlist(user.data[1,1:10]))
    user.data<-user.data[2:nrow(user.data),]
  }
  ###############################################################################################
  user.data$id_map_hash_id<-user.data$user_id
  user.data$user_id<-NULL
  user.data<-merge(user.data,
                   enrol.data[,c("id_map_hash_id","user_id","username","certificate_status",
                                 paste0("profile_",c("gender","country","year_of_birth","level_of_education")))],
                   by="id_map_hash_id", all.x=F)
  user.data<-user.data[!duplicated(user.data$user_id),]
  ###############################################################################################
  user.data$USA<-1*((user.data$profile_country=="US"))
  user.data$certified<-1*(user.data$certificate_status=="downloadable")
  user.data$profile_bachelors<-1*(user.data$profile_level_of_education%in%c("b","a","m","p","p_oth","p_se"))
  user.data$profile_age<-as.numeric(substr(start.date, 0, 4))-user.data$profile_year_of_birth
  user.data$teacher<-1*(user.data$teach=="Yes")
  user.data$intent<-1*(grepl("enough",user.data$reason))
  user.data$wvs.survey<-as.numeric(substr(user.data$wvsurvey_1,0,2))
  ###############################################################################################
  return(user.data)
}
###############################################################################################
###############################################################################################
post_process<-function(posts_data,start.date){
  ###############################################################################################
  posts_data$level<-3
  posts_data[(posts_data$parent_ids==""),"level"]<-2
  posts_data[(posts_data$X_type=="CommentThread"),"level"]<-1
  posts_data$thread_id<-posts_data$comment_thread_id
  posts_data[posts_data$level==1,]$thread_id<-posts_data[posts_data$level==1,"mongoid"]
  ###############################################################################################
  posts_data$user_id<-posts_data$author_id
  posts_data$post_count<-posts_data$comment_count
  posts_data$username<-posts_data$author_username
  posts_data$abuse<-1*(posts_data$abuse_flaggers!="[]")
  posts_data$span<-floor(as.numeric(difftime(posts_data$created_at,paste0(start.date,"12:00 GMT"), units="days")))
  posts_data$word_count<-qdap::word_count(posts_data$body,missing=0)
  posts_data$shortyes<-1*(posts_data$word_count<4)
  ###############################################################################################
  posts_data[,c("toppost_name","parent_name")]<-""
  posts_data[,c("toppost_span","parent_span","comment_order")]<-NA
  for (x in 1:nrow(posts_data)){
    try(posts_data[x,]$toppost_leftright<-posts_data[posts_data$mongoid==posts_data[x,"comment_thread_id"],]$leftright,silent=T)
    try(posts_data[x,]$toppost_name<-posts_data[posts_data$mongoid==posts_data[x,"comment_thread_id"],]$author_username,silent=T)
    try(posts_data[x,]$toppost_span<-posts_data[posts_data$mongoid==posts_data[x,"comment_thread_id"],]$span,silent=T)
    if(posts_data[x,]$level==3){
      try(posts_data[x,]$parent_leftright<-posts_data[posts_data$mongoid==posts_data[x,"parent_ids"],]$leftright,silent=T)
      try(posts_data[x,]$parent_name<-posts_data[posts_data$mongoid==posts_data[x,"parent_ids"],]$author_username,silent=T)
      try(posts_data[x,]$parent_span<-posts_data[posts_data$mongoid==posts_data[x,"parent_ids"],]$span,silent=T)
      try(posts_data[x,]$comment_order<-which(rev(posts_data[posts_data$parent_ids==posts_data[x,]$parent_ids,"mongoid"])==posts_data[x,]$mongoid), silent=T)
    }
  }
  posts_data$course.post<-1*(posts_data$toppost_name%in%course.team.names)
  posts_data$self.reply<-1*((posts_data$parent_name==posts_data$author_username)|(posts_data$toppost_name==posts_data$author_username))
  posts_data$admin<-posts_data$issue<-0
  ###############################################################################################
  return(posts_data)
}
###############################################################################################
###############################################################################################
upvotes_process<-function(forum_data){
  forum_data$mongoid<-forum_data$thread_id
  forum_data$upvote_username<-forum_data$username
  forum_data$personpost<-paste0(forum_data$username,forum_data$mongoid)
  ###############################################################################################
  upvotes_data<-data.frame(forum_data[grepl("upvote",forum_data$forum_action),])
  un_x<-data.frame(forum_data[grepl("unvote",forum_data$forum_action),])
  for (un in 1:nrow(un_x)){
    upvotes_data<-upvotes_data[!((upvotes_data$personpost==un_x[un,]$personpost)&(!duplicated(upvotes_data$personpost))),]
  }
  ###############################################################################################
  upvotes_data<-upvotes_data[!duplicated(upvotes_data$personpost),]
  upvotes_data<-upvotes_data[(upvotes_data$poster_username!=upvotes_data$upvote_username)&(!is.na(upvotes_data$poster_username)),]
  ###############################################################################################
  upvotes_data$username<-upvotes_data$target_user_id<-upvotes_data$personpost<-NULL
  upvotes_data$admin<-upvotes_data$issue<-0
  ###############################################################################################
  return(upvotes_data)
}
###############################################################################################
###############################################################################################

############################################################
enrol_process<-function(enrol.data,pc.data, start.date){
  enrol.enter<-merge(enrol.data,pc.data[,c("user_id","viewed")],by="user_id", all.x=T)
  enrol.enter<-enrol.enter[(enrol.enter$viewed=="True"),]
  enrol.enter$USA<-1*((enrol.enter$profile_country=="US"))
  enrol.enter$certified<-1*(enrol.enter$certificate_status=="downloadable")
  enrol.enter$profile_bachelors<-1*(enrol.enter$profile_level_of_education%in%c("b","a","m","p","p_oth","p_se"))
  enrol.enter$profile_age<-as.numeric(substr(start.date, 0, 4))-enrol.enter$profile_year_of_birth
  return(enrol.enter)
}
############################################################