
data.checker<-function(posts_x, forum_x, enrol_x, pc_x, users_x,
                       USA.only=TRUE, self.posts=FALSE, course.threads.only=TRUE,
                       course.name="Course", start.date=NULL, course.team.names=NULL){
  if((sum(colnames(users_x)[1:10]==paste0("V",1:10))==10)){
    colnames(users_x)[1:10]<-as.character(unlist(users_x[1,1:10]))
    users_x<-users_x[2:nrow(users_x),]
  }
  # Descriptives from Sub-Populations
  table_build<-list()
  
  # Enrolled
  enrol_x<-merge(enrol_x,pc_x[,c("user_id","viewed")],by="user_id")
  enrol_x<-enrol_x[(enrol_x$viewed=="True"),]
  table_build[["enrolled"]]<-table_row_build(enrol_x, start.date)
  
  # Survey Takers
  users_x$id_map_hash_id<-users_x$user_id
  users_x$user_id<-NULL
  users_x<-merge(users_x,
                 enrol_x[,c("id_map_hash_id","user_id","username","certificate_status",
                            paste0("profile_",c("gender","country","year_of_birth","level_of_education")))],
                 by="id_map_hash_id", all.x=F)
  table_build[["surveyed"]]<-table_row_build(users_x, start.date)
  
  # Ideology Response
  users_x$leftright<-as.numeric(substr(users_x$wvsurvey_1,0,2))
  users_x<-users_x[!is.na(users_x$leftright),]
  users_x<-users_x[!duplicated(users_x$user_id),]
  table_build[["ideology"]]<-table_row_build(users_x, start.date)
  
  # Country-Specific Ideology
  if(course.USA.only){
    users_x<-users_x[(users_x$profile_country=="US"),]
    table_build[["ideology_USA"]]<-table_row_build(users_x, start.date)
  }
  
  posts_x<-post_process(posts_x, start.date)
  posts_x<-merge(posts_x,users_x[,c("user_id","leftright")],by="user_id", all.x=T)
  
  ###############################################################################################
  # Forum Structure Labelling
  ###############################################################################################
  posts_x[,c("toppost_name","parent_name")]<-""
  posts_x[,c("toppost_span","toppost_leftright","parent_span","parent_leftright","comment_order")]<-NA
  
  for (x in 1:nrow(posts_x)){
    try(posts_x[x,]$toppost_leftright<-posts_x[posts_x$mongoid==posts_x[x,"comment_thread_id"],]$leftright,silent=T)
    try(posts_x[x,]$toppost_name<-posts_x[posts_x$mongoid==posts_x[x,"comment_thread_id"],]$author_username,silent=T)
    try(posts_x[x,]$toppost_span<-posts_x[posts_x$mongoid==posts_x[x,"comment_thread_id"],]$span,silent=T)
    if(posts_x[x,]$level==3){
      try(posts_x[x,]$parent_leftright<-posts_x[posts_x$mongoid==posts_x[x,"parent_ids"],]$leftright,silent=T)
      try(posts_x[x,]$parent_name<-posts_x[posts_x$mongoid==posts_x[x,"parent_ids"],]$author_username,silent=T)
      try(posts_x[x,]$parent_span<-posts_x[posts_x$mongoid==posts_x[x,"parent_ids"],]$span,silent=T)
      try(posts_x[x,]$comment_order<-which(rev(posts_x[posts_x$parent_ids==posts_x[x,]$parent_ids,"mongoid"])==posts_x[x,]$mongoid), silent=T)
    }
  }
  ###############################################################################################
  # Upvotes!
  ###############################################################################################
  upvotes_x<-upvote_process(forum_x)
  
  upvotes_x[,c("poster_username","poster_leftright","upvote_leftright","upvote_user_id")]<-NA
  for (uv in 1:nrow(upvotes_x)){
    try(upvotes_x[uv,]$thread_id<-posts_x[posts_x$mongoid==upvotes_x[uv,]$mongoid,"thread_id"],silent=T)
    try(upvotes_x[uv,]$poster_username<-posts_x[posts_x$mongoid==upvotes_x[uv,]$mongoid,"username"], silent=T)
    try(upvotes_x[uv,]$poster_leftright<-unlist(users_x[users_x$username==upvotes_x[uv,]$poster_username,"leftright"]),silent=T)
    try(upvotes_x[uv,]$upvote_leftright<-unlist(users_x[users_x$username==upvotes_x[uv,]$upvote_username,"leftright"]),silent=T)
  }
  upvotes_x$poster_leftright<-as.numeric(upvotes_x$poster_leftright)
  upvotes_x$upvote_leftright<-as.numeric(upvotes_x$upvote_leftright)
  
  ###############################################################################################
  # Thread-level data
  ###############################################################################################
  threads_x<-posts_x[posts_x$level==1,]
  threads_x[,paste0(c("reply","comment","upvote"),"_count")]<-NA
  for (x in 1:nrow(threads_x)){
    threads_x[x,"reply_count"]<-sum((posts_x$thread_id==threads_x[x,"mongoid"])&(posts_x$level==2))
    threads_x[x,"comment_count"]<-sum((posts_x$thread_id==threads_x[x,"mongoid"])&(posts_x$level==3))
    threads_x[x,"upvote_count"]<-sum((upvotes_x$thread_id==threads_x[x,"mongoid"]))
  }
  threads_x$activities<-rowSums(threads_x[,paste0(c("reply","comment","upvote"),"_count")])
  ###############################################################################################
  # Thread Types
  ###############################################################################################
  threads_x$course.post<-1*(threads_x$author_username%in%course.team.names)
  posts_x$course.post<-1*(posts_x$thread_id%in%threads_x[threads_x$course.post==1,"thread_id"])
  
  posts_x$self.reply<-1*((posts_x$parent_name==posts_x$author_username)|(posts_x$toppost_name==posts_x$author_username))
  posts_x$admin<-posts_x$issue<-0
  upvotes_x$admin<-upvotes_x$issue<-0
  
  ###############################################################################################
  # Activity Counts per person
  ###############################################################################################
  FOCUS_x<-ifelse(self.reply,(posts_x$course.post==1),(posts_x$course.post==1)&(posts_x$self.reply==0))
  FOCup_x<-(upvotes_x$course.post==1)
  
  users_x[,paste0(c("reply","comment","upvote"),"_count")]<-NA
  for (x in 1:nrow(users_x)){
    users_x[x,"reply_count"]<-sum(FOCUS_x&(posts_x$user_id==users_x[x,"user_id"])&(posts_x$level==2))
    users_x[x,"comment_count"]<-sum(FOCUS_x&(posts_x$user_id==users_x[x,"user_id"])&(posts_x$level==3))
    users_x[x,"upvote_count"]<-sum(FOCup_x&(upvotes_x$upvote_username==users_x[x,"username"]),na.rm=T)
  }
  users_x$activities<-rowSums(users_x[,paste0(c("reply","comment","upvote"),"_count")])
  table_build[["forum_active"]]<-table_row_build(users_x[users_x$activities>0,], start.date)
}
