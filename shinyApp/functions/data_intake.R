test_users<-read.csv("/Users/mikeyeomans/Desktop/users.csv",stringsAsFactors=F)


data.checker<-function(posts_x, forum_x, enrol_x, users_x, of.interest="wvs.survey",
                       USA.only=TRUE, self.posts=FALSE, course.threads.only=TRUE,
                       course.name="Course", start.date=NULL, course.team.names=NULL){
  ###############################################################################################
  if((sum(colnames(users_x)[1:10]==paste0("V",1:10))==10)){
    colnames(users_x)[1:10]<-as.character(unlist(users_x[1,1:10]))
    users_x<-users_x[2:nrow(users_x),]
  }
  # Survey Takers
  users_x$id_map_hash_id<-users_x$user_id
  users_x$user_id<-NULL
  users_x<-merge(users_x,
                 enrol_x[,c("id_map_hash_id","user_id","username","certificate_status",
                            paste0("profile_",c("gender","country","year_of_birth","level_of_education")))],
                 by="id_map_hash_id", all.x=F)
  users_x<-users_x[!duplicated(users_x$user_id),]
  
  # Ideology Score
  users_x$leftright<-ifelse(of.interest=="wvs.survey",
                            as.numeric(substr(users_x$wvsurvey_1,0,2)),
                            users_x[,of.interest])
  
  # Country-Specific
  users_x$USA<-1*((users_x$profile_country=="US"))
  
  users_x$included<-1*(!is.na(users_x$leftright))
  if(course.USA.only) users_x[users_x$USA!=1,"included"]<-FALSE
  
  ###############################################################################################
  # Forum Structure
  ###############################################################################################
  posts_x<-post_process(posts_x, start.date)
  upvotes_x<-upvote_process(forum_x)
  
  
  posts_x<-merge(posts_x,users_x[users_x$included==1,c("user_id","leftright")],by="user_id", all.x=T)
  posts_x[,c("toppost_leftright","parent_leftright")]<-NA
  for (x in 1:nrow(posts_x)){
    try(posts_x[x,]$toppost_leftright<-posts_x[posts_x$mongoid==posts_x[x,"comment_thread_id"],]$leftright,silent=T)
    if(posts_x[x,]$level==3){
      try(posts_x[x,]$parent_leftright<-posts_x[posts_x$mongoid==posts_x[x,"parent_ids"],]$leftright,silent=T)
    }
  }
  ###############################################################################################
  # Upvote Structure
  upvotes_x[,c("poster_username","upvote_user_id","poster_leftright","upvote_leftright")]<-NA
  for (uv in 1:nrow(upvotes_x)){
    try(upvotes_x[uv,]$thread_id<-posts_x[posts_x$mongoid==upvotes_x[uv,]$mongoid,"thread_id"],silent=T)
    try(upvotes_x[uv,]$poster_username<-posts_x[posts_x$mongoid==upvotes_x[uv,]$mongoid,"username"], silent=T)
    try(upvotes_x[uv,]$poster_leftright<-as.numeric(unlist(users_x[users_x$username==upvotes_x[uv,]$poster_username,"leftright"]),silent=T))
    try(upvotes_x[uv,]$upvote_leftright<-as.numeric(unlist(users_x[users_x$username==upvotes_x[uv,]$upvote_username,"leftright"]),silent=T))
  }
  upvotes_x$course.post<-1*(upvotes_x$thread_id%in%posts_x[(posts_x$course.post==1)&(posts_x$level==1),"thread_id"])
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
  ###############################################################################################
  return(list(users=users_x,
              posts=posts_x,
              upvotes=upvotes_x))
}
