post_process<-function(posts_data,course.start.date){
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
  posts_data$span<-floor(as.numeric(difftime(posts_data$created_at,paste0(course.start.date,"12:00 GMT"), units="days")))
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

upvote_process<-function(forum_data){
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