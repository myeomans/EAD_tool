
table_row_build<-function(data, course.start.date){
  data$profile_age<-(as.numeric(substr(course.start.date, 0, 4))-data$profile_year_of_birth)
  row.values<-list(certified=mean(data$certificate_status=="downloadable", na.rm=T),
                   female=mean(users_x$profile_gender=="f", na.rm=T),
                   bachelors=mean(users_x$profile_level_of_education%in%c("b","a","m","p","p_oth","p_se"), na.rm=T),
                   USA=mean(users_x$profile_country=="US", na.rm=T),
                   age_m=median(data$profile_age, na.rm=T),
                   age_sd=sd(data$profile_age, na.rm=T),
                   teacher=mean(data$teach=="Yes",na.rm=T),
                   intent=mean(grepl("enough",data$reason),na.rm=T),
                   students=nrow(data))
  return(row.values)
}

post_process<-function(posts_data,course.start.date){
  posts_data$level<-3
  posts_data[(posts_data$parent_ids==""),"level"]<-2
  posts_data[(posts_data$X_type=="CommentThread"),"level"]<-1
  posts_data$thread_id<-posts_data$comment_thread_id
  posts_data[posts_data$level==1,]$thread_id<-posts_data[posts_data$level==1,"mongoid"]
  posts_data$user_id<-posts_data$author_id
  posts_data$post_count<-posts_data$comment_count
  posts_data$username<-posts_data$author_username
  posts_data$abuse<-1*(posts_data$abuse_flaggers!="[]")
  posts_data$span<-floor(as.numeric(difftime(posts_data$created_at,paste0(course.start.date,"12:00 GMT"), units="days")))
  posts_data$word_count<-qdap::word_count(posts_data$body,missing=0)
  posts_data$shortyes<-1*(posts_data$word_count<4)
}

upvote_process<-function(forum_data){
  forum_data$mongoid<-forum_data$thread_id
  forum_data$upvote_username<-forum_data$username
  forum_data$personpost<-paste0(forum_data$username,forum_data$mongoid)
  upvotes_data<-data.frame(forum_data[grepl("upvote",forum_data$forum_action),])
  un_x<-data.frame(forum_data[grepl("unvote",forum_data$forum_action),])
  for (un in 1:nrow(un_x)){
    upvotes_data<-upvotes_data[!((upvotes_data$personpost==un_x[un,]$personpost)&(!duplicated(upvotes_data$personpost))),]
  }
  upvotes_data<-upvotes_data[!duplicated(upvotes_data$personpost),]
  upvotes_data<-upvotes_data[(upvotes_data$poster_username!=upvotes_data$upvote_username)&(!is.na(upvotes_data$poster_username)),]
  upvotes_data$username<-upvotes_data$target_user_id<-upvotes_data$personpost<-NULL
  
  return(upvotes_data)
}