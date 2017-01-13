##########################################################################################
# DESCRIPTIVES
##########################################################################################
table_build<-list()
table_row_build<-function(data){
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
TriPartite<-function(ideology, bins=list(Conservative=6:10,
                                         Moderate=4:5,
                                         Liberal=1:3)){
  labels<-rep(NA,length(ideology))
  for(bb in 1:length(bins)){
    labels[ideology%in%bins[[bb]]]<-names(bins)[bb]
  }
  return(labels)
}
##########################################################################################
# Enrolled
##########################################################################################
enrol_x<-merge(enrol_x,pc_x[,c("user_id","viewed")],by="user_id")
enrol_x<-enrol_x[(enrol_x$viewed=="True"),]

table_build[["enrolled"]]<-table_row_build(enrol_x)
##########################################################################################
# Survey Takers
##########################################################################################
users_x$id_map_hash_id<-users_x$user_id
users_x<-merge(users_x,
               enrol_x[,c("id_map_hash_id","user_id","username","certificate_status",
                          paste0("profile_",c("gender","country","year_of_birth","level_of_education")))],
               by="id_map_hash_id", all.x=F)
# From Survey
table_build[["surveyed"]]<-table_row_build(users_x)
#############################################
# Ideology Response
#############################################
users_x$leftright<-as.numeric(substr(users_x$wvsurvey_1,0,2))
users_x<-users_x[!is.na(users_x$leftright),]
users_x<-users_x[!duplicated(users_x$user_id),]
table_build[["ideology"]]<-table_row_build(users_x)
#############################################
# Country-Specific Ideology
#############################################
if(course.USA.only){
  users_x<-users_x[(users_x$profile_country=="US"),]
  table_build[["ideology_USA"]]<-table_row_build(users_x)
}
###############################################################################################
# Post-level data
###############################################################################################
posts_x$level<-3
posts_x[(posts_x$parent_ids==""),"level"]<-2
posts_x[(posts_x$X_type=="CommentThread"),"level"]<-1
##############################################
posts_x$abuse<-1*(posts_x$abuse_flaggers!="[]")
posts_x$span<-floor(as.numeric(difftime(posts_x$created_at,paste0(course.start.date,"12:00 GMT"), units="days")))
posts_x$word_count<-qdap::word_count(posts_x$body,missing=0)
posts_x$shortyes<-1*(posts_x$word_count<4)
##############################################
posts_x$thread_id<-posts_x$comment_thread_id
posts_x[posts_x$X_type=="CommentThread",]$thread_id<-posts_x[posts_x$X_type=="CommentThread","mongoid"]
posts_x$user_id<-posts_x$author_id
posts_x$post_count<-posts_x$comment_count
posts_x$username<-posts_x$author_username
###############################################################################################
# Ideology Categories
###############################################################################################
tpb<-txtProgressBar(0,nrow(posts_x))
posts_x[,c("leftright")]<-NA
for (x in 1:nrow(posts_x)){
  MATCH<-(users_x$user_id==posts_x[x,]$user_id)
  if(sum(MATCH)>0){
    try(posts_x[x,]$leftright<-users_x[MATCH,"leftright"],silent=T)
  }
  setTxtProgressBar(tpb, value=x)
}
#posts_x<-merge(posts_x,users_x[,c("user_id","leftright")],by="user_id", all.x=T)
###############################################################################################
# Forum Structure Labelling
###############################################################################################
posts_x$toppost_name<-""
posts_x$toppost_span<-NA
posts_x$toppost_leftright<-NA
posts_x$parent_name<-""
posts_x$parent_span<-NA
posts_x$parent_leftright<-NA
posts_x$comment_order<-NA

tpb<-txtProgressBar(0,nrow(posts_x))
for (x in 1:nrow(posts_x)){
  try(posts_x[x,]$toppost_leftright<-posts_x[posts_x$mongoid==posts_x[x,"comment_thread_id"],]$leftright,silent=T)
  try(posts_x[x,]$toppost_name<-posts_x[posts_x$mongoid==posts_x[x,"comment_thread_id"],]$author_username,silent=T)
  try(posts_x[x,]$toppost_span<-posts_x[posts_x$mongoid==posts_x[x,"comment_thread_id"],]$span,silent=T)
  if(posts_x[x,]$level==3){
    try(posts_x[x,]$parent_leftright<-posts_x[posts_x$mongoid==posts_x[x,"parent_ids"],]$leftright,silent=T)
    try(posts_x[x,]$parent_name<-posts_x[posts_x$mongoid==posts_x[x,"parent_ids"],]$author_username,silent=T)
    try(posts_x[x,]$parent_span<-posts_x[posts_x$mongoid==posts_x[x,"parent_ids"],]$span,silent=T)
    try(posts_x[x,]$comment_order<-which(rev(posts_x[posts_x$parent_ids==posts_x[x,]$parent_ids,"mongoid"])==posts_x[x,]$mongoid), silent=T)
    setTxtProgressBar(tpb, value=x)
  }
}
###############################################################################################
# Upvotes!
###############################################################################################
forum.dat$mongoid<-forum.dat$thread_id
forum.dat$upvote_username<-forum.dat$username
forum.dat$personpost<-paste0(forum.dat$username,forum.dat$mongoid)
upvotes_x<-data.frame(forum.dat[grepl("upvote",forum.dat$forum_action),])
un_x<-data.frame(forum.dat[grepl("unvote",forum.dat$forum_action),])
for (un in 1:nrow(un_x)){
  upvotes_x<-upvotes_x[!((upvotes_x$personpost==un_x[un,]$personpost)&(!duplicated(upvotes_x$personpost))),]
}
upvotes_x<-upvotes_x[!duplicated(upvotes_x$personpost),]
upvotes_x$username<-upvotes_x$target_user_id<-upvotes_x$personpost<-NULL
upvotes_x[,c("poster_username","poster_leftright","upvote_leftright","upvote_user_id")]<-NA
for (uv in 1:nrow(upvotes_x)){
  try(upvotes_x[uv,]$thread_id<-posts_x[posts_x$mongoid==upvotes_x[uv,]$mongoid,"thread_id"],silent=T)
  try(upvotes_x[uv,]$poster_username<-posts_x[posts_x$mongoid==upvotes_x[uv,]$mongoid,"username"], silent=T)
  try(upvotes_x[uv,]$poster_leftright<-unlist(users_x[users_x$username==upvotes_x[uv,]$poster_username,"leftright"]),silent=T)
  try(upvotes_x[uv,]$upvote_leftright<-unlist(users_x[users_x$username==upvotes_x[uv,]$upvote_username,"leftright"]),silent=T)
}
upvotes_x$poster_leftright<-as.numeric(upvotes_x$poster_leftright)
upvotes_x$upvote_leftright<-as.numeric(upvotes_x$upvote_leftright)

# upvote selfies
upvotes_x<-upvotes_x[(upvotes_x$poster_username!=upvotes_x$upvote_username)&(!is.na(upvotes_x$poster_username)),]

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
# upvote selfies were just deleted...

posts_x$admin<-posts_x$issue<-0
upvotes_x$admin<-upvotes_x$issue<-0

FOCUS_x<-(posts_x$course.post==1)&(posts_x$self.reply==0)
FOCup_x<-(upvotes_x$course.post==1)
###############################################################################################
# Activity Counts per person
###############################################################################################

# ERROR
# ERROR
# ERROR
# ERROR
# ERROR

users_x[,paste0(c("reply","comment","upvote"),"_count")]<-NA
for (x in 1:nrow(users_x)){
  users_x[x,"reply_count"]<-sum(FOCUS_x&(posts_x$user_id==users_x[x,"user_id"])&(posts_x$level==2))
  users_x[x,"comment_count"]<-sum(FOCUS_x&(posts_x$user_id==users_x[x,"user_id"])&(posts_x$level==3))
  users_x[x,"upvote_count"]<-sum(FOCup_x&(upvotes_x$upvote_username==users_x[x,"username"]),na.rm=T)
}
users_x$activities<-rowSums(users_x[,paste0(c("reply","comment","upvote"),"_count")])
table_build[["forum_active"]]<-table_row_build(users_x[users_x$activities>0,])
###############################################################################################
# Reply-Level Data
###############################################################################################
# Are replies necessary??

replies_x<-posts_x[(posts_x$level==2)&(posts_x$course.post==1)&(posts_x$admin==0),]

replies_x[,c(paste0("comment_",c("count","others","shortyes")),"upvote_count")]<-0
replies_x[,paste0("comment_",c("","others_","shortyes_","longs_"),"leftright")]<-NA
tpb<-txtProgressBar(0,nrow(replies_x))
for(r in 1:nrow(replies_x)){
  replies_x[r,"upvote_count"]<-sum((upvotes_x$mongoid==replies_x[x,"mongoid"]))
  comments<-posts_x[(posts_x$level==3)&(posts_x$parent_ids==replies_x[r,"mongoid"]),]
  if(nrow(comments)>0){
    not.author<-(comments$author_id!=replies_x[r,"author_id"])
    replies_x[r,"comment_count"]<-nrow(comments)
    replies_x[r,"comment_others"]<-sum(not.author)
    replies_x[r,"comment_shortyes"]<-sum(not.author&(comments$shortyes==1))
    replies_x[r,"comment_leftright"]<-mean(comments[,"leftright"],na.rm=T)
    replies_x[r,"comment_others_leftright"]<-mean(comments[not.author,"leftright"],na.rm=T)
    replies_x[r,"comment_shortyes_leftright"]<-mean(comments[not.author&(comments$shortyes==1),"leftright"],na.rm=T)
    replies_x[r,"comment_longs_leftright"]<-mean(comments[not.author&(comments$shortyes==0),"leftright"],na.rm=T)
    setTxtProgressBar(tpb, value=r)
  }
}
replies_x$comment_longs<-replies_x$comment_others-replies_x$comment_shortyes
##############################################################################################################################################################################################
# Tripartite
###############################################################################################
posts_x$TriPart<-TriPartite(posts_x$leftright)
posts_x$parent_TriPart<-TriPartite(posts_x$parent_leftright)
upvotes_x$poster_TriPart<-TriPartite(upvotes_x$poster_leftright)
upvotes_x$upvote_TriPart<-TriPartite(upvotes_x$upvote_leftright)
##############################################################################################################################################################################################
# Table 4 - Ideology distribution
round(100*(table(users_x$leftright)/nrow(users_x)),1) 
##############################################################################################################################################################################################
rm(pc_x,enrol_x,x,r,tpb,MATCH,code.name,not.author,comments,forum.dat,un_x,uv,un,FOCup_x,FOCUS_x)
###############################################################