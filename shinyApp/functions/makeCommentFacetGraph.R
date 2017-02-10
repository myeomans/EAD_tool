##################################################################
# Comment facet graph
# Original Author: Mike Yeomans
# Modified by: Alejandro Kantor

##################################################################
# Groupings

#table(posts_gse$TriPart,posts_gse$parent_TriPart)
#table(posts_hks$TriPart,posts_hks$parent_TriPart)
##################################################################

makeCommentFacetGraph <- function(user.data, post.data, settings){  
  ##################################################################
  exclusions<-(post.data$TriPart!="")&(post.data$parent_TriPart!="")&(post.data$level==3)
  exclusions<-exclusions&(!is.na(post.data$TriPart))&(!is.na(post.data$parent_TriPart))
  if(settings$course.only) exclusions<-exclusions&(post.data$course.post==1)
  if(!settings$self.posts)  exclusions<-exclusions&(post.data$self.reply==0)
  if(settings$usa.only){   
    exclusions<-exclusions&(post.data$user_id%in%user.data[user.data$USA==1,"user_id"])
    exclusions<-exclusions&(post.data$parent_name%in%user.data[user.data$USA==1,"username"])
  }
  #if(settings$short.yes)   exclusions<-exclusions&(post.data$shortyes==0)

  #table(post.data$TriPart,post.data$parent_TriPart,useNA="ifany")
  post.data$TriPart<-factor(post.data$TriPart,levels=c("Liberal","Moderate","Conservative"))
  post.data$parent_TriPart<-factor(post.data$parent_TriPart,levels=c("Liberal","Moderate","Conservative"))
  post.data<-post.data[exclusions,]
  
  # post.data<-merge(post.data,user.data[,c("user_id",settings$of.interest)],by="user_id", all.x=T)
  # posts_x[,c("toppost_leftright","parent_leftright")]<-NA
  # for (x in 1:nrow(posts_x)){
  #   try(posts_x[x,]$toppost_leftright<-posts_x[posts_x$mongoid==posts_x[x,"comment_thread_id"],]$leftright,silent=T)
  #   if(posts_x[x,]$level==3){
  #     try(posts_x[x,]$parent_leftright<-posts_x[posts_x$mongoid==posts_x[x,"parent_ids"],]$leftright,silent=T)
  #   }
  # }
  ##################################################################
  plot.top <- max(table(post.data$TriPart,post.data$parent_TriPart))
  
  g_plot <- qplot(TriPart, data=post.data, geom="bar", fill=TriPart) +
    facet_wrap(~ parent_TriPart, strip.position = "bottom") + 
    ylim(0,plot.top) + scale_fill_brewer( palette = "Set1")+
    labs(x="Original Poster Ideology",y="Post-Comment Pairs", fill = "Commenter Ideology") +
    scale_x_discrete(breaks=NULL) +
    #scale_fill_discrete(name="Commenter Ideology") +
    getTheme()
  
  return(g_plot)
}
