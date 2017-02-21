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
  # Merge in relevant dimension
  TOP<-paste0("parent_",settings$of.interest)
  BOT<-settings$of.interest
  parent.user.data<-user.data
  parent.user.data[,paste0("parent_",c("user_id",BOT))]<-parent.user.data[,c("user_id",BOT)]
  post.data<-merge(post.data,user.data[,c("user_id",BOT)],by="user_id",all.x=T)
  post.data<-merge(post.data,parent.user.data[,c("parent_user_id",TOP)],by="parent_user_id",all.x=T)
  post.data$top<-post.data[,TOP]
  post.data$bot<-post.data[,BOT]
  ##################################################################
  # Filter posts
  exclusions<-(post.data$bot!="")&(post.data$top!="")&(post.data$level>2)
  exclusions<-exclusions&(!is.na(post.data$bot))&(!is.na(post.data$top))
  # if(settings$course.only) exclusions<-exclusions&(post.data$course.post==1)
  # if(!settings$self.posts)  exclusions<-exclusions&(post.data$self.reply==0)
  if(settings$usa.only){
    exclusions<-exclusions&(post.data$user_id%in%user.data[user.data$USA==1,"user_id"])
    exclusions<-exclusions&(post.data$parent_user_id%in%user.data[user.data$USA==1,"user_id"])
  }
  #if(settings$short.yes)   exclusions<-exclusions&(post.data$shortyes==0)

  post.data<-post.data[exclusions,]
  ##################################################################
  plot.top <- max(table(post.data$bot,post.data$top))
  
  g_plot <- qplot(bot, data=post.data, geom="bar", fill=bot) +
    facet_wrap(~ top, strip.position = "bottom") + 
    ylim(0,plot.top) + scale_fill_brewer( palette = "Set1")+
    labs(x="Original Poster Ideology",y="Post-Comment Pairs", fill = "Commenter Ideology") +
    scale_x_discrete(breaks=NULL) +
    #scale_fill_discrete(name="Commenter Ideology") +
    getTheme()
  
  return(g_plot)
}
