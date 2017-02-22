##################################################################
# upvote facet graph
# Original Author: Mike Yeomans
# Modified by: Alejandro Kantor

##################################################################

makeUpvoteFacetGraph <- function(user.data, upvote.data, settings){  
  if( is.null(user.data) | is.null(upvote.data)){
    return(NULL)
  }
  ##################################################################
  # Merge in relevant dimension
  TOP<-paste0("parent_",settings$of.interest)
  BOT<-settings$of.interest
  parent.user.data<-user.data
  parent.user.data[,paste0("parent_",c("user_id",BOT))]<-parent.user.data[,c("user_id",BOT)]
  upvote.data<-merge(upvote.data,user.data[,c("user_id",BOT)],by="user_id",all.x=T)
  upvote.data<-merge(upvote.data,parent.user.data[,paste0("parent_",c("user_id",BOT))],by="parent_user_id",all.x=T)
  upvote.data$top<-upvote.data[,TOP]
  upvote.data$bot<-upvote.data[,BOT]
  
  ##################################################################
  exclusions<-(upvote.data$bot!="")&(upvote.data$top!="")
  exclusions<-exclusions&(!is.na(upvote.data$bot))&(!is.na(upvote.data$top))
  
  # upvotes_x$course.post<-1*(upvotes_x$thread_id%in%posts_x[(posts_x$course.post==1)&(posts_x$level==1),"thread_id"])
  #if(settings$course.only) exclusions<-exclusions&(upvote.data$course.post==1)
  #if(!settings$self.posts)  exclusions<-exclusions&(upvote.data$self.reply==0) # does this apply to up-votes?
  #if(settings$short.yes)   exclusions<-exclusions&(upvote.data$shortyes==0)
  if(settings$usa.only){   
    exclusions<-exclusions&(upvote.data$user_id%in%user.data[user.data$USA==1,"user_id"])
    exclusions<-exclusions&(upvote.data$parent_user_id%in%user.data[user.data$USA==1,"user_id"])
  }
  
  upvote.data<-upvote.data[exclusions,]
  
  ##################################################################
  plot.top <- max(table(upvote.data$top,upvote.data$bot))
  
  #xxx<-plot(table(upvote.data$top))
  #return(xxx)
  
  g_upvote_graph <- qplot(bot, data=upvote.data, geom="bar", fill=bot) +
    facet_wrap(~ top, strip.position = "bottom") +
    ylim(0,plot.top) +scale_fill_brewer( palette = "Set1")+
    labs(x="Original Poster Ideology",y="Upvote-Post Pairs", fill = "Upvoter Ideology") +
    scale_x_discrete(breaks=NULL) +
    #scale_fill_discrete(name="Upvoter Ideology") +
    getTheme()
  return(g_upvote_graph)
}
