##################################################################
# upvote facet graph
# Original Author: Mike Yeomans
# Modified by: Alejandro Kantor

##################################################################

makeUpvoteFacetGraph <- function(user.data, plot.data, settings){  
  
  plot.data$TriPart<-plot.data$upvote_TriPart
  plot.data$parent_TriPart<-plot.data$poster_TriPart
  ##################################################################
  exclusions<-(plot.data$TriPart!="")&(plot.data$parent_TriPart!="")&(!is.na(plot.data$TriPart))&(!is.na(plot.data$parent_TriPart))
  if(settings$course.only) exclusions<-exclusions&(plot.data$course.post==1)
  #if(!settings$self.posts)  exclusions<-exclusions&(plot.data$self.reply==0) # does this apply to up-votes?
  #if(settings$short.yes)   exclusions<-exclusions&(plot.data$shortyes==0)
    #table(plot.data$TriPart,plot.data$parent_TriPart,useNA="ifany")
  if(settings$usa.only){   
    exclusions<-exclusions&(plot.data$upvote_username%in%user.data[user.data$USA==1,"username"])
    exclusions<-exclusions&(plot.data$poster_username%in%user.data[user.data$USA==1,"username"])
  }
  
  plot.data$TriPart<-factor(plot.data$TriPart,levels=c("Liberal","Moderate","Conservative"))
  plot.data$parent_TriPart<-factor(plot.data$parent_TriPart,levels=c("Liberal","Moderate","Conservative"))
  plot.data<-plot.data[exclusions,]
  
  #   MOVE FROM INTAKE
  # #upvotes_x<-merge(upvotes_x,posts_x[,c("mongoid","thread_id","username")],by="mongoid", all.x=T)
  # #names(upvotes_x)[names(upvotes_x)=="username"]<-"poster_username"
  # upvotes_x[,c("poster_username","thread_id","poster_leftright","upvote_leftright")]<-NA
  # for (uv in 1:nrow(upvotes_x)){
  #   try(upvotes_x[uv,]$thread_id<-posts_x[posts_x$mongoid==upvotes_x[uv,]$mongoid,"thread_id"],silent=T)
  #   try(upvotes_x[uv,]$poster_username<-posts_x[posts_x$mongoid==upvotes_x[uv,]$mongoid,"username"], silent=T)
  #   try(upvotes_x[uv,]$poster_leftright<-as.numeric(unlist(users_x[users_x$username==upvotes_x[uv,]$poster_username,"leftright"]),silent=T))
  #   try(upvotes_x[uv,]$upvote_leftright<-as.numeric(unlist(users_x[users_x$username==upvotes_x[uv,]$upvote_username,"leftright"]),silent=T))
  # }
  # upvotes_x$course.post<-1*(upvotes_x$thread_id%in%posts_x[(posts_x$course.post==1)&(posts_x$level==1),"thread_id"])
  
  ##################################################################
  plot.top <- max(table(plot.data$TriPart,plot.data$parent_TriPart))
  
  g_upvote_graph <- qplot(TriPart, data=plot.data, geom="bar", fill=TriPart) +
    facet_wrap(~ parent_TriPart, strip.position = "bottom") + 
    ylim(0,plot.top) +scale_fill_brewer( palette = "Set1")+
    labs(x="Original Poster Ideology",y="Upvote-Post Pairs", fill = "Upvoter Ideology") +
    scale_x_discrete(breaks=NULL) +
    #scale_fill_discrete(name="Upvoter Ideology") + 
    getTheme() 
  return(g_upvote_graph)
}
