##################################################################
# upvote facet graph
# Original Author: Mike Yeomans
# Modified by: Alejandro Kantor

##################################################################
# Groupings

#table(posts_gse$TriPart,posts_gse$parent_TriPart)
#table(posts_hks$TriPart,posts_hks$parent_TriPart)

##################################################################

makeUpvoteFacetGraph <- function( plot.data){  
  
  plot.data$TriPart<-plot.data$upvote_TriPart
  plot.data$parent_TriPart<-plot.data$poster_TriPart
  plot.data$self.reply<-0
  plot.data$level<-3
  plot.data$shortyes<-0
  ##################################################################
  plot.data<-plot.data[(plot.data$TriPart!="")&(plot.data$parent_TriPart!="")&(plot.data$self.reply==0),]
  plot.data<-plot.data[(plot.data$level==3)&(plot.data$course.post==1)&(plot.data$admin==0)&(plot.data$shortyes==0),]
  
  #table(plot.data$TriPart,plot.data$parent_TriPart,useNA="ifany")
  
  plot.data$TriPart<-factor(plot.data$TriPart,levels=c("Liberal","Moderate","Conservative"))
  plot.data$parent_TriPart<-factor(plot.data$parent_TriPart,levels=c("Liberal","Moderate","Conservative"))
  
  plot.top <- max(table(plot.data$TriPart,plot.data$parent_TriPart))
  
  g_upvote_graph <- qplot(TriPart, data=plot.data, geom="bar", fill=TriPart) +
    facet_wrap(~ parent_TriPart, strip.position = "bottom") + 
    ylim(0,plot.top) +
    labs(x="Original Poster Ideology",y="Upvote-Post Pairs") +
    scale_x_discrete(breaks=NULL) +
    scale_fill_discrete(name="Upvoter Ideology") +
    getTheme() 
  return(g_upvote_graph)
}
