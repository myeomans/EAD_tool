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
  
  plot.top<-40
  legend.place<-array(c(0,0.02,1,0.98),c(2,2))
  
  
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
  
  
  g_upvote_graph <- qplot(TriPart, data=plot.data, geom="bar", fill=TriPart) +
    facet_wrap(~ parent_TriPart, strip.position = "bottom") + 
    ylim(0,plot.top) +
    labs(x="Original Poster Ideology",y="Upvote-Post Pairs") +
    scale_x_discrete(breaks=NULL) +
    scale_fill_discrete(name="Upvoter Ideology") +
    theme(legend.justification=legend.place[1,], legend.position=legend.place[2,], 
          plot.title = element_text(hjust = 0.5, face="bold", size=20),
          axis.title = element_text(face="bold", size=20),
          legend.title = element_text(face="bold", size=14),
          legend.text = element_text(size=15),
          legend.background = element_rect(fill="white", size=1, linetype="solid",colour="black"),
          legend.key.size = unit(0.5, "cm"),
          strip.text.x = element_text(size=15, face="bold"),
          panel.grid.major = element_line(colour = "grey10"),
          panel.grid.minor = element_line(colour = "white"),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill=NA, size=2),
          text = element_text(family="Times"))
  return(g_upvote_graph)
}
