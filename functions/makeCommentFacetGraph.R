##################################################################
# Comment facet graph
# Original Author: Mike Yeomans
# Modified by: Alejandro Kantor

##################################################################
# Groupings

#table(posts_gse$TriPart,posts_gse$parent_TriPart)
#table(posts_hks$TriPart,posts_hks$parent_TriPart)

##################################################################


makeCommentFacetGraph <- function( plot.data ){  
  
  plot.top<-120
  legend.place<-array(c(0,0.02,1,0.98),c(2,2))
  
  ##################################################################
  plot.data<-plot.data[(plot.data$TriPart!="")&(plot.data$parent_TriPart!="")&(plot.data$self.reply==0),]
  plot.data<-plot.data[(plot.data$level==3)&(plot.data$course.post==1)&(plot.data$admin==0)&(plot.data$shortyes==0),]
  
  #table(plot.data$TriPart,plot.data$parent_TriPart,useNA="ifany")
  
  plot.data$TriPart<-factor(plot.data$TriPart,levels=c("Liberal","Moderate","Conservative"))
  plot.data$parent_TriPart<-factor(plot.data$parent_TriPart,levels=c("Liberal","Moderate","Conservative"))
  
  
  g_plot <- qplot(TriPart, data=plot.data, geom="bar", fill=TriPart) +
    facet_wrap(~ parent_TriPart, strip.position = "bottom") + 
    ylim(0,plot.top) +
    labs(x="Original Poster Ideology",y="Post-Comment Pairs") +
    scale_x_discrete(breaks=NULL) +
    scale_fill_discrete(name="Commenter Ideology") +
    getTheme()

  return(g_plot)
}
