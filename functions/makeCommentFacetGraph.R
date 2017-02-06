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
  
  
  ##################################################################
  plot.data<-plot.data[(plot.data$TriPart!="")&(plot.data$parent_TriPart!="")&(plot.data$self.reply==0),]
  plot.data<-plot.data[(plot.data$level==3)&(plot.data$course.post==1)&(plot.data$admin==0)&(plot.data$shortyes==0),]
  
  #table(plot.data$TriPart,plot.data$parent_TriPart,useNA="ifany")
  
  plot.data$TriPart<-factor(plot.data$TriPart,levels=c("Liberal","Moderate","Conservative"))
  plot.data$parent_TriPart<-factor(plot.data$parent_TriPart,levels=c("Liberal","Moderate","Conservative"))
  
  plot.top <- max(table(plot.data$TriPart,plot.data$parent_TriPart))
  
  g_plot <- qplot(TriPart, data=plot.data, geom="bar", fill=TriPart) +
    facet_wrap(~ parent_TriPart, strip.position = "bottom") + 
    ylim(0,plot.top) + scale_fill_brewer( palette = "Set1")+
    labs(x="Original Poster Ideology",y="Post-Comment Pairs", fill = "Commenter Ideology") +
    scale_x_discrete(breaks=NULL) +
    #scale_fill_discrete(name="Commenter Ideology") +
    getTheme()

  return(g_plot)
}
