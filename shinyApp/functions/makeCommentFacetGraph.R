##################################################################
# Comment facet graph
# Original Author: Mike Yeomans
# Modified by: Alejandro Kantor

##################################################################
# Groupings

#table(posts_gse$TriPart,posts_gse$parent_TriPart)
#table(posts_hks$TriPart,posts_hks$parent_TriPart)
##################################################################


makeCommentFacetGraph <- function( post.data, settings){  
  if( is.null(post.data)){
    return(NULL)
  }
  ##################################################################
  # Merge in relevant dimension

  s_of_interest <- settings$of.interest
  s_of_interest_parent <- paste0("parent_",s_of_interest)
  
  post.data$bot <- post.data[[s_of_interest]]
  post.data$top <- post.data[[s_of_interest_parent]]
 
  if(!is.factor(post.data$bot)){
    post.data$bot <- factor(post.data$bot)
    post.data$top <- factor(post.data$top)
  }
   
  ##################################################################
  # # Filter posts
  valid_rows <- (post.data$bot!="") & (post.data$top!="") & (post.data$level>2)
  valid_rows <- valid_rows&(!is.na(post.data$bot)) & (!is.na(post.data$top))
  #if(settings$course.only) valid_rows <- valid_rows & (post.data$course.post==1)
  #if(!settings$self.posts)  valid_rows <- valid_rows & (post.data$self.reply==0)
  if(settings$usa.only){
    valid_rows<-valid_rows&(post.data$USA==1)
    valid_rows<-valid_rows&(post.data$parent_USA==1)
  }
  #if(settings$short.yes)   valid_rows<-valid_rows&(post.data$shortyes==0)

  post.data<-post.data[valid_rows,]
  ##################################################################
  
  g_plot <- qplot(bot, data=post.data, geom="bar", fill=bot) +
    facet_wrap(~ top, strip.position = "bottom") + 
    ylim(0,NA) + scale_fill_brewer( palette = "Set1")+
    labs(x="Original Poster Ideology",y="Post-Comment Pairs", fill = "Commenter Ideology") +
    scale_x_discrete(breaks=NULL) +
    #scale_fill_discrete(name="Commenter Ideology") +
    getTheme()
  
  return(g_plot)
}
