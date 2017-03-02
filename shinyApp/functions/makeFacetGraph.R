##################################################################
# Comment facet graph
# Original Author: Mike Yeomans
# Modified by: Alejandro Kantor

##################################################################

makeFacetGraph <- function( data, s_data_origin, settings){  
  if( is.null(data)){
    return(NULL)
  }
  ##################################################################
  # Merge in relevant dimension
  
  s_of_interest <- settings$of.interest
  s_of_interest_parent <- paste0("parent_",s_of_interest)
  
  data$bot <- data[[s_of_interest]]
  data$top <- data[[s_of_interest_parent]]
  
  if(!is.factor(data$bot)){
    if(length(levels(as.factor(data$bot)))>7){
      
      
      data$bot<-cut(data$bot, breaks=quantile(data$bot, probs=seq(0,1, by=0.25), na.rm=TRUE), 
                    include.lowest=TRUE, ordered_result=T,dig.lab = 2)
      data$top<-cut(data$top, breaks=quantile(data$top, probs=seq(0,1, by=0.25), na.rm=TRUE), 
                    include.lowest=TRUE, ordered_result=T,dig.lab = 2)
    }else{
      data$bot <- factor(data$bot)
      data$top <- factor(data$top)
    }
  }
  
  ##################################################################
  # # Filter 
  ##################################################################
  ##################################################################
  # # Filter posts
  
  valid_rows <- (data$bot!="") & (data$top!="")
  if(tolower(s_data_origin )== "post"){
    valid_rows <- valid_rows & (data$level>2)
  }
  
  valid_rows <- valid_rows&(!is.na(data$bot)) & (!is.na(data$top))
  #if(settings$course.only) valid_rows <- valid_rows & (data$course.post==1)
  #if(!settings$self.posts)  valid_rows <- valid_rows & (data$self.reply==0)
  if(settings$usa.only){
    valid_rows<-valid_rows&(data$USA==1)
    valid_rows<-valid_rows&(data$parent_USA==1)
  }
  #if(settings$short.yes)   valid_rows<-valid_rows&(data$shortyes==0)
  
  data<-data[valid_rows,]
  
  if( tolower(s_data_origin )== "post"){
    s_xlab <- "Original Poster"
    s_ylab <- "Post-Comment"
    s_fill <- "Commenter"
  } else if( tolower(s_data_origin )== "upvote"){
    s_xlab <- "Original Poster"
    s_ylab <- "Upvote-Comment"
    s_fill <- "Upvoter"
  } else {
    stop("s_data_origin must be post or upvote")
  }
  
  g_plot <- qplot(bot, data=data, geom="bar", fill=bot) +
    facet_wrap(~ top, strip.position = "bottom") + 
    ylim(0,NA) + scale_fill_brewer( palette = "Set1")+
    labs(x=s_xlab,y=s_ylab, fill = s_fill) +
    scale_x_discrete(breaks=NULL) +
    #scale_fill_discrete(name="Commenter Ideology") +
    getTheme()
  
  return(g_plot)
}
