##################################################################
# Activity vs. Ideology
# Original Author: Mike Yeomans
# Modified by: Alejandro Kantor
##################################################################

makeActivityIdeologyGraph <- function(user.data, post.data, upvote.data, settings){
  user.data<-user.data[!is.na(user.data[,settings$of.interest]),]
  # if(settings$usa.only){user.data<-user.data[user.data$USA==1,]}
  # if(settings$course.only){
  #   post.data<-post.data[post.data$course.post==1,]
  #   upvote.data<-upvote.data[upvote.data$course.post==1,]
  # }
  # if(!settings$self.posts){
  #   post.data<-post.data[post.data$self.reply==0,]
  # }
  
  df_plot <- user.data[ ,c("user_id",settings$of.interest)]
  df_plot$Replies<-unlist(sapply(1:nrow(df_plot),function(x) sum((post.data$user_id==df_plot[x,"user_id"])&(post.data$level==2))))
  df_plot$Comments<-unlist(sapply(1:nrow(df_plot),function(x) sum((post.data$user_id==df_plot[x,"user_id"])&(post.data$level>2))))
  df_plot$Upvotes<-unlist(sapply(1:nrow(df_plot),function(x) sum((upvote.data$user_id==df_plot[x,"user_id"]),na.rm=T)))
  df_plot$correlative <- 1:nrow(df_plot)
  df_plot[,c("user_id")]<-NULL
  df_plot <- melt(df_plot, id.vars = c("correlative",settings$of.interest))
  
  g_plot <- ggplot(df_plot, aes_string(x=settings$of.interest, y="value", group=settings$of.interest)) +
    facet_wrap(~ variable, ncol = 1 ,scales = "free") +
    theme( # remove the vertical grid lines, keep horizontal
      panel.grid.major.x = element_blank() ,
      panel.grid.major.y = element_line( size=.1, color="black" )
    )
  if(length(unique(df_plot[,settings$of.interest])) <= 5 ){
    g_plot <- g_plot + geom_boxplot(alpha = 0.2)
  } else{
    if(length(unique(df_plot[,settings$of.interest])) <= 12 ){
      g_plot <- g_plot + scale_x_continuous(breaks = unique(df_plot[,settings$of.interest]))
    }
    g_plot <- g_plot + geom_jitter(position=position_jitter(.02),alpha=0.2)
  }
  g_plot <- g_plot + xlab(settings$of.interest) +
    ylab( "Number of Actions per Person") +
    getTheme()
  
  return(g_plot)
}