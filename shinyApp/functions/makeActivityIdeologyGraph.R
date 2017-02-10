##################################################################
# Comment facet graph
# Original Author: Mike Yeomans
# Modified by: Alejandro Kantor
##################################################################

makeActivityIdeologyGraph <- function(plot.data, settings){
  plot.data<-plot.data[!is.na(plot.data[,settings$of.interest]),]
  if(settings$usa.only){plot.data<-plot.data[plot.data$USA==1,]}
  count_col_names <-  c("leftright","reply_count", "comment_count","upvote_count")
  df_plot <- plot.data[ ,count_col_names]
  df_plot$ideology <- as.numeric(scale(df_plot[,settings$of.interest]))
  df_plot[,settings$of.interest] <- NULL
  i_num_ideology <- length(unique(df_plot$ideology))
  
  num_row <- nrow(df_plot)
  df_plot$correlative <- 1:num_row
  df_plot <- melt(df_plot, id.vars = c("correlative","ideology"))
  
  levels(df_plot$variable ) <- c("Replies", "Comments", "Upvotes")
  
  g_plot <- ggplot(df_plot, aes(x=ideology, y=value, group =ideology))+
    facet_wrap(~ variable, ncol = 1 ,scales = "free") 
  if( i_num_ideology <= 7 ){
    g_plot <- g_plot + geom_boxplot(alpha = 0.2)
  } else {
    g_plot <- g_plot + geom_jitter(position=position_jitter(.02),alpha=0.2)
  }
  g_plot <- g_plot + xlab( "Partisan Ideology (Standarized)") +
    ylab( "Count") +
    getTheme()
  
   return(g_plot)
}