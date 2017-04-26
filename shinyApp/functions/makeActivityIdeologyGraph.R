##################################################################
# Activity vs. Ideology
# Original Author: Mike Yeomans
# Modified by: Alejandro Kantor
##################################################################

makeActivityIdeologyGraph <- function(user.data,  settings){
  
  if( is.null(user.data)){
    return(NULL)
  }
  if( all(! c("count_replies", "count_comments", "count_upvotes") %in% names(user.data)) ){
    return(NULL)
  }
  s_col_interest <-  settings$of.interest
  user.data<-user.data[!is.na(user.data[,s_col_interest]),]
  
  df_plot <- user.data[ ,c("user_id",s_col_interest)]
  df_plot$Replies <- user.data$count_replies
  df_plot$Comments <- user.data$count_comments
  df_plot$Upvotes <- user.data$count_upvotes
  df_plot$correlative <- 1:nrow(df_plot)
  df_plot[,c("user_id")]<-NULL
  df_plot <- melt(df_plot, id.vars = c("correlative",s_col_interest))
  
  g_plot <- ggplot(df_plot, aes_string(x=settings$of.interest, y="value", group=s_col_interest)) +
    facet_wrap(~ variable, ncol = 1 ,scales = "free") +
    theme( 
      panel.grid.major.x = element_blank() ,
      panel.grid.major.y = element_line( size=.1, color="black" )
    )
  buckets<-unique(df_plot[,s_col_interest])
  if(length(buckets) <= 5 ){
    g_plot <- g_plot + geom_boxplot(alpha = 0.2)
  } else{
    if(length(buckets) <= 12 ){
      g_plot <- g_plot + scale_x_continuous(breaks = buckets)
    }
    g_plot <- g_plot + geom_jitter(position=position_jitter(.02),alpha=0.2)
  }
  g_plot <- g_plot + xlab(settings$of.interest) +
    ylab( "Number of Actions per Person") +
    getTheme()
  
  return(g_plot)
}
