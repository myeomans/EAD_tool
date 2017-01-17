##################################################################
# Comment facet graph
# Original Author: Mike Yeomans
# Modified by: Alejandro Kantor
##################################################################

# if more than 10
# use leftright
# not leftright_sc 
# and standarize ourselves for the user  call new measure ideology

makeActivityIdeologyGraph <- function(plot.data){
  count_col_names <-  c("leftright_sc","reply_count", "comment_count","upvote_count")
  df_plot <- plot.data[ ,count_col_names]
  num_row <- nrow(df_plot)
  df_plot$correlative <- 1:num_row
  df_plot <- melt(df_plot, id.vars = c("correlative","leftright_sc"))
  
  levels(df_plot$variable ) <- c("Replies", "Comments", "Upvotes")
  
  g_plot <- ggplot(df_plot, aes(x=leftright_sc, y=value, group =variable ))+
    facet_wrap(~ variable, ncol = 1 ,scales = "free") + 
    geom_point(alpha = 1/5, size = 3) + 
    #geom_boxplot() +
    xlab( "Partisan Ideology (Standarized)") +
    ylab( "Count") +
    theme(plot.title = element_text(hjust = 0.5, face="bold", size=20),
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
  
  return(g_plot)
}