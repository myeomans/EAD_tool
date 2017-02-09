##################################################################
# Ideology graph
# Original Author: Mike Yeomans
# Modified by: Alejandro Kantor
##################################################################


makeIdeologyGraph <- function(plot.data){
  if(length(unique(plot.data$leftright))<5){
  num_row <- nrow(plot.data)
  plot.data$correlative <- 1:num_row
  plot.data <- melt(plot.data, id.vars = c("correlative","leftright"))
  
  g_plot <- qplot(plot.data$leftright, geom="histogram")
                  
  g_plot <- g_plot + xlab( "Ideology Distribution") +
    ylab( "Count") +
    getTheme()
  }
  else{
    g_plot<-plot(table(plot.data$leftright),lwd=10, xlab="Distribution of Interest", ylab="Number of Users")
  }
  return(g_plot)
}