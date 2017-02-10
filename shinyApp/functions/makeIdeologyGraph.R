##################################################################
# Ideology graph
# Original Author: Mike Yeomans
# Modified by: Alejandro Kantor
##################################################################

makeIdeologyGraph <- function(plot.data, settings){
  plot.data<-plot.data[!is.na(plot.data[,settings$of.interest]),]
  if(settings$usa.only){plot.data<-plot.data[plot.data$USA==1,]}
  if(length(unique(plot.data[,settings$of.interest]))<5){
  num_row <- nrow(plot.data)
  plot.data$correlative <- 1:num_row
  plot.data <- melt(plot.data, id.vars = c("correlative","leftright"))
  
  g_plot <- qplot(plot.data[,settings$of.interest], geom="histogram")
                  
  g_plot <- g_plot + xlab( "Ideology Distribution") +
    ylab( "Count") +
    getTheme()
  }
  else{
    par(family="Times")
    g_plot<-plot(table(plot.data[,settings$of.interest]),lwd=10, xlab="Distribution of Interest", ylab="Number of Users")
  }
  return(g_plot)
}