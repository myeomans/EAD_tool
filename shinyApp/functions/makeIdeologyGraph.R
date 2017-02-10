##################################################################
# Ideology graph
# Original Author: Mike Yeomans
# Modified by: Alejandro Kantor
##################################################################

makeIdeologyGraph <- function(user.data, post.data, settings){
  user.data<-user.data[!is.na(user.data[,settings$of.interest]),]
  if(settings$usa.only){user.data<-user.data[user.data$USA==1,]}
  if ((!is.null(post.data))&settings$forum.active){
    if(settings$course.only){
      post.data<-post.data[post.data$course.post==1,]
    }
    user.data<-user.data[unlist(sapply(1:nrow(user.data),function(x) sum((post.data$user_id==user.data[x,"user_id"]))))>0,]
  }
  if(length(unique(user.data[,settings$of.interest]))<12){
    
    g_plot <- ggplot(user.data, aes_string(x=settings$of.interest)) +
      scale_x_discrete(settings$of.interest) +
      geom_histogram(stat="count") + 
      xlab(settings$of.interest) + 
      theme(panel.grid.major = element_blank() )
  }
  else{
    g_plot <- ggplot(user.data, aes_string(x=settings$of.interest)) +
      geom_histogram(stat="bin", bins=20) + 
      xlab(settings$of.interest) +
      theme(panel.grid.major = element_blank())
    }
  
  return(g_plot)
}