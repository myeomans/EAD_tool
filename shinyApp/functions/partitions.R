##############################################################################################################################################################################################
# Tripartite
###############################################################################################
TriPartite<-function(ideology, bins=list(Conservative=6:10,
                                         Moderate=4:5,
                                         Liberal=1:3)){
  labels<-rep(NA,length(ideology))
  for(bb in 1:length(bins)){
    labels[ideology%in%bins[[bb]]]<-names(bins)[bb]
  }
  return(labels)
}
# posts_x$TriPart<-TriPartite(posts_x$leftright)
# posts_x$parent_TriPart<-TriPartite(posts_x$parent_leftright)
# upvotes_x$poster_TriPart<-TriPartite(upvotes_x$poster_leftright)
# upvotes_x$upvote_TriPart<-TriPartite(upvotes_x$upvote_leftright)
##############################################################################################################################################################################################