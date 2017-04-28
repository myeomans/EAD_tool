# ######################################################################
# # Main function
# ######################################################################
makeWordGraph <- function(data, settings){
  if( is.null(data)){
    return(NULL)
  }else{
    s_of_interest <- settings$of.interest
    textRating <- data[,s_of_interest]
    if(!is.numeric(textRating)){
      textRating[textRating==""]<-NA
      textRating<-(1*(textRating==levels(textRating)[length(levels(textRating))])
                   -1*(textRating==levels(textRating)[1]))
    }
    textRating<-as.numeric(scale(as.numeric(textRating)))
    #textRating<- rnorm(nrow(data))
    textMatrix<- quanteda::dfm(data$body, stem=TRUE, remove=stopwords(),
                               removePunct=TRUE,removeSymbols=TRUE, removeNumbers=TRUE)
    textMatrix<-textMatrix[,colMeans(textMatrix, na.rm=T)>0.01]
    textMatrix<-group.max.conc(textMatrix, data$thread_id)
    text.results <- cmp.slor.mis(textMatrix, textRating)
    # Display mutual information plot
    grayline<- quantile(text.results$mis,.5)
    word.plot<-ggplot() +
      #geom_point(aes(x=slor, y=mis, color=slor), size1) +
      geom_text(aes(x=text.results$slor, y=text.results$mis, label=colnames(textMatrix)),
                alpha=ifelse(text.results$mis > grayline, 1, text.results$mis *(0.75/grayline)),
                color=ifelse(text.results$col, "firebrick", "navy"),
                size=4,
                nudge_y=0.00) +
      guides(fill=FALSE) +
      # scale_color_gradient2(low="dodgerblue", mid="gray97", high="firebrick", midpoint=0,
      #                       breaks=c(min(text.results$slor)+0.5, max(text.results$slor)-0.5),
      #                       labels=c("More liberal","More conservative")) +
      theme_bw() +
      labs(title="Partisanship of Word Usage",
           x="Variance-weighted log-odds ratio",
           y="Mutual Information",
           color="Partisanship") +
      theme(plot.title = element_text(hjust = 0.5, size=20, family="Times",face="bold"),
            axis.title = element_text(family="Times",face="bold", size=20), legend.position="none")
    return(word.plot)
  }
}
