# ========================
# Author: 
#    concept: Brandon
#    code: Alejandro Kantor
plotHeatMap <- function(post.data, settings){
  if( is.null(post.data)){
    return(NULL)
  }
  s_of_interest <- settings$of.interest
  s_of_interest_parent <- paste0("parent_",s_of_interest)
  
  post.data$bot <- post.data[[s_of_interest]]
  post.data$top <- post.data[[s_of_interest_parent]]
  
  if(!is.factor(post.data$bot)){
    if(length(levels(as.factor(post.data$bot)))>7){
      buckets<-quantile(post.data$bot, probs=seq(0,1, by=0.25), na.rm=TRUE)
      
      post.data$bot<-cut(post.data$bot, breaks=buckets, include.lowest=TRUE, ordered_result=T,dig.lab = 2)
      post.data$top<-cut(post.data$top, breaks=buckets, include.lowest=TRUE, ordered_result=T,dig.lab = 2)
    }else{
      post.data$bot <- factor(post.data$bot)
      post.data$top <- factor(post.data$top)
    }
  } 
  s_f_levels <- levels(post.data$bot)
  
  post.data <- data.table(post.data)
  
  #-----
  #post.data <- post.data[responded_to!=username ]
  #-----
  v_s_cols <-  c("top", "bot")
  dt_freq <- post.data[ , .(freq = .N), keyby = v_s_cols]
  
  if( is.factor(post.data[["top" ]])){
    v_levels <- unique( c( as.character(post.data[["top" ]]) , 
                           as.character(post.data[["bot" ]])))
  } else {
    v_levels <- unique( c( post.data[["top" ]] , 
                           post.data[["bot" ]]))
  }

  v_levels <- v_levels[! is.na(v_levels)]
  dt_comb <- CJ(v_levels, v_levels)
  
  v_s_names <- c("user_ideology", "respond_ideology")
  if(exists("s_f_levels")){
    dt_comb[ , user_ideology := factor(user_ideology , levels = s_f_levels)]
  }
  
  setnames(dt_freq , v_s_cols, v_s_names)
  setnames(dt_comb  , v_s_names)
  dt_freq <- merge(dt_comb, dt_freq, by = v_s_names, all.x  = TRUE)
  dt_freq[ is.na(freq) , freq := 0]
  
  dt_freq[ , sum_user := sum(freq), by = "user_ideology"]
  dt_freq[ , sum_post := sum(freq), by = "respond_ideology"]
  dt_freq[ , expected := sum_post * sum_user/ sum(freq) ]
  
  dt_freq[ , difference := freq - expected  ]
  
  dt_freq[ , expected := round(expected)] 
  if( !is.factor(dt_freq[ , user_ideology])){
    dt_freq[ , user_ideology := factor(user_ideology, v_levels)] 
    dt_freq[ , respond_ideology := factor(respond_ideology, levels=rev(v_levels))]
  } else {
    dt_freq[ , respond_ideology := factor(respond_ideology, levels=s_f_levels)]
  }
  
  # original code incorrectly set x as response ideology but xlab as user ideology
  gg_out <- ggplot(dt_freq, aes(x =user_ideology, y = respond_ideology, fill = difference,   group=user_ideology)) +
    geom_tile() + 
    geom_text(aes( label = round(difference, 1))) +
    theme_bw() + 
    ylab("Response Poster Ideology") + xlab("Comment Reply Ideology") +
    scale_fill_distiller(palette = "Spectral")
    #scale_fill_gradient(low = "blue", high = "red") + 
    #ggtitle("Observed - Expected Replies by Ideology")
  
  return(gg_out)
}

#plotHeatMap(posts_hks,"parent_leftright","leftright" )
