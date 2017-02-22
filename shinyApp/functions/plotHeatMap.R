# ========================
# Author: 
#    concept: Brandon
#    code: Alejandro Kantor

plotHeatMap <- function(user.data, post.data, settings){
  if( is.null(user.data) | is.null(post.data)){
    return(NULL)
  }
  TOP<-paste0("parent_",settings$of.interest)
  BOT<-settings$of.interest
  parent.user.data<-user.data
  parent.user.data[,paste0("parent_",c("user_id",BOT))]<-parent.user.data[,c("user_id",BOT)]
  post.data<-merge(post.data,user.data[,c("user_id",BOT)],by="user_id",all.x=T)
  post.data<-merge(post.data,parent.user.data[,c("parent_user_id",TOP)],by="parent_user_id",all.x=T)
  
  post.data <- data.table(post.data)
  
  #-----
  #post.data <- post.data[responded_to!=username ]
  #-----
  v_s_cols <-  c(TOP, BOT)
  dt_freq <- post.data[ , .(freq = .N), keyby = v_s_cols]
  v_levels <- unique( c( post.data[[TOP ]] , 
                         post.data[[BOT ]]))
  v_levels <- v_levels[! is.na(v_levels)]
  dt_comb <- CJ(v_levels, v_levels)
  
  v_s_names <- c("user_ideology", "respond_ideology")
  setnames(dt_freq , v_s_cols, v_s_names)
  setnames(dt_comb  , v_s_names)
  dt_freq <- merge(dt_comb, dt_freq, by = v_s_names, all.x  = TRUE)
  dt_freq[ is.na(freq) , freq := 0]
  
  dt_freq[ , sum_user := sum(freq), by = "user_ideology"]
  dt_freq[ , sum_post := sum(freq), by = "respond_ideology"]
  dt_freq[ , expected := sum_post * sum_user/ sum(freq) ]
  
  dt_freq[ , difference := freq - expected  ]
  
  dt_freq <- round(dt_freq)
  
  dt_freq[ , respond_ideology := factor(respond_ideology, levels=10:1)]
  
  # original code incorrectly set x as response ideology but xlab as user ideology
  gg_out <- ggplot(dt_freq, aes(x = as.factor(user_ideology), as.factor(respond_ideology), fill = difference,   group=user_ideology)) +
    geom_tile() + 
    geom_text(aes( label = round(difference, 1))) +
    theme_bw() + 
    ylab("Response Poster Ideology") + xlab("Comment Reply Ideology") +
    scale_fill_distiller(palette = "Spectral")
    #scale_fill_gradient(low = "blue", high = "red") + 
    ggtitle("Observed - Expected Replies by Ideology")
  
  return(gg_out)
}

#plotHeatMap(posts_hks,"parent_leftright","leftright" )
