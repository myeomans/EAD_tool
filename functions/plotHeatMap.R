# ========================
# Author: 
#    concept: Brandon
#    code: Alejandro Kantor

plotHeatMap <- function(dt_post, s_user_ideology = "parent_leftright", s_respond_ideology = "leftright"){
  dt_post <- data.table(dt_post)
  #-----
  #dt_post <- dt_post[responded_to!=username ]
  #-----
  v_s_cols <-  c(s_user_ideology, s_respond_ideology)
  dt_freq <- dt_post[ , .(freq = .N), keyby = v_s_cols]
  v_levels <- unique( c( dt_post[[s_user_ideology ]] , 
                         dt_post[[s_respond_ideology ]]))
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
