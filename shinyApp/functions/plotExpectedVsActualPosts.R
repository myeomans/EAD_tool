# ========================
# Author: 
#    concept: Brandon
#    code: Alejandro Kantor


plotExpectedVsActualPosts <- function( dt_user, dt_post, settings){
  if( is.null(dt_user) | is.null(dt_post)){
    return(NULL)
  }
  s_user_ideology<-settings$of.interest
  s_post_ideology<-paste0("parent_",settings$of.interest)
  dt_user <- data.table(dt_user)
  dt_post <- data.table(dt_post)
  dt_freq_user <- dt_user[ , .(freq_user = .N), keyby = s_user_ideology]
  dt_freq_post <- dt_post[ , .(freq_post = .N), keyby = s_post_ideology]
  
  setnames(dt_freq_user,s_user_ideology,"ideology")
  setnames(dt_freq_post, s_post_ideology,"ideology")
  
  #remove NA ideology
  dt_freq_user <- dt_freq_user[ !is.na(ideology)]
  dt_freq_post <- dt_freq_post[ !is.na(ideology)]
  # control that both data sets have the same ideology levels
  if( ! all(  dt_freq_post[, ideology ] %in% dt_freq_user[, ideology ] ) ) {
    stop( "At least one post ideological value not in user ideological value")
  }
  
  dt_freq <- merge(dt_freq_user, dt_freq_post, by = "ideology", all.x = TRUE )
  
  dt_freq[ is.na(freq_post), freq_post := 0]
  
  dt_freq[ , prop_user := freq_user/sum(freq_user)]
  dt_freq[ , expected_posts := prop_user * sum(freq_post)]
  
  gg_out <- ggplot(dt_freq, aes(x = expected_posts, y = freq_post, label = ideology)) + 
    geom_text() + theme_bw() + 
    geom_abline(slope =1 , intercept =0, alpha = 0.2,show.legend  = TRUE) + coord_fixed() +
    labs(x = "Expected" , y = "Actual")
  return(gg_out)
  
}
