############################################################
# Quick Summary Statistics
############################################################
pct_rounded<-function(x){
  return(round(100*mean(x, na.rm=T),1))
}
############################################################
table_row_build<-function(data){
  row.values<-list()
  row.values[["Observations"]]<-nrow(data)
  row.values[["% Male"]]<-pct_rounded(data$gender)
  row.values[["College Degree"]]<-pct_rounded(data$bachelors==1)
  row.values[["% from USA"]]<-pct_rounded(data$USA==1)
  row.values[["Median Age (SD)"]]<-paste0(median(data$age, na.rm=T)," (",round(sd(data$age, na.rm=T),1),")")
  row.values[["% Teachers"]]<-pct_rounded(data$teach==1)
  row.values[["Intended Course Completion"]]<-pct_rounded(data$intent)
  row.values[["Actual Course Completion"]]<-pct_rounded(data$certified)
  return(row.values)
}
############################################################
############################################################
demoTable <- function(user.data=NULL,enrol.data=NULL, settings){
  if(is.null(user.data)){ 
    return(NULL)
  } else {
    table_build<-list()
    if ((!is.null(enrol.data))){
      try(table_build[["Enrolled"]]<-table_row_build(enrol.data), silent=T)
    }
    user.data$included<-1*(!is.na(user.data[,settings$of.interest]))
    table_build[["Answered Survey"]]<-table_row_build(user.data[(user.data$included==1),])
    if(settings$usa.only){
      user.data$included<-1*(!is.na(user.data[,settings$of.interest]))&(user.data$USA==1)
      table_build[["From USA"]]<-table_row_build(user.data[(user.data$included==1),])
    }
    if (!is.null(user.data$count_post)){
      user.data$activities <- user.data$count_post

      table_build[["Active in Forum"]]<-table_row_build(user.data[(user.data$included==1)&(user.data$activities>0),])
    }
    output<-do.call(cbind, table_build)
    
    return(htmlTable(output,
                     header = paste0("&nbsp;&nbsp;&nbsp;",names(table_build),"&nbsp;&nbsp;&nbsp;"), 
                     n.tspanner = c(1,5,2), 
                     rnames = names(table_build[[1]]),
                     tspanner = c("","","")
    ))}
}