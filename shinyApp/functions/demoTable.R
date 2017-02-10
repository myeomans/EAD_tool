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
  row.values[["% Female"]]<-pct_rounded(data$profile_gender=="f")
  row.values[["College Degree"]]<-pct_rounded(data$profile_bachelors==1)
  row.values[["% from USA"]]<-pct_rounded(data$USA==1)
  row.values[["Median Age (SD)"]]<-paste0(median(data$profile_age, na.rm=T)," (",round(sd(data$profile_age, na.rm=T),1),")")
  row.values[["% Teachers"]]<-pct_rounded(data$teacher==1)
  row.values[["Intended Course Completion"]]<-pct_rounded(grepl("enough",data$reason))#pct_rounded(data$intent),
  row.values[["Actual Course Completion"]]<-pct_rounded(data$certified)
  return(row.values)
}
############################################################
############################################################
demoTable <- function(user.data=NULL,enrol.data=NULL, pc.data=NULL, settings){
  if(is.null(user.data)){ 
    return(NULL)
  } else {
    table_build<-list()
    if ((!is.null(enrol.data))&!is.null(pc.data)){
      enrol.enter<-enrol_process(enrol.data,pc.data,settings$start.date)
      table_build[["Enrolled in Course"]]<-table_row_build(enrol.enter)
    }
    user.data$included<-1*(!is.na(user.data[,settings$of.interest]))
    table_build[["Answered WVS"]]<-table_row_build(user.data[(user.data$included==1),])
    if(settings$usa.only){
      user.data$included<-1*(!is.na(user.data[,settings$of.interest]))&(user.data$USA==1)
      table_build[["From USA"]]<-table_row_build(user.data[(user.data$included==1),])
    }
    table_build[["Active In Forum"]]<-table_row_build(user.data[(user.data$included==1)&(user.data$activities>0),])
    output<-do.call(cbind, table_build)
    
    return(htmlTable(output,
                     header = paste0("&nbsp;&nbsp;&nbsp;",names(table_build),"&nbsp;&nbsp;&nbsp;"), 
                     n.tspanner = c(1, 5 ,2), 
                     rnames = names(table_build[[1]]),
                     tspanner = c("","","")
    ))}
}

############################################################
enrol_process<-function(enrol.data,pc.data, start.date){
  enrol.enter<-merge(enrol.data,pc.data[,c("user_id","viewed")],by="user_id", all.x=T)
  enrol.enter<-enrol.enter[(enrol.enter$viewed=="True"),]
  enrol.enter$USA<-1*((enrol.enter$profile_country=="US"))
  enrol.enter$certified<-1*(enrol.enter$certificate_status=="downloadable")
  enrol.enter$profile_bachelors<-1*(enrol.enter$profile_level_of_education%in%c("b","a","m","p","p_oth","p_se"))
  enrol.enter$profile_age<-as.numeric(substr(start.date, 0, 4))-enrol.enter$profile_year_of_birth
  return(enrol.enter)
}